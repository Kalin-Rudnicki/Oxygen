package oxygen.cli

import oxygen.cli.error.*
import oxygen.predef.core.*
import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Parser
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait Parser[+A] {

  def optionalName: Option[Name]

  def helpMessage: HelpMessage

  def parse(values: List[Arg.ValueLike], params: List[Arg.ParamLike]): Parser.ParseResult[A]

  final def apply(args: List[String]): Parser.FinalParseResult[A] =
    Arg.parse(args) match {
      case Right((values, params)) => this.parse(values, params).toFinal
      case Left(error)             => Parser.FinalParseResult.Fail(ParseError.UnableToParseArgs(error), HelpMessage.RootMessage.Empty)
    }

  final def apply(args: String*): Parser.FinalParseResult[A] =
    apply(args.toList)

  // TODO (KR) : does it make sense to do this in different stages?
  /**
    * This should do 2 things:
    *   1) Fail if there are duplicate param names
    *   2) Convert Defaultable.Auto into Defaultable.Some/Defaultable.None based on start of long name
    */
  def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Parser[A])]

  def map[B](f: A => B): Parser[B] = Parser.Mapped(this, f)
  def mapOrFail[B](f: A => Either[String, B]): Parser[B] = Parser.MappedOrFail(this, f)

  final def <||[A2 >: A](that: Parser[A2]): Parser[A2] =
    Parser.merge(this, that)(
      Values.Or(_, _),
      Params.Or(_, _),
      Parser.Or(_, _),
    )

  final def <||>[B](that: Parser[B]): Parser[Either[A, B]] =
    Parser.merge(this, that)(
      (a, b) => Values.Or(a.map(_.asLeft), b.map(_.asRight)),
      (a, b) => Params.Or(a.map(_.asLeft), b.map(_.asRight)),
      (a, b) => Parser.Or(a.map(_.asLeft), b.map(_.asRight)),
    )

  final def ^>>[B](that: Parser[B])(implicit zip: Zip[A @uncheckedVariance, B]): Parser[zip.Out] =
    Parser.merge(this, that)(
      Values.Then(_, _, zip),
      Params.And(_, _, zip),
      (a, b) =>
        (a, b, zip) match {
          case (Parser.Empty, Parser.Empty, _: Zip.ZipUnitId[?]) => Parser.Empty.asInstanceOf[Parser[zip.Out]]
          case (Parser.Empty, Parser.Empty, _: Zip.ZipIdUnit[?]) => Parser.Empty.asInstanceOf[Parser[zip.Out]]
          case (a: Parser[A], Parser.Empty, _: Zip.ZipIdUnit[?]) => a.asInstanceOf[Parser[zip.Out]]
          case (Parser.Empty, b: Parser[B], _: Zip.ZipUnitId[?]) => b.asInstanceOf[Parser[zip.Out]]
          case (a, b, _)                                         => Parser.Then(a, b, zip)
        },
    )

  final def bracketed(name: LongName): Values[A] = Values.Bracketed(name, this, Nil)

  final def build: Either[BuildError, Parser[Either[HelpType, A]]] =
    for {
      (up, help2) <- Parser.help.buildInternal(Set.empty)
      (_, self2) <- this.buildInternal(up)
    } yield help2 <||> self2

}
object Parser {

  // =====|  |=====

  case object Empty extends Parser[Unit] {

    override def optionalName: Option[Name] = None

    override def helpMessage: HelpMessage = HelpMessage.RootMessage.Empty

    override def parse(values: List[Arg.ValueLike], params: List[Arg.ParamLike]): Parser.ParseResult[Unit] = Parser.ParseResult.Success((), Nil, values, params)

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Parser[Unit])] = (usedParams, this).asRight

  }

  final case class Then[A, B, O](
      left: Parser[A],
      right: Parser[B],
      zip: Zip.Out[A, B, O],
  ) extends Parser[O] {

    override def optionalName: Option[Name] = None

    override def helpMessage: HelpMessage = HelpMessage.RootMessage.And(left.helpMessage, right.helpMessage)

    override def parse(values: List[Arg.ValueLike], params: List[Arg.ParamLike]): Parser.ParseResult[O] =
      left.parse(values, params) match {
        case Parser.ParseResult.Success(value1, parsed1, remainingValues1, remainingParams1) =>
          right.parse(remainingValues1, remainingParams1) match {
            case ParseResult.Success(value2, parsed2, remainingValues2, remainingParams2) =>
              Parser.ParseResult.Success(zip.zip(value1, value2), parsed1 ::: parsed2, remainingValues2, remainingParams2)
            case fail @ ParseResult.Fail(_, _) => fail
          }
        case fail @ ParseResult.Fail(_, _) => fail
      }

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Parser[O])] =
      for {
        (up2, left2) <- left.buildInternal(usedParams)
        (up3, right2) <- right.buildInternal(up2)
      } yield (up3, Then(left2, right2, zip))

  }

  final case class Or[A](
      left: Parser[A],
      right: Parser[A],
  ) extends Parser[A] {

    override def optionalName: Option[Name] = None

    override def helpMessage: HelpMessage = HelpMessage.RootMessage.Or(left.helpMessage, right.helpMessage)

    override def parse(values: List[Arg.ValueLike], params: List[Arg.ParamLike]): Parser.ParseResult[A] =
      left.parse(values, params) match {
        case success @ Parser.ParseResult.Success(_, _, _, _) => success
        case Parser.ParseResult.Fail(error1, help1) =>
          right.parse(values, params) match {
            case success @ Parser.ParseResult.Success(_, _, _, _) => success
            case Parser.ParseResult.Fail(error2, help2)           => Parser.ParseResult.Fail(ParseError.RootOr(error1, error2), HelpMessage.RootMessage.Or(help1, help2))
          }
      }

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Parser[A])] =
      for {
        (up2, left2) <- left.buildInternal(usedParams)
        (up3, right2) <- right.buildInternal(usedParams)
      } yield (up2 | up3, Or(left2, right2))

    override def map[B](f: A => B): Parser[B] = Parser.Or(left.map(f), right.map(f))
    override def mapOrFail[B](f: A => Either[String, B]): Parser[B] = Parser.Or(left.mapOrFail(f), right.mapOrFail(f))

  }

  final case class Mapped[A, B](
      parser: Parser[A],
      f: A => B,
  ) extends Parser[B] {

    override def optionalName: Option[Name] = parser.optionalName

    override def helpMessage: HelpMessage = parser.helpMessage

    override def parse(values: List[Arg.ValueLike], params: List[Arg.ParamLike]): Parser.ParseResult[B] =
      parser.parse(values, params).map(f)

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Parser[B])] =
      parser.buildInternal(usedParams).map { case (up2, p) => (up2, Mapped(p, f)) }

    override def map[C](f: B => C): Parser[C] = Mapped(parser, this.f.andThen(f))
    override def mapOrFail[C](f: B => Either[String, C]): Parser[C] = MappedOrFail(parser, this.f.andThen(f))

  }

  final case class MappedOrFail[A, B](
      parser: Parser[A],
      f: A => Either[String, B],
  ) extends Parser[B] {

    override def optionalName: Option[Name] = parser.optionalName

    override def helpMessage: HelpMessage = parser.helpMessage

    override def parse(values: List[Arg.ValueLike], params: List[Arg.ParamLike]): Parser.ParseResult[B] =
      parser.parse(values, params).mapOrFail(this)(f)

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Parser[B])] =
      parser.buildInternal(usedParams).map { case (up2, p) => (up2, MappedOrFail(p, f)) }

    override def map[C](f: B => C): Parser[C] = MappedOrFail(parser, this.f(_).map(f))
    override def mapOrFail[C](f: B => Either[String, C]): Parser[C] = MappedOrFail(parser, this.f(_).flatMap(f))

  }

  // =====|  |=====

  sealed trait ParseResult[+A] {

    final def map[B](f: A => B): ParseResult[B] = this match
      case Parser.ParseResult.Success(value, parsed, remainingValues, remainingParams) => Parser.ParseResult.Success(f(value), parsed, remainingValues, remainingParams)
      case fail @ Parser.ParseResult.Fail(_, _)                                        => fail

    final def mapOrFail[B](parser: Parser[?])(f: A => Either[String, B]): ParseResult[B] = this match {
      case Parser.ParseResult.Success(value, parsed, remainingValues, remainingParams) =>
        f(value) match {
          case Right(value) => Parser.ParseResult.Success(value, parsed, remainingValues, remainingParams)
          case Left(error)  => Parser.ParseResult.Fail(ParseError.RootValidation(parsed, ParseError.FailedValidation(error)), parser.helpMessage.addHints(HelpHint.Error(error) :: Nil))
        }
      case fail @ Parser.ParseResult.Fail(_, _) => fail
    }

    final def toFinal: Parser.FinalParseResult[A] = this match
      case Parser.ParseResult.Success(value, parsed, Nil, Nil) => Parser.FinalParseResult.Success(value, parsed)
      case Parser.ParseResult.Success(_, _, upvH :: upvT, Nil) =>
        Parser.FinalParseResult.Fail(
          ParseError.UnparsedValues(NonEmptyList(upvH, upvT)),
          HelpMessage.ValueMessage.UnparsedArgs(NonEmptyList(upvH, upvT)),
        )
      case Parser.ParseResult.Success(_, _, Nil, uppH :: uppT) =>
        Parser.FinalParseResult.Fail(
          ParseError.UnparsedParams(NonEmptyList(uppH, uppT)),
          HelpMessage.ParamMessage.UnparsedArgs(NonEmptyList(uppH, uppT)),
        )
      case Parser.ParseResult.Success(_, _, upvH :: upvT, uppH :: uppT) =>
        Parser.FinalParseResult.Fail(
          ParseError.RootAnd(
            ParseError.UnparsedValues(NonEmptyList(upvH, upvT)),
            ParseError.UnparsedParams(NonEmptyList(uppH, uppT)),
          ),
          HelpMessage.RootMessage.And(
            HelpMessage.ValueMessage.UnparsedArgs(NonEmptyList(upvH, upvT)),
            HelpMessage.ParamMessage.UnparsedArgs(NonEmptyList(uppH, uppT)),
          ),
        )
      case Parser.ParseResult.Fail(error, help) => Parser.FinalParseResult.Fail(error, help)

  }
  object ParseResult {
    final case class Success[+A](value: A, parsed: List[ParsedArg], remainingValues: List[Arg.ValueLike], remainingParams: List[Arg.ParamLike]) extends ParseResult[A]
    final case class Fail(error: ParseError, help: HelpMessage) extends ParseResult[Nothing]
  }

  sealed trait FinalParseResult[+A] {

    final def toEither: Either[(ParseError, HelpMessage), A] = this match
      case FinalParseResult.Success(value, _) => value.asRight
      case FinalParseResult.Fail(error, help) => (error, help).asLeft

  }
  object FinalParseResult {
    final case class Success[+A](value: A, parsed: List[ParsedArg]) extends FinalParseResult[A]
    final case class Fail(error: ParseError, help: HelpMessage) extends FinalParseResult[Nothing]
  }

  // =====|  |=====

  private[cli] def validateNoDuplicates(usedParams: Set[SimpleName], myParams: Set[SimpleName]): Either[BuildError, Set[SimpleName]] = {
    val overlap = usedParams & myParams
    if (overlap.isEmpty) (usedParams | myParams).asRight
    else BuildError.DuplicateParams(overlap).asLeft
  }

  private[cli] def defaultAuto(usedParams: Set[SimpleName], long: LongName, short: Defaultable.Optional[ShortName]): (Set[SimpleName], Defaultable.Optional[ShortName]) =
    (short, long.firstChar.expand) match
      case (Defaultable.Auto, Right((_, lower))) if !usedParams.contains(lower) => (usedParams + lower, Defaultable.Some(lower))
      case (Defaultable.Auto, Right((upper, _))) if !usedParams.contains(upper) => (usedParams + upper, Defaultable.Some(upper))
      case (Defaultable.Auto, Left(digit)) if !usedParams.contains(digit)       => (usedParams + digit, Defaultable.Some(digit))
      case _                                                                    => (usedParams, short)

  private[cli] def defaultAuto(usedParams: Set[SimpleName], long: BooleanLongName, short: Defaultable.Optional[BooleanShortName]): (Set[SimpleName], Defaultable.Optional[BooleanShortName]) =
    (short, long.base.firstChar.expand) match
      case (Defaultable.Auto, Right((upper, lower))) if !usedParams.contains(lower) && !usedParams.contains(upper) => (usedParams + lower + upper, Defaultable.Some(BooleanShortName(upper, lower)))
      case _                                                                                                       => (usedParams, short)

  private def merge[A, B, C](a: Parser[A], b: Parser[B])(
      mergeValues: (Values[A], Values[B]) => Values[C],
      mergeParams: (Params[A], Params[B]) => Params[C],
      mergeRoot: (Parser[A], Parser[B]) => Parser[C],
  ): Parser[C] =
    (a, b) match
      case (a: Values[A], b: Values[B]) => mergeValues(a, b)
      case (a: Params[A], b: Params[B]) => mergeParams(a, b)
      case _                            => mergeRoot(a, b)

  // =====|  |=====

  val unit: Parser[Unit] = Parser.Empty

  private[cli] val help: Parser[HelpType] =
    Values.Ignored ^>>
      (
        Params.valueWith("help-extra", 'H', hints = "Show detailed help message" :: Nil)(Values.Ignored.map(_ => HelpType.HelpExtra)) <||
          Params.valueWith("help", 'h', hints = "Show help message" :: Nil)(Values.Ignored.map(_ => HelpType.Help))
      ) &&
      Params.Ignored

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Values
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait Values[+A] extends Parser[A] {

  def parseValues(values: List[Arg.ValueLike]): Values.ParseResult[A]

  override def helpMessage: HelpMessage.ValueMessage

  override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Values[A])]

  override final def parse(values: List[Arg.ValueLike], params: List[Arg.ParamLike]): Parser.ParseResult[A] = parseValues(values).toParserParseResult(params)

  final def ^>>[B](that: Values[B])(implicit zip: Zip[A @uncheckedVariance, B]): Values[zip.Out] = Values.Then(this, that, zip)
  final def <||[A2 >: A](that: Values[A2]): Values[A2] = Values.Or(this, that)
  final def <||>[B](that: Values[B]): Values[Either[A, B]] = Values.Or(this.map(_.asLeft), that.map(_.asRight))

  override def map[B](f: A => B): Values[B] = Values.Mapped(this, f)
  override def mapOrFail[B](f: A => Either[String, B]): Values[B] = Values.MappedOrFail(this, f)

  final def optional: Values[Option[A]] = Values.Optional(this, false)
  final def optional(breakOnAnyError: Boolean): Values[Option[A]] = Values.Optional(this, breakOnAnyError)

  final def repeated: Values[List[A]] = Values.Repeated(this, true)
  final def repeated(breakOnAnyError: Boolean): Values[List[A]] = Values.Repeated(this, breakOnAnyError)

  final def repeatedNel: Values[NonEmptyList[A]] = Values.RepeatedNel(this, true)
  final def repeatedNel(breakOnAnyError: Boolean): Values[NonEmptyList[A]] = Values.RepeatedNel(this, breakOnAnyError)

  final def withDefault[A2 >: A](default: A2): Values[A2] = withDefault(default, false, _.toString)
  final def withDefault[A2 >: A](default: A2, showDefault: A2 => String): Values[A2] = withDefault(default, false, showDefault)
  final def withDefault[A2 >: A](default: A2, breakOnAnyError: Boolean): Values[A2] = withDefault(default, breakOnAnyError, _.toString)
  final def withDefault[A2 >: A](default: A2, breakOnAnyError: Boolean, showDefault: A2 => String): Values[A2] = Values.WithDefault(this, default, showDefault(default), breakOnAnyError)

  final def withOptionalDefault[A2 >: A](default: Option[A2]): Values[A2] = default.fold(this)(this.withDefault(_))
  final def withOptionalDefault[A2 >: A](default: Option[A2], showDefault: A2 => String): Values[A2] = default.fold(this)(this.withDefault(_, showDefault))
  final def withOptionalDefault[A2 >: A](default: Option[A2], breakOnAnyError: Boolean): Values[A2] = default.fold(this)(this.withDefault(_, breakOnAnyError))
  final def withOptionalDefault[A2 >: A](default: Option[A2], breakOnAnyError: Boolean, showDefault: A2 => String): Values[A2] = default.fold(this)(this.withDefault(_, breakOnAnyError, showDefault))

}
object Values {

  // =====| Builders |=====

  def value[A: StringDecoder](
      name: LongName,
      hints: List[HelpHint.Make] = Nil,
  ): Values[A] =
    Values
      .SingleValue(
        name = name,
        hints = hints.map(HelpHint(_)),
      )
      .mapOrFail(StringDecoder[A].decode)

  // =====|  |=====

  final case class SingleValue(
      name: LongName,
      hints: List[HelpHint],
  ) extends Values[String] {

    override def optionalName: Option[Name] = name.some

    override def helpMessage: HelpMessage.ValueMessage = HelpMessage.ValueMessage.Value(name).addHints(hints)

    override def parseValues(values: List[Arg.ValueLike]): Values.ParseResult[String] =
      values match
        case (arg @ Arg.Value(_, value)) :: rest => Values.ParseResult.Success(value, ParsedValueArg(name :: Nil, arg :: Nil) :: Nil, rest)
        case (arg @ Arg.Bracketed(_, _, _)) :: _ =>
          Values.ParseResult.Fail(ParseError.ValueError(name, arg, ParseError.ExpectedValueArg), helpMessage.addHints(HelpHint.Error("Expected value arg") :: Nil))
        case Nil =>
          Values.ParseResult.Fail(ParseError.ValueError(name, ParseError.MissingRequiredValue), helpMessage.addHints(HelpHint.Error("Missing required value") :: Nil))

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Values[String])] =
      (usedParams, this).asRight

  }

  final case class Bracketed[A](
      name: LongName,
      parser: Parser[A],
      hints: List[HelpHint],
  ) extends Values[A] {

    override def optionalName: Option[Name] = name.some

    override def helpMessage: HelpMessage.ValueMessage = HelpMessage.ValueMessage.Bracketed(name, parser.helpMessage).addHints(hints)

    override def parseValues(values: List[Arg.ValueLike]): Values.ParseResult[A] =
      values match {
        case (arg @ Arg.Bracketed(_, values, params)) :: tail =>
          val parsedArg = ParsedValueArg(name :: Nil, arg :: Nil)
          parser.parse(values, params).toFinal match {
            case Parser.FinalParseResult.Success(value, _) => Values.ParseResult.Success(value, parsedArg :: Nil, tail)
            case Parser.FinalParseResult.Fail(error, help) =>
              Values.ParseResult.Fail(
                ParseError.SingleValueError(parsedArg :: Nil, ParseError.BracketedError(error)),
                HelpMessage.ValueMessage.Bracketed(name, help).addHints(hints),
              )
          }
        case (arg @ Arg.Value(_, _)) :: _ =>
          Values.ParseResult.Fail(ParseError.ValueError(name, arg, ParseError.ExpectedBracketedArg), helpMessage.addHints(HelpHint.Error("Expected bracketed arg") :: Nil))
        case Nil => Values.ParseResult.Fail(ParseError.ValueError(name, ParseError.MissingRequiredValue), helpMessage.addHints(HelpHint.Error("Missing required value") :: Nil))
      }

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Values[A])] =
      parser.buildInternal(Set.empty).map { case (_, p) => (usedParams, Bracketed(name, p, hints)) }

  }

  final case class Raw(
      name: LongName,
      hints: List[HelpHint],
  ) extends Values[Arg.ValueLike] {

    override def optionalName: Option[Name] = name.some

    override def helpMessage: HelpMessage.ValueMessage = HelpMessage.ValueMessage.Raw(name).addHints(hints)

    override def parseValues(values: List[Arg.ValueLike]): Values.ParseResult[Arg.ValueLike] =
      values match
        case h :: t => Values.ParseResult.Success(h, ParsedValueArg(name :: Nil, h :: Nil) :: Nil, t)
        case Nil    => Values.ParseResult.Fail(ParseError.ValueError(name, ParseError.MissingRequiredValue), helpMessage.addHints(HelpHint.Error("Missing required value") :: Nil))

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Values[Arg.ValueLike])] =
      (usedParams, this).asRight

  }

  case object Ignored extends Values[Unit] {

    override def optionalName: Option[Name] = None

    override def helpMessage: HelpMessage.ValueMessage = HelpMessage.ValueMessage.Empty

    override def parseValues(values: List[Arg.ValueLike]): Values.ParseResult[Unit] =
      Values.ParseResult.Success((), ParsedValueArg(Nil, values) :: Nil, Nil)

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Values[Unit])] =
      (usedParams, this).asRight

  }

  final case class Optional[A](
      parser: Values[A],
      breakOnAnyError: Boolean,
  ) extends Values[Option[A]] {

    override def optionalName: Option[Name] = parser.optionalName

    override def helpMessage: HelpMessage.ValueMessage = parser.helpMessage.addHints(HelpHint.Optional :: Nil)

    override def parseValues(values: List[Arg.ValueLike]): Values.ParseResult[Option[A]] =
      parser.parseValues(values) match
        case Values.ParseResult.Success(value, parsed, remaining)                                           => Values.ParseResult.Success(value.some, parsed, remaining)
        case Values.ParseResult.Fail(error, _) if breakOnAnyError || error.onlyContainsMissingRequiredValue => Values.ParseResult.Success(None, Nil, values)
        case fail @ Values.ParseResult.Fail(_, _)                                                           => fail

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Values[Option[A]])] =
      parser.buildInternal(usedParams).map { case (up2, p) => (up2, Optional(p, breakOnAnyError)) }

  }

  final case class Repeated[A](
      parser: Values[A],
      breakOnAnyError: Boolean,
  ) extends Values[List[A]] {

    override def optionalName: Option[Name] = parser.optionalName

    override def helpMessage: HelpMessage.ValueMessage = parser.helpMessage.addHints(HelpHint.Repeated :: Nil)

    @tailrec
    private def loop(
        values: List[Arg.ValueLike],
        parsed: List[ParsedValueArg],
        rStack: List[A],
    ): Values.ParseResult[List[A]] =
      parser.parseValues(values) match
        case Values.ParseResult.Success(value, parsed2, remaining) if parsed2.nonEmpty                      => loop(remaining, parsed ::: parsed2, value :: rStack)
        case Values.ParseResult.Fail(error, _) if breakOnAnyError || error.onlyContainsMissingRequiredValue => Values.ParseResult.Success(rStack.reverse, parsed, values)
        case fail @ Values.ParseResult.Fail(_, _)                                                           => fail
        case Values.ParseResult.Success(_, _, _)                                                            => Values.ParseResult.Success(rStack.reverse, parsed, values)

    override def parseValues(values: List[Arg.ValueLike]): Values.ParseResult[List[A]] =
      loop(values, Nil, Nil)

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Values[List[A]])] =
      parser.buildInternal(usedParams).map { case (up2, p) => (up2, Repeated(p, breakOnAnyError)) }

  }

  final case class RepeatedNel[A](
      parser: Values[A],
      breakOnAnyError: Boolean,
  ) extends Values[NonEmptyList[A]] {

    override def optionalName: Option[Name] = parser.optionalName

    override def helpMessage: HelpMessage.ValueMessage = parser.helpMessage.addHints(HelpHint.RepeatedNel :: Nil)

    @tailrec
    private def loop(
        values: List[Arg.ValueLike],
        parsed: List[ParsedValueArg],
        rStack: NonEmptyList[A],
    ): Values.ParseResult[NonEmptyList[A]] =
      parser.parseValues(values) match
        case Values.ParseResult.Success(value, parsed2, remaining) if parsed2.nonEmpty                      => loop(remaining, parsed ::: parsed2, value :: rStack)
        case Values.ParseResult.Fail(error, _) if breakOnAnyError || error.onlyContainsMissingRequiredValue => Values.ParseResult.Success(rStack.reverse, parsed, values)
        case fail @ Values.ParseResult.Fail(_, _)                                                           => fail
        case Values.ParseResult.Success(_, _, _)                                                            => Values.ParseResult.Success(rStack.reverse, parsed, values)

    override def parseValues(values: List[Arg.ValueLike]): Values.ParseResult[NonEmptyList[A]] =
      parser.parseValues(values) match
        case Values.ParseResult.Success(value, parsed, remaining) => loop(remaining, parsed, NonEmptyList.one(value))
        case fail @ Values.ParseResult.Fail(_, _)                 => fail

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Values[NonEmptyList[A]])] =
      parser.buildInternal(usedParams).map { case (up2, p) => (up2, RepeatedNel(p, breakOnAnyError)) }

  }

  final case class WithDefault[A](
      parser: Values[A],
      default: A,
      shownDefault: String,
      breakOnAnyError: Boolean,
  ) extends Values[A] {

    override def optionalName: Option[Name] = parser.optionalName

    override def helpMessage: HelpMessage.ValueMessage = parser.helpMessage.addHints(HelpHint.Default(shownDefault) :: Nil)

    override def parseValues(values: List[Arg.ValueLike]): Values.ParseResult[A] =
      parser.parseValues(values) match
        case Values.ParseResult.Success(value, parsed, remaining)                                           => Values.ParseResult.Success(value, parsed, remaining)
        case Values.ParseResult.Fail(error, _) if breakOnAnyError || error.onlyContainsMissingRequiredValue => Values.ParseResult.Success(default, Nil, values)
        case fail @ Values.ParseResult.Fail(_, _)                                                           => fail

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Values[A])] =
      parser.buildInternal(usedParams).map { case (up2, p) => (up2, WithDefault(p, default, shownDefault, breakOnAnyError)) }

  }

  final case class Mapped[A, B](
      parser: Values[A],
      f: A => B,
  ) extends Values[B] {

    override def optionalName: Option[Name] = parser.optionalName

    override def helpMessage: HelpMessage.ValueMessage = parser.helpMessage

    override def parseValues(values: List[Arg.ValueLike]): Values.ParseResult[B] =
      parser.parseValues(values).map(f)

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Values[B])] =
      parser.buildInternal(usedParams).map { case (up2, p) => (up2, Mapped(p, f)) }

  }

  final case class MappedOrFail[A, B](
      parser: Values[A],
      f: A => Either[String, B],
  ) extends Values[B] {

    override def optionalName: Option[Name] = parser.optionalName

    override def helpMessage: HelpMessage.ValueMessage = parser.helpMessage

    override def parseValues(values: List[Arg.ValueLike]): Values.ParseResult[B] =
      parser.parseValues(values).mapOrFail(this)(f)

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Values[B])] =
      parser.buildInternal(usedParams).map { case (up2, p) => (up2, MappedOrFail(p, f)) }

  }

  final case class Then[A, B, O](
      a: Values[A],
      b: Values[B],
      zip: Zip.Out[A, B, O],
  ) extends Values[O] {

    override def optionalName: Option[Name] = None

    override def helpMessage: HelpMessage.ValueMessage = HelpMessage.ValueMessage.Then(a.helpMessage, b.helpMessage)

    override def parseValues(values: List[Arg.ValueLike]): Values.ParseResult[O] =
      a.parseValues(values) match {
        case Values.ParseResult.Success(value1, parsed1, remaining1) =>
          b.parseValues(remaining1) match {
            case Values.ParseResult.Success(value2, parsed2, remaining2) => Values.ParseResult.Success(zip.zip(value1, value2), parsed1 ::: parsed2, remaining2)
            case fail @ Values.ParseResult.Fail(_, _)                    => fail
          }
        case fail @ Values.ParseResult.Fail(_, _) => fail
      }

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Values[O])] =
      for {
        (up2, a2) <- a.buildInternal(usedParams)
        (up3, b2) <- b.buildInternal(up2)
      } yield (up3, Then(a2, b2, zip))

  }

  final case class Or[A](
      left: Values[A],
      right: Values[A],
  ) extends Values[A] {

    override def optionalName: Option[Name] = None

    override def helpMessage: HelpMessage.ValueMessage = HelpMessage.ValueMessage.Or(left.helpMessage, right.helpMessage)

    override def parseValues(values: List[Arg.ValueLike]): Values.ParseResult[A] =
      left.parseValues(values) match {
        case success @ Values.ParseResult.Success(_, _, _) => success
        case Values.ParseResult.Fail(error1, help1) =>
          right.parseValues(values) match {
            case success @ Values.ParseResult.Success(_, _, _) => success
            case Values.ParseResult.Fail(error2, help2)        => Values.ParseResult.Fail(ParseError.ValueErrorOr(error1, error2), HelpMessage.ValueMessage.Or(help1, help2))
          }
      }

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Values[A])] =
      for {
        (up2, left2) <- left.buildInternal(usedParams)
        (up3, right2) <- right.buildInternal(usedParams)
      } yield (up2 | up3, Or(left2, right2))

    override def map[B](f: A => B): Values[B] = Values.Or(left.map(f), right.map(f))
    override def mapOrFail[B](f: A => Either[String, B]): Values[B] = Values.Or(left.mapOrFail(f), right.mapOrFail(f))

  }

  // =====|  |=====

  sealed trait ParseResult[+A] {

    final def map[B](f: A => B): ParseResult[B] = this match
      case ParseResult.Success(value, parsed, remaining) => ParseResult.Success(f(value), parsed, remaining)
      case fail @ ParseResult.Fail(_, _)                 => fail

    final def mapOrFail[B](parser: Values[?])(f: A => Either[String, B]): ParseResult[B] = this match {
      case ParseResult.Success(value, parsed, remaining) =>
        f(value) match {
          case Right(value) => ParseResult.Success(value, parsed, remaining)
          case Left(error)  => ParseResult.Fail(ParseError.SingleValueError(parsed, ParseError.FailedValidation(error)), parser.helpMessage.addHints(HelpHint.Error(error) :: Nil))
        }
      case fail @ ParseResult.Fail(_, _) => fail
    }

    final def toFinal: FinalParseResult[A] = this match
      case ParseResult.Success(value, parsed, Nil) => FinalParseResult.Success(value, parsed)
      case ParseResult.Success(_, _, h :: t)       => FinalParseResult.Fail(ParseError.UnparsedValues(NonEmptyList(h, t)), HelpMessage.ValueMessage.UnparsedArgs(NonEmptyList(h, t)))
      case ParseResult.Fail(error, help)           => FinalParseResult.Fail(error, help)

    final def toParserParseResult(params: List[Arg.ParamLike]): Parser.ParseResult[A] = this match
      case ParseResult.Success(value, parsed, remaining) => Parser.ParseResult.Success(value, parsed, remaining, params)
      case ParseResult.Fail(error, help)                 => Parser.ParseResult.Fail(error, help)

  }
  object ParseResult {
    final case class Success[+A](value: A, parsed: List[ParsedValueArg], remaining: List[Arg.ValueLike]) extends ParseResult[A]
    final case class Fail(error: ParseError.ValueError, help: HelpMessage.ValueMessage) extends ParseResult[Nothing]
  }

  sealed trait FinalParseResult[+A]
  object FinalParseResult {
    final case class Success[+A](value: A, parsed: List[ParsedValueArg]) extends FinalParseResult[A]
    final case class Fail(error: ParseError.ValueError | ParseError.UnparsedValues, help: HelpMessage.ValueMessage) extends FinalParseResult[Nothing]
  }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Params
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait Params[+A] extends Parser[A] {

  def parseParams(params: List[Arg.ParamLike]): Params.ParseResult[A]

  override def helpMessage: HelpMessage.ParamMessage

  override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Params[A])]

  override final def parse(values: List[Arg.ValueLike], params: List[Arg.ParamLike]): Parser.ParseResult[A] = parseParams(params).toParserParseResult(values)

  override def map[B](f: A => B): Params[B] = Params.Mapped(this, f)
  override def mapOrFail[B](f: A => Either[String, B]): Params[B] = Params.MappedOrFail(this, f)

  // ---  ---

  final def optional: Params[Option[A]] = Params.Optional(this)
  final def repeated: Params[List[A]] = Params.Repeated(this)
  final def repeatedNel: Params[NonEmptyList[A]] = Params.RepeatedNel(this)

  final def withDefault[A2 >: A](default: A2, showDefault: A2 => String = (_: A2).toString): Params[A2] = Params.WithDefault(this, default, showDefault(default))
  final def withOptionalDefault[A2 >: A](default: Option[A2], showDefault: A2 => String = (_: A2).toString): Params[A2] = default.fold(this)(this.withDefault(_, showDefault))

  /**
    * @see [[Params.And]].
    */
  final def &&[B](that: Params[B])(implicit zip: Zip[A @uncheckedVariance, B]): Params[zip.Out] = Params.And(this, that, zip)

  /**
    * @see [[Params.Or]].
    */
  final def <||[A2 >: A](that: Params[A2]): Params[A2] = Params.Or(this, that)

  /**
    * @see [[Params.Or]].
    */
  final def <||>[B](that: Params[B]): Params[Either[A, B]] = Params.Or(this.map(_.asLeft), that.map(_.asRight))

  /**
    * @see [[Params.FirstOfByArgIndex]].
    */
  final def ||[A2 >: A](that: Params[A2]): Params[A2] = (this, that) match
    case (Params.FirstOfByArgIndex(options1), Params.FirstOfByArgIndex(options2)) => Params.FirstOfByArgIndex(options1 ::: options2)
    case (Params.FirstOfByArgIndex(options1), _)                                  => Params.FirstOfByArgIndex(options1 :+ that)
    case (_, Params.FirstOfByArgIndex(options2))                                  => Params.FirstOfByArgIndex(this +: options2)
    case (_, _)                                                                   => Params.FirstOfByArgIndex(NonEmptyList.of(this, that))

}
object Params {

  // =====| Builders |=====

  def value[A: StringDecoder](
      longName: LongName,
      shortName: Defaultable.Optional[ShortName] = Defaultable.Auto,
      aliases: List[SimpleName] = Nil,
      hints: List[HelpHint.Make] = Nil,
  ): Params[A] =
    Params.ParamWithValues(
      longName = longName,
      shortName = shortName,
      aliases = aliases,
      hints = hints.map(HelpHint(_)),
      valueParser = Values.value[A](longName),
    )

  def `enum`[A](
      longName: LongName,
      shortName: Defaultable.Optional[ShortName] = Defaultable.Auto,
      aliases: List[SimpleName] = Nil,
      hints: List[HelpHint.Make] = Nil,
  )(implicit
      ec: Enum.Companion[A],
  ): Params[A] = {
    implicit val aDec: StringDecoder[A] = ec.companion.stringCodec.decoder

    Params.ParamWithValues(
      longName = longName,
      shortName = shortName,
      aliases = aliases,
      hints = hints.map(HelpHint(_)) :+ HelpHint.EnumValues(ec.companion.ToString.encodedValues),
      valueParser = Values.value[A](longName),
    )
  }

  def valueWith[A](
      longName: LongName,
      shortName: Defaultable.Optional[ShortName] = Defaultable.Auto,
      aliases: List[SimpleName] = Nil,
      hints: List[HelpHint.Make] = Nil,
  )(valueParser: Values[A]): Params[A] =
    Params.ParamWithValues(
      longName = longName,
      shortName = shortName,
      aliases = aliases,
      hints = hints.map(HelpHint(_)),
      valueParser = valueParser,
    )

  def ifPresent[A](
      longName: LongName,
      value: A,
      shortName: Defaultable.Optional[ShortName] = Defaultable.Auto,
      aliases: List[SimpleName] = Nil,
      hints: List[HelpHint.Make] = Nil,
  ): Params[A] =
    Params.IfPresent(
      longName = longName,
      shortName = shortName,
      aliases = aliases,
      hints = hints.map(HelpHint(_)),
      value = value,
    )

  def flag(
      longName: LongName,
      value: Boolean = true,
      shortName: Defaultable.Optional[ShortName] = Defaultable.Auto,
      aliases: List[SimpleName] = Nil,
      hints: List[HelpHint.Make] = Nil,
  ): Params[Boolean] =
    Params
      .IfPresent(
        longName = longName,
        shortName = shortName,
        aliases = aliases,
        hints = hints.map(HelpHint(_)),
        value = (),
      )
      .optional
      .map {
        case Some(_) => value
        case None    => !value
      }

  object toggle {

    def apply(
        longName: BooleanLongName,
        shortName: Defaultable.Optional[BooleanShortName] = Defaultable.Auto,
        aliases: List[BooleanName] = Nil,
        hints: List[HelpHint.Make] = Nil,
    ): Params[Boolean] =
      Params.BooleanToggle(
        longName = longName,
        shortName = shortName,
        aliases = aliases,
        hints = hints.map(HelpHint(_)),
      )

    def prefixTrue(
        truePrefix: LongName,
        baseName: LongName,
        shortName: Defaultable.Optional[BooleanShortName] = Defaultable.Auto,
        aliases: List[BooleanName] = Nil,
        hints: List[HelpHint.Make] = Nil,
    ): Params[Boolean] =
      Params.BooleanToggle(
        longName = BooleanLongName.PrefixTrue(truePrefix, baseName),
        shortName = shortName,
        aliases = aliases,
        hints = hints.map(HelpHint(_)),
      )

    def prefixFalse(
        falsePrefix: LongName,
        baseName: LongName,
        shortName: Defaultable.Optional[BooleanShortName] = Defaultable.Auto,
        aliases: List[BooleanName] = Nil,
        hints: List[HelpHint.Make] = Nil,
    ): Params[Boolean] =
      Params.BooleanToggle(
        longName = BooleanLongName.PrefixFalse(falsePrefix, baseName),
        shortName = shortName,
        aliases = aliases,
        hints = hints.map(HelpHint(_)),
      )

    def prefixBoth(
        truePrefix: LongName,
        falsePrefix: LongName,
        baseName: LongName,
        shortName: Defaultable.Optional[BooleanShortName] = Defaultable.Auto,
        aliases: List[BooleanName] = Nil,
        hints: List[HelpHint.Make] = Nil,
    ): Params[Boolean] =
      Params.BooleanToggle(
        longName = BooleanLongName.PrefixBoth(truePrefix, falsePrefix, baseName),
        shortName = shortName,
        aliases = aliases,
        hints = hints.map(HelpHint(_)),
      )

  }

  /**
    * @see [[Params.FirstOfByArgIndex]].
    */
  def firstOf[A](
      parser0: Params[A],
      parser1: Params[A],
      parserN: Params[A]*,
  ): Params[A] =
    Params.FirstOfByArgIndex(NonEmptyList(parser0, parser1 :: parserN.toList))

  // =====|  |=====

  final case class Const[A](value: A) extends Params[A] {

    override def optionalName: Option[Name] = None

    override def helpMessage: HelpMessage.ParamMessage = HelpMessage.ParamMessage.Empty

    override def parseParams(params: List[Arg.ParamLike]): ParseResult[A] =
      ParseResult.Success(value, Nil, params)

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Params[A])] =
      (usedParams, this).asRight

  }

  final case class ParamWithValues[A](
      longName: LongName,
      shortName: Defaultable.Optional[ShortName],
      aliases: List[SimpleName],
      hints: List[HelpHint],
      valueParser: Values[A],
  ) extends Params[A] {

    private val names: Set[SimpleName] = List(longName :: shortName.toOption.toList, aliases).flatten.toSet

    override def optionalName: Option[Name] = longName.some

    override def helpMessage: HelpMessage.ParamMessage = HelpMessage.ParamMessage.Param(longName, shortName.toOption, aliases, valueParser.helpMessage).addHints(hints)

    override def parseParams(params: List[Arg.ParamLike]): Params.ParseResult[A] =
      findParam(names, params) match {
        case Some((param, rest)) =>
          val parsedArg = ParsedParamArg(longName :: Nil, param :: Nil)
          valueParser.parseValues(param.values).toFinal match {
            case Values.FinalParseResult.Success(value, _) => Params.ParseResult.Success(value, parsedArg :: Nil, rest)
            case Values.FinalParseResult.Fail(error, help) =>
              Params.ParseResult.Fail(
                ParseError.SingleParamError(parsedArg :: Nil, ParseError.ParamValuesValidation(error)),
                HelpMessage.ParamMessage.Param(longName, shortName.toOption, aliases, help).addHints(hints),
              )
          }
        case None =>
          Params.ParseResult.Fail(ParseError.ParamError(longName, ParseError.MissingRequiredParam), helpMessage.addHints(HelpHint.Error("Missing required param") :: Nil))
      }

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Params[A])] =
      for {
        up2 <- Parser.validateNoDuplicates(usedParams, names)
        (_, valueParser2) <- valueParser.buildInternal(Set.empty)
        (up3, shortName2) = Parser.defaultAuto(up2, longName, shortName)
      } yield (
        up3,
        ParamWithValues(
          longName = longName,
          shortName = shortName2,
          aliases = aliases,
          hints = hints,
          valueParser = valueParser2,
        ),
      )

    override def map[B](f: A => B): Params.ParamWithValues[B] = copy(valueParser = valueParser.map(f))

    override def mapOrFail[B](f: A => Either[String, B]): Params.ParamWithValues[B] = copy(valueParser = valueParser.mapOrFail(f))

  }

  final case class IfPresent[A](
      longName: LongName,
      shortName: Defaultable.Optional[ShortName],
      aliases: List[SimpleName],
      hints: List[HelpHint],
      value: A,
  ) extends Params[A] {

    private val names: Set[SimpleName] = List(longName :: shortName.toOption.toList, aliases).flatten.toSet

    override def optionalName: Option[Name] = longName.some

    override def helpMessage: HelpMessage.ParamMessage = HelpMessage.ParamMessage.Param(longName, shortName.toOption, aliases, HelpMessage.ValueMessage.Empty).addHints(hints)

    override def parseParams(params: List[Arg.ParamLike]): ParseResult[A] =
      findParam(names, params) match {
        case Some((param, rest)) =>
          val parsedArg = ParsedParamArg(longName :: Nil, param :: Nil)
          param.values match {
            case Nil => Params.ParseResult.Success(value, parsedArg :: Nil, rest)
            case h :: t =>
              Params.ParseResult.Fail(
                ParseError.SingleParamError(parsedArg :: Nil, ParseError.ParamValuesValidation(ParseError.UnparsedValues(NonEmptyList(h, t)))),
                HelpMessage.ParamMessage.Param(longName, shortName.toOption, aliases, HelpMessage.ValueMessage.UnparsedArgs(NonEmptyList(h, t))).addHints(hints),
              )
          }
        case None =>
          Params.ParseResult.Fail(ParseError.ParamError(longName, ParseError.MissingRequiredParam), helpMessage.addHints(HelpHint.Error("Missing required param") :: Nil))
      }

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Params[A])] =
      for {
        up2 <- Parser.validateNoDuplicates(usedParams, names)
        (up3, shortName2) = Parser.defaultAuto(up2, longName, shortName)
      } yield (
        up3,
        IfPresent(
          longName = longName,
          shortName = shortName2,
          aliases = aliases,
          hints = hints,
          value = value,
        ),
      )

  }

  final case class BooleanToggle(
      longName: BooleanLongName,
      shortName: Defaultable.Optional[BooleanShortName],
      aliases: List[BooleanName],
      hints: List[HelpHint],
  ) extends Params[Boolean] {

    private val names: Map[SimpleName, Boolean] = List(longName :: Nil, shortName.toOption.toList).flatten.flatMap { n => List(n.trueName -> true, n.falseName -> false) }.toMap

    override def optionalName: Option[Name] = longName.some

    override def helpMessage: HelpMessage.ParamMessage = HelpMessage.ParamMessage.Param(longName, shortName.toOption, aliases, HelpMessage.ValueMessage.Empty).addHints(hints)

    override def parseParams(params: List[Arg.ParamLike]): ParseResult[Boolean] =
      findParamTagged(names, params) match {
        case Some((param, value, rest)) =>
          val parsedArg = ParsedParamArg(longName :: Nil, param :: Nil)
          param.values match {
            case Nil => Params.ParseResult.Success(value, parsedArg :: Nil, rest)
            case h :: t =>
              Params.ParseResult.Fail(
                ParseError.SingleParamError(parsedArg :: Nil, ParseError.ParamValuesValidation(ParseError.UnparsedValues(NonEmptyList(h, t)))),
                HelpMessage.ParamMessage.Param(longName, shortName.toOption, aliases, HelpMessage.ValueMessage.UnparsedArgs(NonEmptyList(h, t))).addHints(hints),
              )
          }
        case None =>
          Params.ParseResult.Fail(ParseError.ParamError(longName, ParseError.MissingRequiredParam), helpMessage.addHints(HelpHint.Error("Missing required param") :: Nil))
      }

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Params[Boolean])] =
      for {
        up2 <- Parser.validateNoDuplicates(usedParams, names.keySet)
        (up3, shortName2) = Parser.defaultAuto(up2, longName, shortName)
      } yield (
        up3,
        BooleanToggle(
          longName = longName,
          shortName = shortName2,
          aliases = aliases,
          hints = hints,
        ),
      )

  }

  final case class Raw(
      name: LongName,
      hints: List[HelpHint],
  ) extends Params[Arg.ParamLike] {

    override def optionalName: Option[Name] = name.some

    override def helpMessage: HelpMessage.ParamMessage = HelpMessage.ParamMessage.Raw(name).addHints(hints)

    override def parseParams(params: List[Arg.ParamLike]): Params.ParseResult[Arg.ParamLike] =
      params match
        case h :: t => Params.ParseResult.Success(h, ParsedParamArg(name :: Nil, h :: Nil) :: Nil, t)
        case Nil    => Params.ParseResult.Fail(ParseError.ParamError(name, ParseError.MissingRequiredParam), helpMessage.addHints(HelpHint.Error("Missing required param") :: Nil))

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Params[Arg.ParamLike])] =
      (usedParams, this).asRight

  }

  case object Ignored extends Params[Unit] {

    override def optionalName: Option[Name] = None

    override def helpMessage: HelpMessage.ParamMessage = HelpMessage.ParamMessage.Empty

    override def parseParams(params: List[Arg.ParamLike]): Params.ParseResult[Unit] =
      Params.ParseResult.Success((), ParsedParamArg(Nil, params) :: Nil, Nil)

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Params[Unit])] =
      (usedParams, this).asRight

  }

  final case class Optional[A](
      parser: Params[A],
  ) extends Params[Option[A]] {

    override def optionalName: Option[Name] = parser.optionalName

    override def helpMessage: HelpMessage.ParamMessage = parser.helpMessage.addHints(HelpHint.Repeated :: Nil)

    override def parseParams(params: List[Arg.ParamLike]): Params.ParseResult[Option[A]] = parser.parseParams(params) match
      case Params.ParseResult.Success(value, parsed, remaining)                        => Params.ParseResult.Success(value.some, parsed, remaining)
      case Params.ParseResult.Fail(error, _) if error.onlyContainsMissingRequiredParam => Params.ParseResult.Success(None, Nil, params)
      case fail @ Params.ParseResult.Fail(_, _)                                        => fail

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Params[Option[A]])] =
      parser.buildInternal(usedParams).map { case (up2, p) => (up2, Optional(p)) }

  }

  final case class Repeated[A](
      parser: Params[A],
  ) extends Params[List[A]] {

    override def optionalName: Option[Name] = parser.optionalName

    override def helpMessage: HelpMessage.ParamMessage = parser.helpMessage.addHints(HelpHint.Repeated :: Nil)

    @tailrec
    private def loop(
        params: List[Arg.ParamLike],
        parsed: List[ParsedParamArg],
        rStack: List[A],
    ): Params.ParseResult[List[A]] =
      parser.parseParams(params) match
        case Params.ParseResult.Success(value, parsed2, remaining) if parsed2.nonEmpty   => loop(remaining, parsed ::: parsed2, value :: rStack)
        case Params.ParseResult.Fail(error, _) if error.onlyContainsMissingRequiredParam => Params.ParseResult.Success(rStack.reverse, parsed, params)
        case fail @ Params.ParseResult.Fail(_, _)                                        => fail
        case Params.ParseResult.Success(_, _, _)                                         => Params.ParseResult.Success(rStack.reverse, parsed, params)

    override def parseParams(params: List[Arg.ParamLike]): Params.ParseResult[List[A]] =
      loop(params, Nil, Nil)

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Params[List[A]])] =
      parser.buildInternal(usedParams).map { case (up2, p) => (up2, Repeated(p)) }

  }

  final case class RepeatedNel[A](
      parser: Params[A],
  ) extends Params[NonEmptyList[A]] {

    override def optionalName: Option[Name] = parser.optionalName

    override def helpMessage: HelpMessage.ParamMessage = parser.helpMessage.addHints(HelpHint.RepeatedNel :: Nil)

    @tailrec
    private def loop(
        params: List[Arg.ParamLike],
        parsed: List[ParsedParamArg],
        rStack: NonEmptyList[A],
    ): Params.ParseResult[NonEmptyList[A]] =
      parser.parseParams(params) match
        case Params.ParseResult.Success(value, parsed2, remaining) if parsed2.nonEmpty   => loop(remaining, parsed ::: parsed2, value :: rStack)
        case Params.ParseResult.Fail(error, _) if error.onlyContainsMissingRequiredParam => Params.ParseResult.Success(rStack.reverse, parsed, params)
        case fail @ Params.ParseResult.Fail(_, _)                                        => fail
        case Params.ParseResult.Success(_, _, _)                                         => Params.ParseResult.Success(rStack.reverse, parsed, params)

    override def parseParams(params: List[Arg.ParamLike]): Params.ParseResult[NonEmptyList[A]] =
      parser.parseParams(params) match
        case Params.ParseResult.Success(value, parsed, params) => loop(params, parsed, NonEmptyList.one(value))
        case fail @ Params.ParseResult.Fail(_, _)              => fail

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Params[NonEmptyList[A]])] =
      parser.buildInternal(usedParams).map { case (up2, p) => (up2, RepeatedNel(p)) }

  }

  final case class WithDefault[A](
      parser: Params[A],
      default: A,
      shownDefault: String,
  ) extends Params[A] {

    override def optionalName: Option[Name] = parser.optionalName

    override def helpMessage: HelpMessage.ParamMessage = parser.helpMessage.addHints(HelpHint.Default(shownDefault) :: Nil)

    override def parseParams(params: List[Arg.ParamLike]): Params.ParseResult[A] =
      parser.parseParams(params) match
        case success @ Params.ParseResult.Success(_, _, _)                               => success
        case Params.ParseResult.Fail(error, _) if error.onlyContainsMissingRequiredParam => Params.ParseResult.Success(default, Nil, params)
        case fail @ Params.ParseResult.Fail(_, _)                                        => fail

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Params[A])] =
      parser.buildInternal(usedParams).map { case (up2, p) => (up2, WithDefault(p, default, shownDefault)) }

    override def map[B](f: A => B): Params[B] = Params.WithDefault(parser.map(f), f(default), shownDefault)

  }

  final case class Mapped[A, B](
      parser: Params[A],
      f: A => B,
  ) extends Params[B] {

    override def optionalName: Option[Name] = parser.optionalName

    override def helpMessage: HelpMessage.ParamMessage = parser.helpMessage

    override def parseParams(params: List[Arg.ParamLike]): Params.ParseResult[B] =
      parser.parseParams(params).map(f)

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Params[B])] =
      parser.buildInternal(usedParams).map { case (up2, p) => (up2, Mapped(p, f)) }

    override def map[C](f: B => C): Params[C] = Params.Mapped(parser, this.f.andThen(f))
    override def mapOrFail[C](f: B => Either[String, C]): Params[C] = Params.MappedOrFail(parser, this.f.andThen(f))

  }

  final case class MappedOrFail[A, B](
      parser: Params[A],
      f: A => Either[String, B],
  ) extends Params[B] {

    override def optionalName: Option[Name] = parser.optionalName

    override def helpMessage: HelpMessage.ParamMessage = parser.helpMessage

    override def parseParams(params: List[Arg.ParamLike]): Params.ParseResult[B] =
      parser.parseParams(params).mapOrFail(this)(f)

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Params[B])] =
      parser.buildInternal(usedParams).map { case (up2, p) => (up2, MappedOrFail(p, f)) }

    override def map[C](f: B => C): Params[C] = Params.MappedOrFail(parser, this.f(_).map(f))
    override def mapOrFail[C](f: B => Either[String, C]): Params[C] = Params.MappedOrFail(parser, this.f(_).flatMap(f))

  }

  /**
    * Will attempt to parse [[left]], and then [[right]] with the remaining args after parsing [[left]].
    * Both [[left]] and [[right]] must succeed in order for this parser to succeed.
    */
  final case class And[A, B, O](
      left: Params[A],
      right: Params[B],
      zip: Zip.Out[A, B, O],
  ) extends Params[O] {

    override def optionalName: Option[Name] = None

    override def helpMessage: HelpMessage.ParamMessage = HelpMessage.ParamMessage.And(left.helpMessage, right.helpMessage)

    override def parseParams(params: List[Arg.ParamLike]): Params.ParseResult[O] =
      left.parseParams(params) match {
        case Params.ParseResult.Success(a, parsed1, params2) =>
          right.parseParams(params2) match {
            case Params.ParseResult.Success(b, parsed2, params3) => Params.ParseResult.Success(zip.zip(a, b), parsed1 ::: parsed2, params3)
            case fail @ Params.ParseResult.Fail(_, _)            => fail
          }
        case fail1 @ Params.ParseResult.Fail(error1, help1) =>
          right.parseParams(params) match {
            case Params.ParseResult.Success(_, _, _)    => fail1
            case Params.ParseResult.Fail(error2, help2) => Params.ParseResult.Fail(ParseError.ParamErrorAnd(error1, error2), HelpMessage.ParamMessage.And(help1, help2))
          }
      }

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Params[O])] =
      for {
        (up2, left2) <- left.buildInternal(usedParams)
        (up3, right2) <- right.buildInternal(up2)
      } yield (up3, And(left2, right2, zip))

  }

  /**
    * Will take all parsers specified, each attempting to parse the remaining arguments.
    * If none succeed, will return the errors from all parsers.
    * If only one succeeds, will return the success of that parser.
    * If multiple succeed, will return the first success order wise.
    *
    * Multi example:
    * --key-1=value-1 --key-2=value-2
    * Both succeed with `Key1("value-1")` and `Key2("value-2")`.
    * `Key1("value-1")` would be the success that is returned.
    *
    * This is useful when you want to be able to parse an ADT, and have the results come back in a fixed order.
    * In the above example, if you had 2 separate list parsers for `key-1` and `key-2`, you would not know what order they were parsed in.
    * But, if you combined them into an ADT, used [[Params.firstOf]], and [[Params.repeated]], then you would be able to know what order things were parsed in:
    * -  --key-1=value-1 --key-2=value-2     ->     `List(Key1("value-1"), Key2("value-2"))`
    * -  --key-2=value-2 --key-1=value-1     ->     `List(Key2("value-2"), Key1("value-1"))`
    */
  final case class FirstOfByArgIndex[A](
      options: NonEmptyList[Params[A]],
  ) extends Params[A] {

    override def optionalName: Option[Name] = None

    override def helpMessage: HelpMessage.ParamMessage = options.map(_.helpMessage).reduceLeft(HelpMessage.ParamMessage.Or(_, _))

    override def parseParams(params: List[Arg.ParamLike]): Params.ParseResult[A] = {
      val results = options.map(_.parseParams(params))
      val (errors, successes) = results.toList.partitionMap {
        case success @ Params.ParseResult.Success(_, _, _) => success.asRight
        case fail @ Params.ParseResult.Fail(_, _)          => fail.asLeft
      }

      successes match {
        case h :: Nil        => h
        case list @ (_ :: _) => list.minBy(s => ParsedParams(s.parsed.flatMap(_.args)))
        case Nil             => Params.ParseResult.Fail(errors.map(_._1).reduceLeft(ParseError.ParamErrorOr(_, _)), errors.map(_._2).reduceLeft(HelpMessage.ParamMessage.Or(_, _)))
      }
    }

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Params[A])] =
      options.traverse(_.buildInternal(usedParams)).map { nel => (nel.map(_._1).reduceLeft(_ | _), FirstOfByArgIndex(nel.map(_._2))) }

    override def map[B](f: A => B): Params[B] = Params.FirstOfByArgIndex(options.map(_.map(f)))
    override def mapOrFail[B](f: A => Either[String, B]): Params[B] = Params.FirstOfByArgIndex(options.map(_.mapOrFail(f)))

  }

  /**
    * Will attempt to parse [[left]].
    * If parsing [[left]] succeeds, parsing [[right]] will not be attempted, and this parser will succeed with [[left]]'s success.
    * If parsing [[left]] fails, parsing [[right]] will be attempted.
    * If parsing [[right]] succeeds, this parser will succeed with [[right]]'s success.
    * If parsing [[right]] fails, this parser will fail with both the errors of [[left]] and [[right]].
    */
  final case class Or[A](
      left: Params[A],
      right: Params[A],
  ) extends Params[A] {

    override def optionalName: Option[Name] = None

    override def helpMessage: HelpMessage.ParamMessage = HelpMessage.ParamMessage.Or(left.helpMessage, right.helpMessage)

    override def parseParams(params: List[Arg.ParamLike]): Params.ParseResult[A] =
      left.parseParams(params) match {
        case success @ Params.ParseResult.Success(_, _, _) => success
        case Params.ParseResult.Fail(error1, help1) =>
          right.parseParams(params) match {
            case success @ Params.ParseResult.Success(_, _, _) => success
            case Params.ParseResult.Fail(error2, help2)        => Params.ParseResult.Fail(ParseError.ParamErrorOr(error1, error2), HelpMessage.ParamMessage.Or(help1, help2))
          }
      }

    override def buildInternal(usedParams: Set[SimpleName]): Either[BuildError, (Set[SimpleName], Params[A])] =
      for {
        (up2, left2) <- left.buildInternal(usedParams)
        (up3, right2) <- right.buildInternal(usedParams)
      } yield (up2 | up3, Or(left2, right2))

    override def map[B](f: A => B): Params[B] = Params.Or(left.map(f), right.map(f))
    override def mapOrFail[B](f: A => Either[String, B]): Params[B] = Params.Or(left.mapOrFail(f), right.mapOrFail(f))

  }

  // =====|  |=====

  sealed trait ParseResult[+A] {

    final def map[B](f: A => B): ParseResult[B] = this match
      case ParseResult.Success(value, parsed, remaining) => ParseResult.Success(f(value), parsed, remaining)
      case fail @ ParseResult.Fail(_, _)                 => fail

    final def mapOrFail[B](parser: Params[?])(f: A => Either[String, B]): ParseResult[B] = this match {
      case ParseResult.Success(value, parsed, remaining) =>
        f(value) match {
          case Right(value) => ParseResult.Success(value, parsed, remaining)
          case Left(error)  => ParseResult.Fail(ParseError.SingleParamError(parsed, ParseError.FailedValidation(error)), parser.helpMessage.addHints(HelpHint.Error(error) :: Nil))
        }
      case fail @ ParseResult.Fail(_, _) => fail
    }

    final def toFinal: FinalParseResult[A] = this match
      case ParseResult.Success(value, parsed, Nil) => FinalParseResult.Success(value, parsed)
      case ParseResult.Success(_, _, h :: t)       => FinalParseResult.Fail(ParseError.UnparsedParams(NonEmptyList(h, t)), HelpMessage.ParamMessage.UnparsedArgs(NonEmptyList(h, t)))
      case ParseResult.Fail(error, help)           => FinalParseResult.Fail(error, help)

    final def toParserParseResult(values: List[Arg.ValueLike]): Parser.ParseResult[A] = this match
      case ParseResult.Success(value, parsed, remaining) => Parser.ParseResult.Success(value, parsed, values, remaining)
      case ParseResult.Fail(error, help)                 => Parser.ParseResult.Fail(error, help)

  }
  object ParseResult {
    final case class Success[+A](value: A, parsed: List[ParsedParamArg], remaining: List[Arg.ParamLike]) extends ParseResult[A]
    final case class Fail(error: ParseError.ParamError, help: HelpMessage.ParamMessage) extends ParseResult[Nothing]
  }

  sealed trait FinalParseResult[+A]
  object FinalParseResult {
    final case class Success[+A](value: A, parsed: List[ParsedParamArg]) extends FinalParseResult[A]
    final case class Fail(error: ParseError.ParamError | ParseError.UnparsedParams, help: HelpMessage.ParamMessage) extends FinalParseResult[Nothing]
  }

  final case class ParsedParams private (params: List[Arg.ParamLike])
  object ParsedParams {

    def apply(params: List[Arg.ParamLike]): ParsedParams = new ParsedParams(params.sorted)

    implicit val ordering: Ordering[ParsedParams] =
      (x: ParsedParams, y: ParsedParams) => {
        @tailrec
        def loop(
            x: List[Arg.ParamLike],
            y: List[Arg.ParamLike],
        ): Int =
          (x, y) match {
            case (xH :: xT, yH :: yT) =>
              Arg.ParamLike.ordering.compare(xH, yH) match {
                case 0 => loop(xT, yT)
                case c => c
              }
            case (_ :: _, Nil) => -1
            case (Nil, _ :: _) => 1
            case (Nil, Nil)    => 0
          }

        loop(x.params.sorted, y.params.sorted)
      }

  }

  // =====|  |=====

  def findParamTagged[A](names: Map[SimpleName, A], params: List[Arg.ParamLike]): Option[(Arg.ParamLike, A, List[Arg.ParamLike])] = {
    @tailrec
    def loop(
        queue: List[Arg.ParamLike],
        stack: List[Arg.ParamLike],
    ): Option[(Arg.ParamLike, A, List[Arg.ParamLike])] =
      queue match {
        case head :: tail =>
          names.get(head.name) match {
            case Some(tag) => (head, tag, stack.reverse ::: tail).some
            case None      => loop(tail, head :: stack)
          }
        case Nil => None
      }

    loop(params, Nil)
  }

  def findParam(names: Set[SimpleName], params: List[Arg.ParamLike]): Option[(Arg.ParamLike, List[Arg.ParamLike])] = {
    @tailrec
    def loop(
        queue: List[Arg.ParamLike],
        stack: List[Arg.ParamLike],
    ): Option[(Arg.ParamLike, List[Arg.ParamLike])] =
      queue match {
        case head :: tail if names.contains(head.name) => (head, stack.reverse ::: tail).some
        case head :: tail                              => loop(tail, head :: stack)
        case Nil                                       => None
      }

    loop(params, Nil)
  }

}
