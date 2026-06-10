package oxygen.cli

import oxygen.core.typeclass.Zip
import oxygen.predef.core.*
import oxygen.schema.{JsonSchema, PlainTextSchema, SchemaLike}
import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance

sealed trait ArgsParser[+A] {

  def help: Help

  def parseArgs(input: Args): CliParseResult[A, Args]

  def map[B](f: A => B): ArgsParser[B] = ArgsParser.Mapped(this, f)
  def mapOrFail[B](f: A => Either[String, B]): ArgsParser[B] = ArgsParser.MappedOrFail(this, f)
  final def <||[A2 >: A](that: ArgsParser[A2]): ArgsParser[A2] = ArgsParser.Or(this, that)

  final def ^>>[B](that: ArgsParser[B])(using zip: Zip[A @uncheckedVariance, B]): ArgsParser[zip.Out] =
    ArgsParser.merge(this, that)(
      (a, b) => PositionalArgsParser.Then(a, b, zip),
      (a, b) => NamedArgsParser.And(a, b, zip),
      (a, b) => ArgsParser.Then(a, b, zip),
    )

}
object ArgsParser {

  trait Builder[+A] {
    def build(name: String, help: SubHelp): ArgsParser[A]
  }

  case object Empty extends ArgsParser[Unit] {
    override val help: Help = Help.Empty
    override def parseArgs(input: Args): CliParseResult[Unit, Args] = CliParseResult.Success((), input)
  }

  final case class Const[A](value: A) extends ArgsParser[A] {
    override val help: Help = Help.Empty
    override def parseArgs(input: Args): CliParseResult[A, Args] = CliParseResult.Success(value, input)
  }

  final case class Then[A, B, O](left: ArgsParser[A], right: ArgsParser[B], zip: Zip.Out[A, B, O]) extends ArgsParser[O] {
    override val help: Help = Help.And(left.help, right.help)
    override def parseArgs(input: Args): CliParseResult[O, Args] =
      left.parseArgs(input) match
        case CliParseResult.Success(value1, remaining1) =>
          right.parseArgs(remaining1) match
            case CliParseResult.Success(value2, remaining2) => CliParseResult.Success(zip.zip(value1, value2), remaining2)
            case fail @ CliParseResult.Fail(_, _)           => fail
        case fail @ CliParseResult.Fail(_, _) => fail
  }

  final case class Or[A](left: ArgsParser[A], right: ArgsParser[A]) extends ArgsParser[A] {
    override val help: Help = Help.Or(left.help, right.help)
    override def parseArgs(input: Args): CliParseResult[A, Args] =
      left.parseArgs(input) match
        case success @ CliParseResult.Success(_, _) => success
        case CliParseResult.Fail(error1, help1)     =>
          right.parseArgs(input) match
            case success @ CliParseResult.Success(_, _) => success
            case CliParseResult.Fail(error2, help2)     => CliParseResult.Fail(CliParseError.RootOr(error1, error2), Help.Or(help1, help2))
  }

  final case class Mapped[A, B](parser: ArgsParser[A], f: A => B) extends ArgsParser[B] {
    override val help: Help = parser.help
    override def parseArgs(input: Args): CliParseResult[B, Args] = parser.parseArgs(input).map(f)
  }

  final case class MappedOrFail[A, B](parser: ArgsParser[A], f: A => Either[String, B]) extends ArgsParser[B] {
    override val help: Help = parser.help
    override def parseArgs(input: Args): CliParseResult[B, Args] =
      parser.parseArgs(input) match
        case CliParseResult.Success(value, remaining) =>
          f(value) match
            case Right(value2) => CliParseResult.Success(value2, remaining)
            case Left(message) => CliParseResult.Fail(CliParseError.FailedValidation(message), parser.help.withHints(HelpHint.Error(message) :: Nil))
        case fail @ CliParseResult.Fail(_, _) => fail
  }

  final def toFinal[A](result: CliParseResult[A, Args]): Either[(CliParseError, Help), A] = result match
    case CliParseResult.Success(value, remaining) if remaining.isFullyConsumed => value.asRight
    case CliParseResult.Success(_, remaining) =>
      val pos = remaining.positional.args
      val named = remaining.named.args
      (pos, named) match
        case (upH :: upT, Nil)       => (CliParseError.UnparsedPositional(NonEmptyList(upH, upT)), Help.UnparsedPositional(NonEmptyList(upH, upT))).asLeft
        case (Nil, upH :: upT)       => (CliParseError.UnparsedNamed(NonEmptyList(upH, upT)), Help.UnparsedNamed(NonEmptyList(upH, upT))).asLeft
        case (upH :: upT, uppH :: uppT) =>
          (
            CliParseError.RootAnd(CliParseError.UnparsedPositional(NonEmptyList(upH, upT)), CliParseError.UnparsedNamed(NonEmptyList(uppH, uppT))),
            Help.And(Help.UnparsedPositional(NonEmptyList(upH, upT)), Help.UnparsedNamed(NonEmptyList(uppH, uppT))),
          ).asLeft
        case _ => sys.error("unreachable")
    case CliParseResult.Fail(error, help) => (error, help).asLeft

  val unit: ArgsParser[Unit] = Empty
  def const[A](value: A): ArgsParser[A] = Const(value)

  private def merge[A, B, C](a: ArgsParser[A], b: ArgsParser[B])(
      mergePositional: (PositionalArgsParser[A], PositionalArgsParser[B]) => ArgsParser[C],
      mergeNamed: (NamedArgsParser[A], NamedArgsParser[B]) => ArgsParser[C],
      mergeRoot: (ArgsParser[A], ArgsParser[B]) => ArgsParser[C],
  ): ArgsParser[C] =
    (a, b) match
      case (a: PositionalArgsParser[A], b: PositionalArgsParser[B]) => mergePositional(a, b)
      case (a: NamedArgsParser[A], b: NamedArgsParser[B])           => mergeNamed(a, b)
      case _                                                        => mergeRoot(a, b)

}

sealed trait PositionalArgsParser[+A] extends ArgsParser[A] {

  override final def parseArgs(input: Args): CliParseResult[A, Args] =
    parsePositionalArgs(input.positional).mapRemaining { remaining => Args(remaining, input.named) }

  def parsePositionalArgs(input: PositionalArgs): CliParseResult[A, PositionalArgs]

  override final def map[B](f: A => B): PositionalArgsParser[B] = PositionalArgsParser.Mapped(this, f)
  override final def mapOrFail[B](f: A => Either[String, B]): PositionalArgsParser[B] = PositionalArgsParser.MappedOrFail(this, f)

  final def optional: PositionalArgsParser[Option[A]] = PositionalArgsParser.Optional(this, breakOnAnyError = false)
  final def optional(breakOnAnyError: Boolean): PositionalArgsParser[Option[A]] = PositionalArgsParser.Optional(this, breakOnAnyError)
  final def repeated: PositionalArgsParser[List[A]] = PositionalArgsParser.Repeated(this, breakOnAnyError = true)
  final def repeated(breakOnAnyError: Boolean): PositionalArgsParser[List[A]] = PositionalArgsParser.Repeated(this, breakOnAnyError)
  final def repeatedNel: PositionalArgsParser[NonEmptyList[A]] = PositionalArgsParser.RepeatedNel(this, breakOnAnyError = true)
  final def repeatedNel(breakOnAnyError: Boolean): PositionalArgsParser[NonEmptyList[A]] = PositionalArgsParser.RepeatedNel(this, breakOnAnyError)
  final def withDefault[A2 >: A](default: A2): PositionalArgsParser[A2] = withDefault(default, breakOnAnyError = false, _.toString)
  final def withDefault[A2 >: A](default: A2, showDefault: A2 => String): PositionalArgsParser[A2] = withDefault(default, breakOnAnyError = false, showDefault)
  final def withDefault[A2 >: A](default: A2, breakOnAnyError: Boolean): PositionalArgsParser[A2] = withDefault(default, breakOnAnyError, _.toString)
  final def withDefault[A2 >: A](default: A2, breakOnAnyError: Boolean, showDefault: A2 => String): PositionalArgsParser[A2] =
    PositionalArgsParser.WithDefault(this, default, showDefault(default), breakOnAnyError)
  final def <||[A2 >: A](that: PositionalArgsParser[A2]): PositionalArgsParser[A2] = PositionalArgsParser.Or(this, that)
  final def ^>>[B](that: PositionalArgsParser[B])(using zip: Zip[A @uncheckedVariance, B]): PositionalArgsParser[zip.Out] = PositionalArgsParser.Then(this, that, zip)

}
object PositionalArgsParser {

  trait Builder[+A] {
    def build(name: String, help: SubHelp): PositionalArgsParser[A]
  }

  def single[A](name: String, help: SubHelp)(using schema: SchemaLike[A]): PositionalArgsParser[A] = Single(name, help, schema)

  case object Empty extends PositionalArgsParser[Unit] {
    override val help: Help = Help.Empty
    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[Unit, PositionalArgs] = CliParseResult.Success((), input)
  }

  final case class Const[A](value: A) extends PositionalArgsParser[A] {
    override val help: Help = Help.Empty
    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[A, PositionalArgs] = CliParseResult.Success(value, input)
  }

  final case class Single[A](name: String, subHelp: SubHelp, schema: SchemaLike[A]) extends PositionalArgsParser[A] {
    override val help: Help = Help.Positional(name, subHelp)
    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[A, PositionalArgs] =
      input.args match
        case (_ @ PositionalArg(_, value)) :: rest =>
          schema.decode(value) match
            case Right(decoded) => CliParseResult.Success(decoded, PositionalArgs(rest))
            case Left(message)  => CliParseResult.Fail(CliParseError.FailedValidation(message), help.withHints(HelpHint.Error(message) :: Nil))
        case Nil => CliParseResult.Fail(CliParseError.MissingRequiredPositional(name), help.withHints(HelpHint.Error("Missing required value") :: Nil))
  }

  final case class Optional[A](parser: PositionalArgsParser[A], breakOnAnyError: Boolean) extends PositionalArgsParser[Option[A]] {
    override val help: Help = parser.help.withHints(HelpHint.Optional :: Nil)
    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[Option[A], PositionalArgs] =
      parser.parsePositionalArgs(input) match
        case CliParseResult.Success(value, remaining)                                              => CliParseResult.Success(value.some, remaining)
        case CliParseResult.Fail(error, _) if breakOnAnyError || error.onlyContainsMissingRequired => CliParseResult.Success(None, input)
        case fail @ CliParseResult.Fail(_, _)                                                      => fail
  }

  final case class Repeated[A](parser: PositionalArgsParser[A], breakOnAnyError: Boolean) extends PositionalArgsParser[List[A]] {
    override val help: Help = parser.help.withHints(HelpHint.Repeated :: Nil)
    @tailrec
    private def loop(input: PositionalArgs, rStack: List[A]): CliParseResult[List[A], PositionalArgs] =
      parser.parsePositionalArgs(input) match
        case CliParseResult.Success(value, remaining) if remaining.args.size < input.args.size => loop(remaining, value :: rStack)
        case CliParseResult.Fail(error, _) if breakOnAnyError || error.onlyContainsMissingRequired => CliParseResult.Success(rStack.reverse, input)
        case fail @ CliParseResult.Fail(_, _) => fail
        case CliParseResult.Success(_, remaining) => CliParseResult.Success(rStack.reverse, remaining)
    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[List[A], PositionalArgs] = loop(input, Nil)
  }

  final case class RepeatedNel[A](parser: PositionalArgsParser[A], breakOnAnyError: Boolean) extends PositionalArgsParser[NonEmptyList[A]] {
    override val help: Help = parser.help.withHints(HelpHint.RepeatedNel :: Nil)
    @tailrec
    private def loop(input: PositionalArgs, rStack: NonEmptyList[A]): CliParseResult[NonEmptyList[A], PositionalArgs] =
      parser.parsePositionalArgs(input) match
        case CliParseResult.Success(value, remaining) if remaining.args.size < input.args.size => loop(remaining, value :: rStack)
        case CliParseResult.Fail(error, _) if breakOnAnyError || error.onlyContainsMissingRequired => CliParseResult.Success(rStack.reverse, input)
        case fail @ CliParseResult.Fail(_, _) => fail
        case CliParseResult.Success(_, remaining) => CliParseResult.Success(rStack.reverse, remaining)
    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[NonEmptyList[A], PositionalArgs] =
      parser.parsePositionalArgs(input) match
        case CliParseResult.Success(value, remaining) => loop(remaining, NonEmptyList.one(value))
        case fail @ CliParseResult.Fail(_, _)         => fail
  }

  final case class WithDefault[A](parser: PositionalArgsParser[A], default: A, shownDefault: String, breakOnAnyError: Boolean) extends PositionalArgsParser[A] {
    override val help: Help = parser.help.withHints(HelpHint.Default(shownDefault) :: Nil)
    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[A, PositionalArgs] =
      parser.parsePositionalArgs(input) match
        case success @ CliParseResult.Success(_, _)                                              => success
        case CliParseResult.Fail(error, _) if breakOnAnyError || error.onlyContainsMissingRequired => CliParseResult.Success(default, input)
        case fail @ CliParseResult.Fail(_, _)                                                    => fail
  }

  final case class Then[A, B, O](left: PositionalArgsParser[A], right: PositionalArgsParser[B], zip: Zip.Out[A, B, O]) extends PositionalArgsParser[O] {
    override val help: Help = Help.And(left.help, right.help)
    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[O, PositionalArgs] =
      left.parsePositionalArgs(input) match
        case CliParseResult.Success(value1, remaining1) =>
          right.parsePositionalArgs(remaining1) match
            case CliParseResult.Success(value2, remaining2) => CliParseResult.Success(zip.zip(value1, value2), remaining2)
            case fail @ CliParseResult.Fail(_, _)           => fail
        case fail @ CliParseResult.Fail(_, _) => fail
  }

  final case class Or[A](left: PositionalArgsParser[A], right: PositionalArgsParser[A]) extends PositionalArgsParser[A] {
    override val help: Help = Help.Or(left.help, right.help)
    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[A, PositionalArgs] =
      left.parsePositionalArgs(input) match
        case success @ CliParseResult.Success(_, _) => success
        case CliParseResult.Fail(error1, help1)     =>
          right.parsePositionalArgs(input) match
            case success @ CliParseResult.Success(_, _) => success
            case CliParseResult.Fail(error2, help2)     => CliParseResult.Fail(CliParseError.PositionalOr(error1, error2), Help.Or(help1, help2))
  }

  final case class Mapped[A, B](parser: PositionalArgsParser[A], f: A => B) extends PositionalArgsParser[B] {
    override val help: Help = parser.help
    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[B, PositionalArgs] = parser.parsePositionalArgs(input).map(f)
  }

  final case class MappedOrFail[A, B](parser: PositionalArgsParser[A], f: A => Either[String, B]) extends PositionalArgsParser[B] {
    override val help: Help = parser.help
    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[B, PositionalArgs] = parser.parsePositionalArgs(input).mapOrFail(f)(help)
  }

  val unit: PositionalArgsParser[Unit] = Empty
  def const[A](value: A): PositionalArgsParser[A] = Const(value)

  given plainTextBuilder: [A: PlainTextSchema as schema] => Builder[A] = new Builder[A] {
    override def build(name: String, help: SubHelp): PositionalArgsParser[A] = single(name, help)(using schema)
  }

  given jsonBuilder: [A: JsonSchema as schema] => Builder[A] = new Builder[A] {
    override def build(name: String, help: SubHelp): PositionalArgsParser[A] = single(name, help)(using schema)
  }

}

sealed trait NamedArgsParser[+A] extends ArgsParser[A] {

  override final def parseArgs(input: Args): CliParseResult[A, Args] =
    parseNamedArgs(input.named).mapRemaining { remaining => Args(input.positional, remaining) }

  def parseNamedArgs(input: NamedArgs): CliParseResult[A, NamedArgs]

  override final def map[B](f: A => B): NamedArgsParser[B] = NamedArgsParser.Mapped(this, f)
  override final def mapOrFail[B](f: A => Either[String, B]): NamedArgsParser[B] = NamedArgsParser.MappedOrFail(this, f)

  final def optional: NamedArgsParser[Option[A]] = NamedArgsParser.Optional(this)
  final def repeated: NamedArgsParser[List[A]] = NamedArgsParser.Repeated(this)
  final def repeatedNel: NamedArgsParser[NonEmptyList[A]] = NamedArgsParser.RepeatedNel(this)
  final def withDefault[A2 >: A](default: A2, showDefault: A2 => String = (_: A2).toString): NamedArgsParser[A2] = NamedArgsParser.WithDefault(this, default, showDefault(default))
  final def <||[A2 >: A](that: NamedArgsParser[A2]): NamedArgsParser[A2] = NamedArgsParser.Or(this, that)
  final def &&[B](that: NamedArgsParser[B])(using zip: Zip[A @uncheckedVariance, B]): NamedArgsParser[zip.Out] = NamedArgsParser.And(this, that, zip)

}
object NamedArgsParser {

  trait Builder[+A] {
    def build(name: String, help: SubHelp): NamedArgsParser[A]
  }

  def flag(longName: String, absentValue: Boolean = false, shortName: Option[Char] = None, help: SubHelp = SubHelp.Empty): NamedArgsParser[Boolean] =
    Flag(longName, absentValue, shortName, help)

  def toggle(longNames: ToggleLongNameRepr, shortNames: Option[(Char, Char)] = None, help: SubHelp = SubHelp.Empty): NamedArgsParser[Boolean] =
    Toggle(longNames, shortNames, help)

  def named[A](longName: String, valueParser: PositionalArgsParser[A], shortName: Option[Char] = None, help: SubHelp = SubHelp.Empty): NamedArgsParser[A] =
    Named(longName, shortName, valueParser, help)

  case object Empty extends NamedArgsParser[Unit] {
    override val help: Help = Help.Empty
    override def parseNamedArgs(input: NamedArgs): CliParseResult[Unit, NamedArgs] = CliParseResult.Success((), input)
  }

  final case class Const[A](value: A) extends NamedArgsParser[A] {
    override val help: Help = Help.Empty
    override def parseNamedArgs(input: NamedArgs): CliParseResult[A, NamedArgs] = CliParseResult.Success(value, input)
  }

  final case class Flag(longName: String, absentValue: Boolean, shortName: Option[Char], subHelp: SubHelp) extends NamedArgsParser[Boolean] {
    override val help: Help = Help.Flag(longName, shortName, subHelp)
    override def parseNamedArgs(input: NamedArgs): CliParseResult[Boolean, NamedArgs] =
      findFlag(longName, shortName, input.args) match
        case Some(rest) => CliParseResult.Success(!absentValue, NamedArgs(rest))
        case None       => CliParseResult.Success(absentValue, input)
  }

  final case class Toggle(longNames: ToggleLongNameRepr, shortNames: Option[(Char, Char)], subHelp: SubHelp) extends NamedArgsParser[Boolean] {
    override val help: Help = Help.Toggle(longNames.trueLongName, longNames.falseLongName, shortNames, subHelp)
    override def parseNamedArgs(input: NamedArgs): CliParseResult[Boolean, NamedArgs] =
      findToggle(longNames, shortNames, input.args) match
        case Some((value, rest)) => CliParseResult.Success(value, NamedArgs(rest))
        case None                => CliParseResult.Fail(CliParseError.MissingRequiredNamed(s"${longNames.trueLongName}/${longNames.falseLongName}"), help.withHints(HelpHint.Error("Missing required param") :: Nil))
  }

  final case class Named[A](longName: String, shortName: Option[Char], valueParser: PositionalArgsParser[A], subHelp: SubHelp) extends NamedArgsParser[A] {
    override val help: Help = Help.Named(longName, shortName, valueParser.help, subHelp)
    override def parseNamedArgs(input: NamedArgs): CliParseResult[A, NamedArgs] =
      findNamed(longName, shortName, input.args) match
        case Some((nested, rest)) =>
          val values = NamedArgNested.positional(nested)
          if values.args.isEmpty then CliParseResult.Fail(CliParseError.MissingRequiredPositional(longName), help.withHints(HelpHint.Error("Missing required value") :: Nil))
          else
            valueParser.parsePositionalArgs(values) match
              case CliParseResult.Success(value, remaining) =>
                if remaining.args.isEmpty then CliParseResult.Success(value, NamedArgs(rest))
                else CliParseResult.Fail(CliParseError.NamedUnexpectedValues(longName, NonEmptyList.unsafeFromList(remaining.args)), help)
              case fail @ CliParseResult.Fail(_, _) => fail
        case None => CliParseResult.Fail(CliParseError.MissingRequiredNamed(longName), help.withHints(HelpHint.Error("Missing required param") :: Nil))
  }

  final case class Optional[A](parser: NamedArgsParser[A]) extends NamedArgsParser[Option[A]] {
    override val help: Help = parser.help.withHints(HelpHint.Optional :: Nil)
    override def parseNamedArgs(input: NamedArgs): CliParseResult[Option[A], NamedArgs] =
      parser.parseNamedArgs(input) match
        case CliParseResult.Success(value, remaining)                           => CliParseResult.Success(value.some, remaining)
        case CliParseResult.Fail(error, _) if error.onlyContainsMissingRequired => CliParseResult.Success(None, input)
        case fail @ CliParseResult.Fail(_, _)                                   => fail
  }

  final case class Repeated[A](parser: NamedArgsParser[A]) extends NamedArgsParser[List[A]] {
    override val help: Help = parser.help.withHints(HelpHint.Repeated :: Nil)
    @tailrec
    private def loop(input: NamedArgs, rStack: List[A]): CliParseResult[List[A], NamedArgs] =
      parser.parseNamedArgs(input) match
        case CliParseResult.Success(value, remaining) if remaining.args.size < input.args.size => loop(remaining, value :: rStack)
        case CliParseResult.Fail(error, _) if error.onlyContainsMissingRequired                => CliParseResult.Success(rStack.reverse, input)
        case fail @ CliParseResult.Fail(_, _)                                                  => fail
        case CliParseResult.Success(_, remaining)                                              => CliParseResult.Success(rStack.reverse, remaining)
    override def parseNamedArgs(input: NamedArgs): CliParseResult[List[A], NamedArgs] = loop(input, Nil)
  }

  final case class RepeatedNel[A](parser: NamedArgsParser[A]) extends NamedArgsParser[NonEmptyList[A]] {
    override val help: Help = parser.help.withHints(HelpHint.RepeatedNel :: Nil)
    @tailrec
    private def loop(input: NamedArgs, rStack: NonEmptyList[A]): CliParseResult[NonEmptyList[A], NamedArgs] =
      parser.parseNamedArgs(input) match
        case CliParseResult.Success(value, remaining) if remaining.args.size < input.args.size => loop(remaining, value :: rStack)
        case CliParseResult.Fail(error, _) if error.onlyContainsMissingRequired                => CliParseResult.Success(rStack.reverse, input)
        case fail @ CliParseResult.Fail(_, _)                                                  => fail
        case CliParseResult.Success(_, remaining)                                              => CliParseResult.Success(rStack.reverse, remaining)
    override def parseNamedArgs(input: NamedArgs): CliParseResult[NonEmptyList[A], NamedArgs] =
      parser.parseNamedArgs(input) match
        case CliParseResult.Success(value, remaining) => loop(remaining, NonEmptyList.one(value))
        case fail @ CliParseResult.Fail(_, _)         => fail
  }

  final case class WithDefault[A](parser: NamedArgsParser[A], default: A, shownDefault: String) extends NamedArgsParser[A] {
    override val help: Help = parser.help.withHints(HelpHint.Default(shownDefault) :: Nil)
    override def parseNamedArgs(input: NamedArgs): CliParseResult[A, NamedArgs] =
      parser.parseNamedArgs(input) match
        case success @ CliParseResult.Success(_, _)                             => success
        case CliParseResult.Fail(error, _) if error.onlyContainsMissingRequired => CliParseResult.Success(default, input)
        case fail @ CliParseResult.Fail(_, _)                                   => fail
  }

  final case class And[A, B, O](left: NamedArgsParser[A], right: NamedArgsParser[B], zip: Zip.Out[A, B, O]) extends NamedArgsParser[O] {
    override val help: Help = Help.And(left.help, right.help)
    override def parseNamedArgs(input: NamedArgs): CliParseResult[O, NamedArgs] =
      left.parseNamedArgs(input) match
        case CliParseResult.Success(value1, remaining1) =>
          right.parseNamedArgs(remaining1) match
            case CliParseResult.Success(value2, remaining2) => CliParseResult.Success(zip.zip(value1, value2), remaining2)
            case fail @ CliParseResult.Fail(_, _)           => fail
        case fail1 @ CliParseResult.Fail(error1, help1) =>
          right.parseNamedArgs(input) match
            case CliParseResult.Success(_, _)       => fail1
            case CliParseResult.Fail(error2, help2) => CliParseResult.Fail(CliParseError.RootAnd(error1, error2), Help.And(help1, help2))
  }

  final case class Or[A](left: NamedArgsParser[A], right: NamedArgsParser[A]) extends NamedArgsParser[A] {
    override val help: Help = Help.Or(left.help, right.help)
    override def parseNamedArgs(input: NamedArgs): CliParseResult[A, NamedArgs] =
      left.parseNamedArgs(input) match
        case success @ CliParseResult.Success(_, _) => success
        case CliParseResult.Fail(error1, help1)     =>
          right.parseNamedArgs(input) match
            case success @ CliParseResult.Success(_, _) => success
            case CliParseResult.Fail(error2, help2)     => CliParseResult.Fail(CliParseError.NamedOr(error1, error2), Help.Or(help1, help2))
  }

  final case class Mapped[A, B](parser: NamedArgsParser[A], f: A => B) extends NamedArgsParser[B] {
    override val help: Help = parser.help
    override def parseNamedArgs(input: NamedArgs): CliParseResult[B, NamedArgs] = parser.parseNamedArgs(input).map(f)
  }

  final case class MappedOrFail[A, B](parser: NamedArgsParser[A], f: A => Either[String, B]) extends NamedArgsParser[B] {
    override val help: Help = parser.help
    override def parseNamedArgs(input: NamedArgs): CliParseResult[B, NamedArgs] = parser.parseNamedArgs(input).mapOrFail(f)(help)
  }

  val unit: NamedArgsParser[Unit] = Empty
  def const[A](value: A): NamedArgsParser[A] = Const(value)

  private def findFlag(longName: String, shortName: Option[Char], args: List[NamedArg]): Option[List[NamedArg]] =
    findNamed(longName, shortName, args).map(_._2)

  private def findNamed(longName: String, shortName: Option[Char], args: List[NamedArg]): Option[(Any, List[NamedArg])] = {
    @tailrec
    def loop(queue: List[NamedArg], stack: List[NamedArg]): Option[(Any, List[NamedArg])] =
      queue match
        case LongNameArg(_, name, nested) :: tail if name == longName            => (nested, stack.reverse ::: tail).some
        case ShortNameArg(_, name, nested) :: tail if shortName.contains(name)   => (nested, stack.reverse ::: tail).some
        case MultiShortNameArg(_, _, name) :: tail if shortName.contains(name)   => (NamedArgNested.empty, stack.reverse ::: tail).some
        case head :: tail                                                          => loop(tail, head :: stack)
        case Nil                                                                   => None
    loop(args, Nil)
  }

  private def findToggle(longNames: ToggleLongNameRepr, shortNames: Option[(Char, Char)], args: List[NamedArg]): Option[(Boolean, List[NamedArg])] = {
    @tailrec
    def loop(queue: List[NamedArg], stack: List[NamedArg]): Option[(Boolean, List[NamedArg])] =
      queue match
        case LongNameArg(_, name, nested) :: tail if name == longNames.trueLongName =>
          if NamedArgNested.isEmpty(nested) then (true, stack.reverse ::: tail).some else None
        case LongNameArg(_, name, nested) :: tail if name == longNames.falseLongName =>
          if NamedArgNested.isEmpty(nested) then (false, stack.reverse ::: tail).some else None
        case ShortNameArg(_, name, nested) :: tail =>
          shortNames match
            case Some((t, _)) if name == t && NamedArgNested.isEmpty(nested) => (true, stack.reverse ::: tail).some
            case Some((_, f)) if name == f && NamedArgNested.isEmpty(nested) => (false, stack.reverse ::: tail).some
            case _                                                           => loop(tail, queue.head :: stack)
        case head :: tail => loop(tail, head :: stack)
        case Nil          => None
    loop(args, Nil)
  }

  given plainTextBuilder: [A: PlainTextSchema as schema] => Builder[A] = new Builder[A] {
    override def build(name: String, help: SubHelp): NamedArgsParser[A] = named(name, PositionalArgsParser.single(name, help)(using schema), help = help)
  }

  given jsonBuilder: [A: JsonSchema as schema] => Builder[A] = new Builder[A] {
    override def build(name: String, help: SubHelp): NamedArgsParser[A] = named(name, PositionalArgsParser.single(name, help)(using schema), help = help)
  }

}