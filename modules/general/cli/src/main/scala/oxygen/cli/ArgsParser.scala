package oxygen.cli

import oxygen.predef.core.*
import oxygen.schema.*
import scala.annotation.tailrec
import zio.*

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      ArgsParser
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait ArgsParser[+A] { self: ArgsParser.SelfT[A] =>

  type PositionalT
  type NamedT

  val positionalParser: PositionalArgsParser[PositionalT]
  val namedParser: NamedArgsParser[NamedT]
  def zipResult(positional: PositionalT, named: NamedT): A

  def help: Help
  def show: String

  // Implemented once for every parser: positional and named are parsed as two fully independent
  // passes over their own streams, then recombined via `zipResult`. See cli-decisions.md (D5).
  final def parseArgs(input: Args): CliParseResult[A, Args] =
    // Positional and named are independent streams, so we parse BOTH and report every failure rather than
    // short-circuiting on the first — a missing positional and a missing flag both surface together.
    (positionalParser.parsePositionalArgs(input.positional), namedParser.parseNamedArgs(input.named)) match
      case (CliParseResult.Success(posValue, posRemaining), CliParseResult.Success(namedValue, namedRemaining)) =>
        CliParseResult.Success(zipResult(posValue, namedValue), Args(posRemaining, namedRemaining))
      case (CliParseResult.Fail(e1, h1), CliParseResult.Fail(e2, h2)) =>
        CliParseResult.Fail(CliParseError.RootAnd(e1, e2), Help.And(h1, h2))
      case (fail @ CliParseResult.Fail(_, _), _) => fail
      case (_, fail @ CliParseResult.Fail(_, _)) => fail

  // Tab-completion: given the in-progress `value`, what could it be? Default offers nothing; overridden
  // by leaves (value completion via `CompletionOptions`, flag names via `Help`) and combinators.
  def complete(@scala.annotation.unused request: CompletionRequest, @scala.annotation.unused value: String): Task[List[String]] = ZIO.succeed(Nil)

  def as[B](f: => B): ArgsParser[B] = this.map { _ => f }
  def map[B](f: A => B): ArgsParser[B] = ArgsParser.Mapped(this, f)

  // Rewrite the named side's auto (`Defaultable.Default`) short names with a global, collision-aware
  // two-phase pass. Value-preserving: only which flags match changes, never the parse-result type/values.
  final def resolveAutoShortNames: ArgsParser[A] = ArgsParser.ShortNamesResolved(this)

}
object ArgsParser {

  ///////  ///////////////////////////////////////////////////////////////

  extension [A](self: ArgsParser[A])
    def ^>>&&[B](that: ArgsParser[B])(using zip: Zip[A, B]): ArgsParser[zip.Out] =
      (self, that, zip) match
        case (_, ArgsParser.Empty | PositionalArgsParser.Empty | NamedArgsParser.Empty, _: Zip.ZipIdUnit[?]) => self.asInstanceOf[ArgsParser[zip.Out]]
        case (ArgsParser.Empty | PositionalArgsParser.Empty | NamedArgsParser.Empty, _, _: Zip.ZipUnitId[?]) => that.asInstanceOf[ArgsParser[zip.Out]]
        case (_, ArgsParser.Empty | PositionalArgsParser.Empty | NamedArgsParser.Empty, _)                   => self.map(zip.zip(_, ()))
        case (ArgsParser.Empty | PositionalArgsParser.Empty | NamedArgsParser.Empty, _, _)                   => that.map(zip.zip((), _))
        case (self: PositionalArgsParser[A @unchecked], that: PositionalArgsParser[B @unchecked], _)         => self ^>> that
        case (self: NamedArgsParser[A @unchecked], that: NamedArgsParser[B @unchecked], _)                   => self && that
        case (self: PositionalArgsParser[A @unchecked], that: NamedArgsParser[B @unchecked], _)              => ArgsParser.Both(self, that, zip.zip)
        case (self: NamedArgsParser[A @unchecked], that: PositionalArgsParser[B @unchecked], _)              => ArgsParser.Both(that, self, (n, p) => zip.zip(p, n))
        case _                                                                                               => ArgsParser.ThenAndWith(self, that, zip.zip)

  ///////  ///////////////////////////////////////////////////////////////

  type SelfT[A] = PositionalArgsParser[A] | NamedArgsParser[A] | ArgsParser.Root[A]

  sealed trait Root[A] extends ArgsParser[A]

  case object Empty extends ArgsParser.Root[Unit] {

    override type PositionalT = Unit
    override type NamedT = Unit

    override val positionalParser: PositionalArgsParser[PositionalT] = PositionalArgsParser.Empty
    override val namedParser: NamedArgsParser[NamedT] = NamedArgsParser.Empty

    override def zipResult(positional: PositionalT, named: NamedT): Unit = ()

    override def help: Help = Help.Empty
    override def show: String = "ArgsParser.Empty"

  }

  final case class Both[P, N, B](
      positionalParser: PositionalArgsParser[P],
      namedParser: NamedArgsParser[N],
      zip: (P, N) => B,
  ) extends ArgsParser.Root[B] {

    override type PositionalT = P
    override type NamedT = N

    override def zipResult(positional: PositionalT, named: NamedT): B = zip(positional, named)

    override def help: Help = Help.And(positionalParser.help, namedParser.help)
    override def show: String = s"${positionalParser.show}\n\n${namedParser.show}"
    override def complete(request: CompletionRequest, value: String): Task[List[String]] =
      positionalParser.complete(request, value).zipWith(namedParser.complete(request, value))((p, n) => (p ::: n).distinct)

  }

  final case class ThenAndWith[A, B, C](
      a: ArgsParser[A],
      b: ArgsParser[B],
      zip: (A, B) => C,
  ) extends ArgsParser.Root[C] {

    override type PositionalT = (a.PositionalT, b.PositionalT)
    override type NamedT = (a.NamedT, b.NamedT)

    override val positionalParser: PositionalArgsParser[PositionalT] = a.positionalParser ^>> b.positionalParser
    override val namedParser: NamedArgsParser[NamedT] = a.namedParser && b.namedParser

    override def zipResult(positional: PositionalT, named: NamedT): C =
      zip(
        a.zipResult(positional._1, named._1),
        b.zipResult(positional._2, named._2),
      )

    override def help: Help = Help.And(positionalParser.help, namedParser.help)
    override def show: String = s"${positionalParser.show}\n\n${namedParser.show}"
    override def complete(request: CompletionRequest, value: String): Task[List[String]] =
      positionalParser.complete(request, value).zipWith(namedParser.complete(request, value))((p, n) => (p ::: n).distinct)

  }

  // TODO (KR) : could potentially support a `MapOrFail` here, only if `zipResult` was related to an `either`
  final case class Mapped[A, B](a: ArgsParser[A], f: A => B) extends ArgsParser.Root[B] {

    override type PositionalT = a.PositionalT
    override type NamedT = a.NamedT

    override val positionalParser: PositionalArgsParser[PositionalT] = a.positionalParser
    override val namedParser: NamedArgsParser[NamedT] = a.namedParser

    override def zipResult(positional: PositionalT, named: NamedT): B =
      f(a.zipResult(positional, named))

    override def help: Help = a.help
    override def show: String = a.show
    override def complete(request: CompletionRequest, value: String): Task[List[String]] = a.complete(request, value)

  }

  // Wraps a fully-assembled parser, exposing its named side with auto short names globally resolved.
  // Only the named parser is rebuilt (see `NamedArgsParser.resolveAutoShortNames`); positional parsing
  // and `zipResult` are delegated untouched, so results are identical to `underlying`.
  final case class ShortNamesResolved[A](underlying: ArgsParser[A]) extends ArgsParser.Root[A] {

    override type PositionalT = underlying.PositionalT
    override type NamedT = underlying.NamedT

    override val positionalParser: PositionalArgsParser[PositionalT] = underlying.positionalParser
    override val namedParser: NamedArgsParser[NamedT] = NamedArgsParser.resolveAutoShortNames(underlying.namedParser)

    override def zipResult(positional: PositionalT, named: NamedT): A = underlying.zipResult(positional, named)

    override def help: Help = Help.And(positionalParser.help, namedParser.help)
    override def show: String = underlying.show
    override def complete(request: CompletionRequest, value: String): Task[List[String]] =
      positionalParser.complete(request, value).zipWith(namedParser.complete(request, value))((p, n) => (p ::: n).distinct)

  }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      PositionalArgsParser
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait PositionalArgsParser[A] extends ArgsParser[A] {

  override final type PositionalT = A
  override final type NamedT = Unit

  val minLength: Int
  val maxLength: Option[Int]

  override final val positionalParser: PositionalArgsParser[A] = this
  override final val namedParser: NamedArgsParser[Unit] = NamedArgsParser.Empty
  override final def zipResult(positional: PositionalT, named: NamedT): A = positional

  def parsePositionalArgs(input: PositionalArgs): CliParseResult[A, PositionalArgs]

  override def as[B](f: => B): PositionalArgsParser[B] = PositionalArgsParser.Mapped(this, _ => f)
  override def map[B](f: A => B): PositionalArgsParser[B] = PositionalArgsParser.Mapped(this, f)
  def mapOrFail[B](f: A => Either[String, B]): PositionalArgsParser[B] = PositionalArgsParser.MappedOrFail(this, f)

  final def withDefault(default: A): PositionalArgsParser[A] =
    PositionalArgsParser.WithDefault(this, default, default.toString, breakOnAnyError = false)
  final def withDefault(default: A, shownDefault: String, breakOnAnyError: Boolean): PositionalArgsParser[A] =
    PositionalArgsParser.WithDefault(this, default, shownDefault, breakOnAnyError)

  override final def complete(request: CompletionRequest, value: String): Task[List[String]] =
    this match
      case s: PositionalArgsParser.Single[?]         => s.completion.completionOptions(value).map(_.toList)
      case t: PositionalArgsParser.ThenWith[?, ?, ?] =>
        // Decide whether the cursor is still in `a` or has moved to `b` by re-parsing the typed words.
        t.a.parsePositionalArgs(PositionalArgs(request.args.zipWithIndex.map((s, i) => PositionalArg(i, s)))) match
          case CliParseResult.Success(_, remaining) if remaining.args.isEmpty => t.b.complete(request, value)
          case _                                                              => t.a.complete(request, value)
      case o: PositionalArgsParser.Or[?]              => o.a.complete(request, value).zipWith(o.b.complete(request, value))((l, r) => (l ::: r).distinct)
      case o: PositionalArgsParser.Optional[?]        => o.inner.complete(request, value)
      case r: PositionalArgsParser.Repeated[?]        => r.inner.complete(request, value)
      case r: PositionalArgsParser.RepeatedNel[?]     => r.inner.complete(request, value)
      case w: PositionalArgsParser.WithDefault[?]     => w.inner.complete(request, value)
      case m: PositionalArgsParser.Mapped[?, ?]       => m.a.complete(request, value)
      case m: PositionalArgsParser.MappedOrFail[?, ?] => m.a.complete(request, value)
      case t: PositionalArgsParser.Tupled[?]          => ZIO.foreach(t.elems.toList)(_.complete(request, value)).map(_.flatten.distinct)
      case t: PositionalArgsParser.NameTupled[?]      => ZIO.foreach(t.elems.toList)(_.complete(request, value)).map(_.flatten.distinct)
      case _                                          => ZIO.succeed(Nil)

}
object PositionalArgsParser {

  inline def apply[A: PositionalArgsParser as b]: PositionalArgsParser[A] = b

  ///////  ///////////////////////////////////////////////////////////////

  def singlePlain[A: {PlainTextSchema as s, CompletionOptions as c}](name: String, help: SubHelp = SubHelp.Empty): PositionalArgsParser[A] =
    Single(name, help, s, c)
  def singleJson[A: {JsonSchema as s, CompletionOptions as c}](name: String, help: SubHelp = SubHelp.Empty): PositionalArgsParser[A] =
    Single(name, help, s, c)

  // Decode through the common `SchemaLike` base, regardless of which arm of the `AnySchemaT` union we hold. See cli-decisions.md (D7/D8).
  private[cli] def decodeAny[A](schema: AnySchemaT[A], string: String): Either[String, A] =
    schema match
      case s: PlainTextSchema[A @unchecked] => s.decode(string)
      case s: JsonSchema[A @unchecked]      => s.decode(string)

  ///////  ///////////////////////////////////////////////////////////////

  extension [A](self: PositionalArgsParser[A])
    def ^>>[B](that: PositionalArgsParser[B])(using zip: Zip[A, B]): PositionalArgsParser[zip.Out] =
      (self, that, zip) match
        case (_, PositionalArgsParser.Empty, _: Zip.ZipIdUnit[?])  => self.asInstanceOf[PositionalArgsParser[zip.Out]]
        case (PositionalArgsParser.Empty, _, _: Zip.ZipUnitId[?])  => that.asInstanceOf[PositionalArgsParser[zip.Out]]
        case (_, PositionalArgsParser.Empty, _: Zip.ZipIdId[?, ?]) => self.map(zip.zip(_, ()))
        case (PositionalArgsParser.Empty, _, _: Zip.ZipIdId[?, ?]) => that.map(zip.zip((), _))
        case _                                                     => PositionalArgsParser.ThenWith(self, that, zip.zip)

  ///////  ///////////////////////////////////////////////////////////////

  case object Empty extends PositionalArgsParser[Unit] {

    override val minLength: Int = 0
    override val maxLength: Option[Int] = 0.some

    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[Unit, PositionalArgs] = CliParseResult.Success((), input)

    override def help: Help = Help.Empty
    override def show: String = "PositionalArgsParser.Empty"

  }

  final case class Const[A](value: A) extends PositionalArgsParser[A] {

    override val minLength: Int = 0
    override val maxLength: Option[Int] = 0.some

    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[A, PositionalArgs] = CliParseResult.Success(value, input)

    override def help: Help = Help.Empty
    override def show: String = s"PositionalArgsParser.Const($value)"

  }

  final case class Or[A](
      a: PositionalArgsParser[? <: A],
      b: PositionalArgsParser[? <: A],
  ) extends PositionalArgsParser[A] {

    override val minLength: Int = a.minLength min b.minLength
    override val maxLength: Option[Int] =
      for {
        aLen <- a.maxLength
        bLen <- b.maxLength
      } yield aLen max bLen

    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[A, PositionalArgs] =
      a.parsePositionalArgs(input) match
        case success @ CliParseResult.Success(_, _) => success
        case CliParseResult.Fail(error1, help1)     =>
          b.parsePositionalArgs(input) match
            case success @ CliParseResult.Success(_, _) => success
            case CliParseResult.Fail(error2, help2)     => CliParseResult.Fail(CliParseError.PositionalOr(error1, error2), Help.Or(help1, help2))

    override def help: Help = Help.Or(a.help, b.help)
    override def show: String = s"( ${a.show} ) | ( ${b.show} )"

  }

  final case class ThenWith[A, B, C](
      a: PositionalArgsParser[A],
      b: PositionalArgsParser[B],
      zip: (A, B) => C,
  ) extends PositionalArgsParser[C] {

    override val minLength: Int = a.minLength + b.minLength
    override val maxLength: Option[Int] =
      for {
        aLen <- a.maxLength
        bLen <- b.maxLength
      } yield aLen + bLen

    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[C, PositionalArgs] =
      a.parsePositionalArgs(input) match
        case CliParseResult.Success(value1, remaining1) =>
          b.parsePositionalArgs(remaining1) match
            case CliParseResult.Success(value2, remaining2) => CliParseResult.Success(zip(value1, value2), remaining2)
            case fail @ CliParseResult.Fail(_, _)           => fail
        // `a` failed: still attempt `b` (on the same input — a missing-required `a` consumed nothing) so
        // both errors surface; if `b` also fails, combine them.
        case fail1 @ CliParseResult.Fail(error1, help1) =>
          b.parsePositionalArgs(input) match
            case CliParseResult.Success(_, _)       => fail1
            case CliParseResult.Fail(error2, help2) => CliParseResult.Fail(CliParseError.RootAnd(error1, error2), Help.And(help1, help2))

    override def help: Help = Help.And(a.help, b.help)
    override def show: String = s"${a.show}  ${b.show}"

  }

  /**
    * This is really unsafe, only use it if you really know what youre doing...
    */
  final case class Tupled[A <: Tuple](elems: IArray[PositionalArgsParser[?]], subHelp: SubHelp) extends PositionalArgsParser[A] {

    val tupleSize: Int = elems.length
    override val minLength: Int = elems.map(_.minLength).sum
    override val maxLength: Option[Int] = elems.toSeq.traverse(_.maxLength).map(_.sum)

    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[A, PositionalArgs] =
      parseAll(elems, input).map(arr => Tuple.fromArray(arr).asInstanceOf[A])

    override def toString: String = s"Tupled(${elems.map(_.toString).mkString(", ")})"

    override def help: Help = elems.foldLeft[Help](Help.Empty)((acc, e) => Help.And(acc, e.help))
    override def show: String = s"[ ${elems.map(_.show).mkString("  ")} ]"

  }

  /**
    * This is really unsafe, only use it if you really know what youre doing...
    */
  final case class NameTupled[A <: NamedTuple.AnyNamedTuple](elems: IArray[PositionalArgsParser[?]], subHelp: SubHelp) extends PositionalArgsParser[A] {

    val tupleSize: Int = elems.length
    override val minLength: Int = elems.map(_.minLength).sum
    override val maxLength: Option[Int] = elems.toSeq.traverse(_.maxLength).map(_.sum)

    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[A, PositionalArgs] =
      parseAll(elems, input).map(arr => Tuple.fromArray(arr).asInstanceOf[A])

    override def toString: String = s"NamedTupled(${elems.map(_.toString).mkString(", ")})"

    override def help: Help = elems.foldLeft[Help](Help.Empty)((acc, e) => Help.And(acc, e.help))
    override def show: String = s"[ ${elems.map(_.show).mkString("  ")} ]"

  }

  final case class Mapped[A, B](a: PositionalArgsParser[A], f: A => B) extends PositionalArgsParser[B] {

    override val minLength: Int = a.minLength
    override val maxLength: Option[Int] = a.maxLength

    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[B, PositionalArgs] = a.parsePositionalArgs(input).map(f)

    override def help: Help = a.help
    override def show: String = a.show

  }

  final case class MappedOrFail[A, B](a: PositionalArgsParser[A], f: A => Either[String, B]) extends PositionalArgsParser[B] {

    override val minLength: Int = a.minLength
    override val maxLength: Option[Int] = a.maxLength

    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[B, PositionalArgs] = a.parsePositionalArgs(input).mapOrFail(f)(a.help)

    override def help: Help = a.help
    override def show: String = a.show

  }

  final case class Single[A](name: String, subHelp: SubHelp, schema: AnySchemaT[A], completion: CompletionOptions[A]) extends PositionalArgsParser[A] {

    override val minLength: Int = 1
    override val maxLength: Option[Int] = 1.some

    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[A, PositionalArgs] =
      input.args match
        case (_ @PositionalArg(_, value)) :: rest =>
          decodeAny(schema, value) match
            case Right(decoded) => CliParseResult.Success(decoded, PositionalArgs(rest))
            case Left(message)  => CliParseResult.Fail(CliParseError.FailedValidation(message), help.withHints(HelpHint.Error(message) :: Nil))
        case Nil => CliParseResult.Fail(CliParseError.MissingRequiredPositional(name), help.withHints(HelpHint.Error("Missing required value") :: Nil))

    override def help: Help = Single.helpFor(name, subHelp, completion)
    override def show: String = s"[$name]"

  }
  object Single {
    def helpFor[A](name: String, subHelp: SubHelp, completion: CompletionOptions[A]): Help =
      val positional: Help.Positional = Help.Positional(name, subHelp)
      val hints: List[HelpHint] = CompletionOptions.helpHints(completion)
      if hints.isEmpty then positional
      else Help.WithHints(positional, NonEmptyList.unsafeFromList(hints))
  }

  final case class Optional[A](inner: PositionalArgsParser[A], breakOnAnyError: Boolean = false) extends PositionalArgsParser[Option[A]] {

    override val minLength: Int = 0
    override val maxLength: Option[Int] = inner.maxLength

    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[Option[A], PositionalArgs] =
      inner.parsePositionalArgs(input) match
        case CliParseResult.Success(value, remaining)                                              => CliParseResult.Success(value.some, remaining)
        case CliParseResult.Fail(error, _) if breakOnAnyError || error.onlyContainsMissingRequired => CliParseResult.Success(None, input)
        case fail @ CliParseResult.Fail(_, _)                                                      => fail

    override def help: Help = inner.help.withHints(HelpHint.Optional :: Nil)
    override def show: String = s"( ${inner.show} )?"

  }

  final case class Repeated[A](inner: PositionalArgsParser[A], breakOnAnyError: Boolean = true) extends PositionalArgsParser[List[A]] {

    override val minLength: Int = 0
    override val maxLength: Option[Int] = None

    @tailrec
    private def loop(input: PositionalArgs, rStack: List[A]): CliParseResult[List[A], PositionalArgs] =
      inner.parsePositionalArgs(input) match
        case CliParseResult.Success(value, remaining) if remaining.args.size < input.args.size     => loop(remaining, value :: rStack)
        case CliParseResult.Fail(error, _) if breakOnAnyError || error.onlyContainsMissingRequired => CliParseResult.Success(rStack.reverse, input)
        case fail @ CliParseResult.Fail(_, _)                                                      => fail
        case CliParseResult.Success(_, remaining)                                                  => CliParseResult.Success(rStack.reverse, remaining)

    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[List[A], PositionalArgs] = loop(input, Nil)

    override def help: Help = inner.help.withHints(HelpHint.Repeated :: Nil)
    override def show: String = s"( ${inner.show} )*"

  }

  final case class RepeatedNel[A](inner: PositionalArgsParser[A], breakOnAnyError: Boolean = true) extends PositionalArgsParser[NonEmptyList[A]] {

    override val minLength: Int = inner.minLength
    override val maxLength: Option[Int] = None

    @tailrec
    private def loop(input: PositionalArgs, rStack: NonEmptyList[A]): CliParseResult[NonEmptyList[A], PositionalArgs] =
      inner.parsePositionalArgs(input) match
        case CliParseResult.Success(value, remaining) if remaining.args.size < input.args.size     => loop(remaining, value :: rStack)
        case CliParseResult.Fail(error, _) if breakOnAnyError || error.onlyContainsMissingRequired => CliParseResult.Success(rStack.reverse, input)
        case fail @ CliParseResult.Fail(_, _)                                                      => fail
        case CliParseResult.Success(_, remaining)                                                  => CliParseResult.Success(rStack.reverse, remaining)

    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[NonEmptyList[A], PositionalArgs] =
      inner.parsePositionalArgs(input) match
        case CliParseResult.Success(value, remaining) => loop(remaining, NonEmptyList.one(value))
        case fail @ CliParseResult.Fail(_, _)         => fail

    override def help: Help = inner.help.withHints(HelpHint.RepeatedNel :: Nil)
    override def show: String = s"( ${inner.show} )+"

  }

  final case class WithDefault[A](inner: PositionalArgsParser[A], default: A, shownDefault: String, breakOnAnyError: Boolean) extends PositionalArgsParser[A] {

    override val minLength: Int = 0
    override val maxLength: Option[Int] = inner.maxLength

    override def parsePositionalArgs(input: PositionalArgs): CliParseResult[A, PositionalArgs] =
      inner.parsePositionalArgs(input) match
        case success @ CliParseResult.Success(_, _)                                                => success
        case CliParseResult.Fail(error, _) if breakOnAnyError || error.onlyContainsMissingRequired => CliParseResult.Success(default, input)
        case fail @ CliParseResult.Fail(_, _)                                                      => fail

    override def help: Help = inner.help.withHints(HelpHint.Default(shownDefault) :: Nil)
    override def show: String = s"${inner.show} <default: $shownDefault>"

  }

  ///////  ///////////////////////////////////////////////////////////////

  private def parseAll(elems: IArray[PositionalArgsParser[?]], input: PositionalArgs): CliParseResult[Array[Object], PositionalArgs] = {
    @tailrec
    def loop(i: Int, in: PositionalArgs, acc: List[Any], failAcc: Option[CliParseResult.Fail]): CliParseResult[Array[Object], PositionalArgs] =
      if i >= elems.length then
        failAcc match
          case Some(fail) => fail
          case None       => CliParseResult.Success(acc.reverse.toArray[Any].asInstanceOf[Array[Object]], in)
      else
        elems(i).parsePositionalArgs(in) match
          case CliParseResult.Success(value, remaining) => loop(i + 1, remaining, value :: acc, failAcc)
          // collect every element's failure (continuing on the same input, since a failed element consumed nothing)
          case CliParseResult.Fail(e, h) =>
            val combined = failAcc match
              case None                              => CliParseResult.Fail(e, h)
              case Some(CliParseResult.Fail(e0, h0)) => CliParseResult.Fail(CliParseError.RootAnd(e0, e), Help.And(h0, h))
            loop(i + 1, in, acc, Some(combined))
    loop(0, input, Nil, None)
  }

  ///////  ///////////////////////////////////////////////////////////////

  trait Builder[A] {
    def build(name: String, help: SubHelp): PositionalArgsParser[A]
  }
  object Builder extends BuilderLowPriority.LowPriority1 {

    inline def apply[A: Builder as b]: Builder[A] = b

    given built: [A: PositionalArgsParser as built] => PositionalArgsParser.Builder[A] = PositionalArgsParser.Builder.Built(built)

    final case class Built[A](built: PositionalArgsParser[A]) extends PositionalArgsParser.Builder[A] {
      override def build(name: String, help: SubHelp): PositionalArgsParser[A] = built // TODO (KR) : add help?
    }

    final case class Single[A](schema: AnySchemaT[A], completion: CompletionOptions[A]) extends PositionalArgsParser.Builder[A] {
      override def build(name: String, help: SubHelp): PositionalArgsParser[A] = PositionalArgsParser.Single(name, help, schema, completion)
    }

    final case class Optional[A](builder: PositionalArgsParser.Builder[A]) extends PositionalArgsParser.Builder[Option[A]] {
      override def build(name: String, help: SubHelp): PositionalArgsParser[Option[A]] = PositionalArgsParser.Optional(builder.build(name, help.withHints(HelpHint.Optional :: Nil)))
    }

    final case class Repeated[A](builder: PositionalArgsParser.Builder[A]) extends PositionalArgsParser.Builder[List[A]] {
      override def build(name: String, help: SubHelp): PositionalArgsParser[List[A]] = PositionalArgsParser.Repeated(builder.build(name, help.withHints(HelpHint.Repeated :: Nil)))
    }

    final case class RepeatedNel[A](builder: PositionalArgsParser.Builder[A]) extends PositionalArgsParser.Builder[NonEmptyList[A]] {
      override def build(name: String, help: SubHelp): PositionalArgsParser[NonEmptyList[A]] = PositionalArgsParser.RepeatedNel(builder.build(name, help.withHints(HelpHint.RepeatedNel :: Nil)))
    }

    sealed trait TupleBuilder[A <: Tuple] extends PositionalArgsParser.Builder[A] {

      def builderList: List[PositionalArgsParser.Builder[?]]

      override final def build(name: String, help: SubHelp): PositionalArgsParser[A] =
        PositionalArgsParser.Tupled[A](
          IArray.from(
            this.builderList.zipWithIndex.map { (builder, idx) => builder.build(s"${name}_${idx + 1}", SubHelp.Empty) },
          ),
          help,
        )

    }
    object TupleBuilder {

      given cons: [A: PositionalArgsParser.Builder as a, B <: Tuple: TupleBuilder as b] => TupleBuilder[A *: B] = Cons(a, b)
      given empty: TupleBuilder[EmptyTuple] = Empty

      final case class Cons[A, B <: Tuple](
          a: PositionalArgsParser.Builder[A],
          b: PositionalArgsParser.Builder.TupleBuilder[B],
      ) extends PositionalArgsParser.Builder.TupleBuilder[A *: B] {
        override def builderList: List[PositionalArgsParser.Builder[?]] = a :: b.builderList
      }

      case object Empty extends TupleBuilder[EmptyTuple] {
        override def builderList: List[Builder[?]] = Nil
      }

    }

    sealed trait NamedTupleBuilder[N <: Tuple, V <: Tuple] extends PositionalArgsParser.Builder[NamedTuple.NamedTuple[N, V]] {

      def builderList: List[(PositionalArgsParser.Builder[?], String)]

      override final def build(name: String, help: SubHelp): PositionalArgsParser[NamedTuple.NamedTuple[N, V]] =
        PositionalArgsParser.NameTupled[NamedTuple.NamedTuple[N, V]](
          IArray.from(
            this.builderList.map { (builder, tupName) => builder.build(s"${name}_${tupName}", SubHelp.Empty) },
          ),
          help,
        )

    }
    object NamedTupleBuilder {

      given cons: [HN <: String: ValueOf as hn, HV: PositionalArgsParser.Builder as a, TN <: Tuple, TV <: Tuple] => (b: NamedTupleBuilder[TN, TV]) => NamedTupleBuilder[HN *: TN, HV *: TV] =
        Cons(hn.value, a, b)
      given empty: NamedTupleBuilder[EmptyTuple, EmptyTuple] = Empty

      final case class Cons[HN <: String, HV, TN <: Tuple, TV <: Tuple](
          name: HN,
          a: PositionalArgsParser.Builder[HV],
          b: PositionalArgsParser.Builder.NamedTupleBuilder[TN, TV],
      ) extends PositionalArgsParser.Builder.NamedTupleBuilder[HN *: TN, HV *: TV] {
        override def builderList: List[(PositionalArgsParser.Builder[?], String)] = (a, name) :: b.builderList
      }

      case object Empty extends NamedTupleBuilder[EmptyTuple, EmptyTuple] {
        override def builderList: List[(Builder[?], String)] = Nil
      }

    }

  }

  object BuilderLowPriority {

    trait LowPriority1 extends LowPriority2 {

      given fromNamedTuple: [N <: Tuple, V <: Tuple] => (b: PositionalArgsParser.Builder.NamedTupleBuilder[N, V]) => PositionalArgsParser.Builder[NamedTuple.NamedTuple[N, V]] = b

      given optionPlainText: [A: {PlainTextSchema as s, CompletionOptions as o}] => PositionalArgsParser.Builder[Option[A]] = Builder.Optional(Builder.usingPlainTextSchema[A])
      given listPlainText: [A: {PlainTextSchema as s, CompletionOptions as o}] => PositionalArgsParser.Builder[List[A]] = Builder.Repeated(Builder.usingPlainTextSchema[A])
      given nonEmptyListPlainText: [A: {PlainTextSchema as s, CompletionOptions as o}] => PositionalArgsParser.Builder[NonEmptyList[A]] = Builder.RepeatedNel(Builder.usingPlainTextSchema[A])

    }

    trait LowPriority2 extends LowPriority3 {

      given fromTuple: [A <: Tuple: PositionalArgsParser.Builder.TupleBuilder as b] => PositionalArgsParser.Builder[A] = b

      given optionJson: [A: {JsonSchema as s, CompletionOptions as o}] => PositionalArgsParser.Builder[Option[A]] = Builder.Optional(Builder.usingJsonSchema[A])
      given listJson: [A: {JsonSchema as s, CompletionOptions as o}] => PositionalArgsParser.Builder[List[A]] = Builder.Repeated(Builder.usingJsonSchema[A])
      given nonEmptyListJson: [A: {JsonSchema as s, CompletionOptions as o}] => PositionalArgsParser.Builder[NonEmptyList[A]] = Builder.RepeatedNel(Builder.usingJsonSchema[A])

    }

    trait LowPriority3 extends LowPriority4 {

      given optionWrapped: [A: PositionalArgsParser.Builder as b] => PositionalArgsParser.Builder[Option[A]] = PositionalArgsParser.Builder.Optional(b)
      given listWrapped: [A: PositionalArgsParser.Builder as b] => PositionalArgsParser.Builder[List[A]] = PositionalArgsParser.Builder.Repeated(b)
      given nonEmptyListWrapped: [A: PositionalArgsParser.Builder as b] => PositionalArgsParser.Builder[NonEmptyList[A]] = PositionalArgsParser.Builder.RepeatedNel(b)

    }

    trait LowPriority4 extends LowPriority5 {

      given usingPlainTextSchema: [A: {PlainTextSchema as s, CompletionOptions as o}] => PositionalArgsParser.Builder[A] = PositionalArgsParser.Builder.Single(s, o)

    }

    trait LowPriority5 {

      given usingJsonSchema: [A: {JsonSchema as s, CompletionOptions as o}] => PositionalArgsParser.Builder[A] = PositionalArgsParser.Builder.Single(s, o)

    }

  }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      NamedArgsParser
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait NamedArgsParser[A] extends ArgsParser[A] {

  override final type PositionalT = Unit
  override final type NamedT = A

  override final val positionalParser: PositionalArgsParser[Unit] = PositionalArgsParser.Empty
  override final val namedParser: NamedArgsParser[A] = this
  override final def zipResult(positional: PositionalT, named: NamedT): A = named

  def parseNamedArgs(input: NamedArgs): CliParseResult[A, NamedArgs]

  override def as[B](f: => B): NamedArgsParser[B] = NamedArgsParser.Mapped(this, _ => f)
  override def map[B](f: A => B): NamedArgsParser[B] = NamedArgsParser.Mapped(this, f)
  def mapOrFail[B](f: A => Either[String, B]): NamedArgsParser[B] = NamedArgsParser.MappedOrFail(this, f)

  final def withDefault(default: A): NamedArgsParser[A] =
    NamedArgsParser.WithDefault(this, default, default.toString)
  final def withDefault(default: A, shownDefault: String): NamedArgsParser[A] =
    NamedArgsParser.WithDefault(this, default, shownDefault)

  override final def complete(request: CompletionRequest, value: String): Task[List[String]] =
    this match
      case f: NamedArgsParser.Flag =>
        ZIO.succeed(if value.isEmpty || value.startsWith("-") then CliHelp.paramNameCompletions(f.help, value) else Nil)
      case t: NamedArgsParser.Toggle =>
        ZIO.succeed(if value.isEmpty || value.startsWith("-") then CliHelp.paramNameCompletions(t.help, value) else Nil)
      case n: NamedArgsParser.Named[?] =>
        val short = NamedArgsParser.resolvedShortName(n.longName, n.shortName)
        if NamedArgsParser.completingFlagValue(request, n.longName, short) && !value.startsWith("-") then n.nested.complete(request, value)
        else if value.isEmpty || value.startsWith("-") then ZIO.succeed(CliHelp.paramNameCompletions(n.help, value))
        else n.nested.complete(request, value)
      case a: NamedArgsParser.AndWith[?, ?, ?]   => a.a.complete(request, value).zipWith(a.b.complete(request, value))((l, r) => (l ::: r).distinct)
      case o: NamedArgsParser.FirstOf[?]         => o.a.complete(request, value).zipWith(o.b.complete(request, value))((l, r) => (l ::: r).distinct)
      case o: NamedArgsParser.Optional[?]        => o.inner.complete(request, value)
      case r: NamedArgsParser.Repeated[?]        => r.inner.complete(request, value)
      case r: NamedArgsParser.RepeatedNel[?]     => r.inner.complete(request, value)
      case w: NamedArgsParser.WithDefault[?]     => w.inner.complete(request, value)
      case m: NamedArgsParser.Mapped[?, ?]       => m.a.complete(request, value)
      case m: NamedArgsParser.MappedOrFail[?, ?] => m.a.complete(request, value)
      case _                                     => ZIO.succeed(Nil)

}
object NamedArgsParser {

  inline def apply[A: NamedArgsParser as b]: NamedArgsParser[A] = b

  ///////  ///////////////////////////////////////////////////////////////

  // `Defaultable.Default` short names auto-derive from the first char of the long name. See cli-decisions.md (D2).
  // NB: this is the *local* fallback used by an un-normalized parser; `resolveAutoShortNames` below does the
  // global, collision-aware resolution and is what the executable pipeline applies before parsing/help.
  private[cli] def resolvedShortName(longName: String, shortName: Defaultable.Opt[Char]): Option[Char] =
    shortName match
      case Defaultable.Explicit(opt) => opt
      case Defaultable.Default       => longName.headOption

  // Two-phase auto short-name resolution over a fully-assembled named parser tree (D2):
  //   phase 1 — reserve every *explicitly* set short (`Explicit(Some)` on Named/Flag, both chars of a Toggle),
  //   phase 2 — walk left-to-right giving each `Default` the first char of its long name, but only if that
  //             char is still free; otherwise it gets no short (`Explicit(None)`).
  // Explicit names always win over autos, and earlier autos win over later ones. This replaces the old
  // per-node fallback that silently let two params both claim `-x`.
  def resolveAutoShortNames[A](parser: NamedArgsParser[A]): NamedArgsParser[A] = {
    val used: scala.collection.mutable.Set[Char] = scala.collection.mutable.Set.from(collectExplicitShorts(parser))

    def claimAuto(longName: String, shortName: Defaultable.Opt[Char]): Defaultable.Opt[Char] =
      shortName match
        case Defaultable.Explicit(_) => shortName
        case Defaultable.Default     =>
          longName.headOption match
            case None    => Defaultable.Explicit(None)
            case Some(c) =>
              // Try the first char as-is, then its case-flipped variant (`--host` -> -h, `--hotness` -> -H),
              // else no short. Only these two candidates, so a third `h*` param simply gets nothing.
              val flipped: Char = if c.isUpper then c.toLower else c.toUpper
              List(c, flipped).distinct.find(!used.contains(_)) match
                case Some(chosen) => used += chosen; Defaultable.Explicit(Some(chosen))
                case None         => Defaultable.Explicit(None)

    def rewrite[X](p: NamedArgsParser[X]): NamedArgsParser[X] =
      (p match
        case Empty       => Empty
        case c: Const[?] => c
        case t: Toggle   => t // toggles never auto-derive a short; only reserved in phase 1
        case n: Named[?] => Named(n.longName, claimAuto(n.longName, n.shortName), n.nested, n.subHelp)
        case f: Flag     => Flag(f.longName, claimAuto(f.longName, f.shortName), f.default, f.subHelp)
        // structural nodes: recurse (args evaluated left-to-right, preserving declaration order for phase 2)
        case a: AndWith[?, ?, ?]   => AndWith(rewrite(a.a), rewrite(a.b), a.zip)
        case m: Mapped[?, ?]       => Mapped(rewrite(m.a), m.f)
        case m: MappedOrFail[?, ?] => MappedOrFail(rewrite(m.a), m.f)
        case o: FirstOf[?]         => FirstOf(rewrite(o.a), rewrite(o.b))
        case o: Optional[?]        => Optional(rewrite(o.inner))
        case r: Repeated[?]        => Repeated(rewrite(r.inner))
        case r: RepeatedNel[?]     => RepeatedNel(rewrite(r.inner))
        case w: WithDefault[?]     => WithDefault(rewrite(w.inner), w.default, w.shownDefault)
      ).asInstanceOf[NamedArgsParser[X]]

    rewrite(parser)
  }

  private def collectExplicitShorts(parser: NamedArgsParser[?]): Set[Char] = {
    def loop(p: NamedArgsParser[?], acc: Set[Char]): Set[Char] =
      p match
        case Empty                 => acc
        case _: Const[?]           => acc
        case n: Named[?]           => n.shortName match { case Defaultable.Explicit(Some(c)) => acc + c; case _ => acc }
        case f: Flag               => f.shortName match { case Defaultable.Explicit(Some(c)) => acc + c; case _ => acc }
        case t: Toggle             => t.shortNames match { case Some((a, b)) => acc + a + b; case None => acc }
        case a: AndWith[?, ?, ?]   => loop(a.b, loop(a.a, acc))
        case m: Mapped[?, ?]       => loop(m.a, acc)
        case m: MappedOrFail[?, ?] => loop(m.a, acc)
        case o: FirstOf[?]         => loop(o.b, loop(o.a, acc))
        case o: Optional[?]        => loop(o.inner, acc)
        case r: Repeated[?]        => loop(r.inner, acc)
        case r: RepeatedNel[?]     => loop(r.inner, acc)
        case w: WithDefault[?]     => loop(w.inner, acc)
    loop(parser, Set.empty)
  }

  extension [A](self: NamedArgsParser[A])
    def &&[B](that: NamedArgsParser[B])(using zip: Zip[A, B]): NamedArgsParser[zip.Out] =
      (self, that, zip) match
        case (_, NamedArgsParser.Empty, _: Zip.ZipIdUnit[?])  => self.asInstanceOf[NamedArgsParser[zip.Out]]
        case (NamedArgsParser.Empty, _, _: Zip.ZipUnitId[?])  => that.asInstanceOf[NamedArgsParser[zip.Out]]
        case (_, NamedArgsParser.Empty, _: Zip.ZipIdId[?, ?]) => self.map(zip.zip(_, ()))
        case (NamedArgsParser.Empty, _, _: Zip.ZipIdId[?, ?]) => that.map(zip.zip((), _))
        case _                                                => NamedArgsParser.AndWith(self, that, zip.zip)

  ///////  ///////////////////////////////////////////////////////////////

  case object Empty extends NamedArgsParser[Unit] {

    override def parseNamedArgs(input: NamedArgs): CliParseResult[Unit, NamedArgs] = CliParseResult.Success((), input)

    override def help: Help = Help.Empty
    override def show: String = "NamedArgsParser.Empty"

  }

  final case class Const[A](value: A) extends NamedArgsParser[A] {

    override def parseNamedArgs(input: NamedArgs): CliParseResult[A, NamedArgs] = CliParseResult.Success(value, input)

    override def help: Help = Help.Empty
    override def show: String = s"NamedArgsParser.Const($value)"

  }

  final case class AndWith[A, B, C](
      a: NamedArgsParser[A],
      b: NamedArgsParser[B],
      zip: (A, B) => C,
  ) extends NamedArgsParser[C] {

    override def parseNamedArgs(input: NamedArgs): CliParseResult[C, NamedArgs] =
      a.parseNamedArgs(input) match
        case CliParseResult.Success(value1, remaining1) =>
          b.parseNamedArgs(remaining1) match
            case CliParseResult.Success(value2, remaining2) => CliParseResult.Success(zip(value1, value2), remaining2)
            case fail @ CliParseResult.Fail(_, _)           => fail
        case fail1 @ CliParseResult.Fail(error1, help1) =>
          b.parseNamedArgs(input) match
            case CliParseResult.Success(_, _)       => fail1
            case CliParseResult.Fail(error2, help2) => CliParseResult.Fail(CliParseError.RootAnd(error1, error2), Help.And(help1, help2))

    override def help: Help = Help.And(a.help, b.help)
    override def show: String = s"${a.show}\n${b.show}"

  }

  final case class Mapped[A, B](a: NamedArgsParser[A], f: A => B) extends NamedArgsParser[B] {

    override def parseNamedArgs(input: NamedArgs): CliParseResult[B, NamedArgs] = a.parseNamedArgs(input).map(f)

    override def help: Help = a.help
    override def show: String = a.show

  }

  final case class MappedOrFail[A, B](a: NamedArgsParser[A], f: A => Either[String, B]) extends NamedArgsParser[B] {

    override def parseNamedArgs(input: NamedArgs): CliParseResult[B, NamedArgs] = a.parseNamedArgs(input).mapOrFail(f)(a.help)

    override def help: Help = a.help
    override def show: String = a.show

  }

  final case class Named[A](longName: String, shortName: Defaultable.Opt[Char], nested: PositionalArgsParser[A], subHelp: SubHelp) extends NamedArgsParser[A] {

    private def resolvedShort: Option[Char] = resolvedShortName(longName, shortName)

    override def parseNamedArgs(input: NamedArgs): CliParseResult[A, NamedArgs] =
      findNamed(longName, resolvedShort, input.args) match
        case Some((nested0, rest)) =>
          val values = NamedArgNested.positional(nested0)
          nested.parsePositionalArgs(values) match
            case CliParseResult.Success(value, remaining) =>
              if remaining.args.isEmpty then CliParseResult.Success(value, NamedArgs(rest))
              else
                CliParseResult.Fail(
                  CliParseError.NamedUnexpectedValues(longName, NonEmptyList.unsafeFromList(remaining.args)),
                  help.withHints(HelpHint.Error("Requires a single value") :: Nil),
                )
            case fail @ CliParseResult.Fail(_, _) => fail
        case None => CliParseResult.Fail(CliParseError.MissingRequiredNamed(longName), help.withHints(HelpHint.Error("Missing required param") :: Nil))

    private def showShort: String = shortName match
      case Defaultable.Explicit(Some(value)) => s" -$value"
      case Defaultable.Explicit(None)        => ""
      case Defaultable.Default               => " -(auto)"

    override def help: Help = Help.Named(longName, resolvedShort, nested.help, subHelp)
    override def show: String = s"--$longName$showShort ( ${nested.show} )"

  }

  final case class Flag(longName: String, shortName: Defaultable.Opt[Char], default: Boolean, subHelp: SubHelp) extends NamedArgsParser[Boolean] {

    private def resolvedShort: Option[Char] = resolvedShortName(longName, shortName)

    override def parseNamedArgs(input: NamedArgs): CliParseResult[Boolean, NamedArgs] =
      findFlag(longName, resolvedShort, input.args) match
        case Some(rest) => CliParseResult.Success(!default, NamedArgs(rest))
        case None       => CliParseResult.Success(default, input)

    private def showShort: String = shortName match
      case Defaultable.Explicit(Some(value)) => s" -$value"
      case Defaultable.Explicit(None)        => ""
      case Defaultable.Default               => " -(auto)"

    override def help: Help = Help.Flag(longName, resolvedShort, subHelp)
    override def show: String = s"--$longName$showShort <$default>"

  }

  final case class Toggle(longNames: ToggleLongNameRepr, shortNames: Option[(Char, Char)], subHelp: SubHelp) extends NamedArgsParser[Boolean] {

    override def parseNamedArgs(input: NamedArgs): CliParseResult[Boolean, NamedArgs] =
      findToggle(longNames, shortNames, input.args) match
        case Some((value, rest)) => CliParseResult.Success(value, NamedArgs(rest))
        case None                =>
          CliParseResult.Fail(
            CliParseError.MissingRequiredNamed(s"${longNames.trueLongName}/${longNames.falseLongName}"),
            help.withHints(HelpHint.Error("Missing required param") :: Nil),
          )

    override def help: Help = Help.Toggle(longNames.trueLongName, longNames.falseLongName, shortNames, subHelp)
    override def show: String = s"--${longNames.trueLongName} / --${longNames.falseLongName}"

  }

  final case class FirstOf[A](
      a: NamedArgsParser[? <: A],
      b: NamedArgsParser[? <: A],
  ) extends NamedArgsParser[A] {

    override def parseNamedArgs(input: NamedArgs): CliParseResult[A, NamedArgs] =
      a.parseNamedArgs(input) match
        case success @ CliParseResult.Success(_, _) => success
        case CliParseResult.Fail(error1, help1)     =>
          b.parseNamedArgs(input) match
            case success @ CliParseResult.Success(_, _) => success
            case CliParseResult.Fail(error2, help2)     => CliParseResult.Fail(CliParseError.NamedOr(error1, error2), Help.Or(help1, help2))

    override def help: Help = Help.Or(a.help, b.help)
    override def show: String = s"( ${a.show} ) | ( ${b.show} )"

  }

  final case class Optional[A](inner: NamedArgsParser[A]) extends NamedArgsParser[Option[A]] {

    override def parseNamedArgs(input: NamedArgs): CliParseResult[Option[A], NamedArgs] =
      inner.parseNamedArgs(input) match
        case CliParseResult.Success(value, remaining)                           => CliParseResult.Success(value.some, remaining)
        case CliParseResult.Fail(error, _) if error.onlyContainsMissingRequired => CliParseResult.Success(None, input)
        case fail @ CliParseResult.Fail(_, _)                                   => fail

    override def help: Help = inner.help.withHints(HelpHint.Optional :: Nil)
    override def show: String = s"( ${inner.show} )?"

  }

  final case class Repeated[A](inner: NamedArgsParser[A]) extends NamedArgsParser[List[A]] {

    @tailrec
    private def loop(input: NamedArgs, rStack: List[A]): CliParseResult[List[A], NamedArgs] =
      inner.parseNamedArgs(input) match
        case CliParseResult.Success(value, remaining) if remaining.args.size < input.args.size => loop(remaining, value :: rStack)
        case CliParseResult.Fail(error, _) if error.onlyContainsMissingRequired                => CliParseResult.Success(rStack.reverse, input)
        case fail @ CliParseResult.Fail(_, _)                                                  => fail
        case CliParseResult.Success(_, remaining)                                              => CliParseResult.Success(rStack.reverse, remaining)

    override def parseNamedArgs(input: NamedArgs): CliParseResult[List[A], NamedArgs] = loop(input, Nil)

    override def help: Help = inner.help.withHints(HelpHint.Repeated :: Nil)
    override def show: String = s"( ${inner.show} )*"

  }

  final case class RepeatedNel[A](inner: NamedArgsParser[A]) extends NamedArgsParser[NonEmptyList[A]] {

    @tailrec
    private def loop(input: NamedArgs, rStack: NonEmptyList[A]): CliParseResult[NonEmptyList[A], NamedArgs] =
      inner.parseNamedArgs(input) match
        case CliParseResult.Success(value, remaining) if remaining.args.size < input.args.size => loop(remaining, value :: rStack)
        case CliParseResult.Fail(error, _) if error.onlyContainsMissingRequired                => CliParseResult.Success(rStack.reverse, input)
        case fail @ CliParseResult.Fail(_, _)                                                  => fail
        case CliParseResult.Success(_, remaining)                                              => CliParseResult.Success(rStack.reverse, remaining)

    override def parseNamedArgs(input: NamedArgs): CliParseResult[NonEmptyList[A], NamedArgs] =
      inner.parseNamedArgs(input) match
        case CliParseResult.Success(value, remaining) => loop(remaining, NonEmptyList.one(value))
        case fail @ CliParseResult.Fail(_, _)         => fail

    override def help: Help = inner.help.withHints(HelpHint.RepeatedNel :: Nil)
    override def show: String = s"( ${inner.show} )+"

  }

  final case class WithDefault[A](inner: NamedArgsParser[A], default: A, shownDefault: String) extends NamedArgsParser[A] {

    override def parseNamedArgs(input: NamedArgs): CliParseResult[A, NamedArgs] =
      inner.parseNamedArgs(input) match
        case success @ CliParseResult.Success(_, _)                             => success
        case CliParseResult.Fail(error, _) if error.onlyContainsMissingRequired => CliParseResult.Success(default, input)
        case fail @ CliParseResult.Fail(_, _)                                   => fail

    override def help: Help = inner.help.withHints(HelpHint.Default(shownDefault) :: Nil)
    override def show: String = s"${inner.show} <default: $shownDefault>"

  }

  ///////  ///////////////////////////////////////////////////////////////

  // True when the cursor sits in the value position right after `--longName` (so we complete the value, not flag names).
  private[cli] def completingFlagValue(request: CompletionRequest, longName: String, shortName: Option[Char]): Boolean =
    request.prefixArgs.nonEmpty && Args.parse(request.prefixArgs).toOption.exists { args =>
      findNamed(longName, shortName, args.named.args).exists { case (nested, _) => NamedArgNested.isEmpty(nested) }
    }

  private def findFlag(longName: String, shortName: Option[Char], args: List[NamedArg]): Option[List[NamedArg]] =
    findNamed(longName, shortName, args).map(_._2)

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

  private def findNamed(longName: String, shortName: Option[Char], args: List[NamedArg]): Option[(Any, List[NamedArg])] = {
    @tailrec
    def loop(queue: List[NamedArg], stack: List[NamedArg]): Option[(Any, List[NamedArg])] =
      queue match
        case LongNameArg(_, name, nested) :: tail if name == longName          => (nested, stack.reverse ::: tail).some
        case ShortNameArg(_, name, nested) :: tail if shortName.contains(name) => (nested, stack.reverse ::: tail).some
        case MultiShortNameArg(_, _, name) :: tail if shortName.contains(name) => (NamedArgNested.empty, stack.reverse ::: tail).some
        case head :: tail                                                      => loop(tail, head :: stack)
        case Nil                                                               => None
    loop(args, Nil)
  }

  ///////  ///////////////////////////////////////////////////////////////

  trait Builder[A] {
    def build(name: String, shortName: Defaultable.Opt[Char], help: SubHelp): NamedArgsParser[A]
  }
  object Builder extends BuilderLowPriority.LowPriority1 {

    inline def apply[A: Builder as b]: Builder[A] = b

    final case class Named[A](nested: PositionalArgsParser.Builder[A]) extends NamedArgsParser.Builder[A] {
      override def build(name: String, shortName: Defaultable.Opt[Char], help: SubHelp): NamedArgsParser[A] =
        NamedArgsParser.Named(name, shortName, nested.build(name, SubHelp.Empty), help)
    }

    final case class Optional[A](inner: NamedArgsParser.Builder[A]) extends NamedArgsParser.Builder[Option[A]] {
      override def build(name: String, shortName: Defaultable.Opt[Char], help: SubHelp): NamedArgsParser[Option[A]] =
        NamedArgsParser.Optional(inner.build(name, shortName, help.withHints(HelpHint.Optional :: Nil)))
    }

    final case class Repeated[A](inner: NamedArgsParser.Builder[A]) extends NamedArgsParser.Builder[List[A]] {
      override def build(name: String, shortName: Defaultable.Opt[Char], help: SubHelp): NamedArgsParser[List[A]] =
        NamedArgsParser.Repeated(inner.build(name, shortName, help.withHints(HelpHint.Repeated :: Nil)))
    }

    final case class RepeatedNel[A](inner: NamedArgsParser.Builder[A]) extends NamedArgsParser.Builder[NonEmptyList[A]] {
      override def build(name: String, shortName: Defaultable.Opt[Char], help: SubHelp): NamedArgsParser[NonEmptyList[A]] =
        NamedArgsParser.RepeatedNel(inner.build(name, shortName, help.withHints(HelpHint.RepeatedNel :: Nil)))
    }

  }

  object BuilderLowPriority {
    import PositionalArgsParser.Builder as PBuilder

    trait LowPriority1 extends LowPriority2 {

      given optionPlainText: [A: PBuilder as b] => NamedArgsParser.Builder[Option[A]] = Builder.Optional(Builder.Named(b))
      given listPlainText: [A: PBuilder as b] => NamedArgsParser.Builder[List[A]] = Builder.Repeated(Builder.Named(b))
      given nonEmptyListPlainText: [A: PBuilder as b] => NamedArgsParser.Builder[NonEmptyList[A]] = Builder.RepeatedNel(Builder.Named(b))

    }

    trait LowPriority2 {

      given named: [A: PBuilder as b] => NamedArgsParser.Builder[A] = Builder.Named(b)

    }

  }

}
