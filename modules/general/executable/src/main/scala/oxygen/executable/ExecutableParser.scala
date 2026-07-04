package oxygen.executable

import oxygen.cli.*
import oxygen.predef.core.*
import oxygen.schema.*
import zio.*

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      ExecutableParser
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait ExecutableParser[+A] { self: ExecutableParser.SelfT[A] =>

  type CliT
  type NonCliT

  val cliParser: ArgsParser[CliT]
  val nonCliParser: NonCLIExecutableParser[NonCliT]
  def zipResult(cli: CliT, nonCli: NonCliT): A

  // Parse the CLI args (pure), enforce full consumption, then evaluate the non-CLI (env/config)
  // sources as an effect, then recombine. The CLI pass is split positional-vs-named inside
  // `ArgsParser#parseArgs`; this layer just zips the two halves of the executable's inputs back
  // together. Failures surface as a `CliParseResult.Fail` (which carries the `Help` to render).
  // See cli-decisions.md (D5/D6).
  final def parse(input: Args): IO[CliParseResult.Fail, A] = {
    val cliResult: Either[CliParseResult.Fail, CliT] =
      cliParser.parseArgs(input) match
        case CliParseResult.Success(cliValue, remaining) =>
          ExecutableParser.checkFullyConsumed(remaining).toLeft(cliValue)
        case fail @ CliParseResult.Fail(_, _) => Left(fail)
    // Evaluate env/config even when the CLI parse failed, so a missing env var is reported alongside
    // (not behind) the CLI errors.
    nonCliParser.load.either.flatMap { envResult =>
      (cliResult, envResult) match
        case (Right(cliValue), Right(nonCliValue)) => ZIO.succeed(zipResult(cliValue, nonCliValue))
        case (Left(cliFail), Right(_))             => ZIO.fail(cliFail)
        case (Right(_), Left(message))             => ZIO.fail(ExecutableParser.envFail(message))
        case (Left(cliFail), Left(message))        =>
          val ef = ExecutableParser.envFail(message)
          ZIO.fail(CliParseResult.Fail(CliParseError.RootAnd(cliFail.error, ef.error), Help.And(cliFail.help, ef.help)))
    }
  }

  // Completion only concerns the CLI args; env/config sources contribute nothing to tab-completion.
  final def complete(request: CompletionRequest, value: String): Task[List[String]] =
    cliParser.complete(request, value)

  final def help: Help = cliParser.help

  def as[B](f: => B): ExecutableParser[B] = ExecutableParser.Mapped(this, _ => f)
  def map[B](f: A => B): ExecutableParser[B] = ExecutableParser.Mapped(this, f)

  // Resolve the CLI side's auto short names with a global two-phase pass. Applied once at the
  // fully-assembled command parser (see `CompiledCliApp.Single.fullParser`) so every named arg on the
  // command line — instance, layer (`def env`), and effect params — shares one collision namespace.
  final def resolveAutoShortNames: ExecutableParser[A] = ExecutableParser.CliShortNamesResolved(this)

}
object ExecutableParser {

  ///////  ///////////////////////////////////////////////////////////////

  private[executable] def envFail(message: String): CliParseResult.Fail =
    // NB: `Help.Empty.withHints(...)` is a no-op (Empty isn't a `Help.Base`), so use a renderable node.
    CliParseResult.Fail(CliParseError.MissingEnvVar(message), Help.Raw(message))

  // After parsing, any leftover positional/named args are an error (mirrors the old `ArgsParser.toFinal`).
  private[executable] def checkFullyConsumed(remaining: Args): Option[CliParseResult.Fail] =
    (remaining.positional.args, remaining.named.args) match
      case (Nil, Nil)      => None
      case (pH :: pT, Nil) =>
        Some(CliParseResult.Fail(CliParseError.UnparsedPositional(NonEmptyList(pH, pT)), Help.UnparsedPositional(NonEmptyList(pH, pT))))
      case (Nil, nH :: nT) =>
        Some(CliParseResult.Fail(CliParseError.UnparsedNamed(NonEmptyList(nH, nT)), Help.UnparsedNamed(NonEmptyList(nH, nT))))
      case (pH :: pT, nH :: nT) =>
        Some(
          CliParseResult.Fail(
            CliParseError.RootAnd(CliParseError.UnparsedPositional(NonEmptyList(pH, pT)), CliParseError.UnparsedNamed(NonEmptyList(nH, nT))),
            Help.And(Help.UnparsedPositional(NonEmptyList(pH, pT)), Help.UnparsedNamed(NonEmptyList(nH, nT))),
          ),
        )

  ///////  ///////////////////////////////////////////////////////////////

  extension [A](self: ExecutableParser[A])
    def ^>>&&##[B](that: ExecutableParser[B])(using zip: Zip[A, B]): ExecutableParser[zip.Out] =
      (self, that, zip) match {
        case (_, ExecutableParser.Empty | CLIExecutableParser(ArgsParser.Empty | PositionalArgsParser.Empty | NamedArgsParser.Empty), _: Zip.ZipIdUnit[?]) =>
          self.asInstanceOf[ExecutableParser[zip.Out]]
        case (ExecutableParser.Empty | CLIExecutableParser(ArgsParser.Empty | PositionalArgsParser.Empty | NamedArgsParser.Empty), _, _: Zip.ZipUnitId[?]) =>
          self.asInstanceOf[ExecutableParser[zip.Out]]
        case (_, ExecutableParser.Empty | CLIExecutableParser(ArgsParser.Empty | PositionalArgsParser.Empty | NamedArgsParser.Empty), _: Zip.ZipIdId[?, ?]) => self.map(zip.zip(_, ()))
        case (ExecutableParser.Empty | CLIExecutableParser(ArgsParser.Empty | PositionalArgsParser.Empty | NamedArgsParser.Empty), _, _: Zip.ZipIdId[?, ?]) => that.map(zip.zip((), _))
        case (self: CLIExecutableParser[A @unchecked], that: CLIExecutableParser[B @unchecked], _)                                                          => self ^>>&& that
        case (self: NonCLIExecutableParser[A @unchecked], that: NonCLIExecutableParser[B @unchecked], _)                                                    => self ## that
        case (self: CLIExecutableParser[A @unchecked], that: NonCLIExecutableParser[B @unchecked], _) => ExecutableParser.Both(self.cliParser, that, zip.zip)
        case (self: NonCLIExecutableParser[A @unchecked], that: CLIExecutableParser[B @unchecked], _) => ExecutableParser.Both(that.cliParser, self, (c, nc) => zip.zip(nc, c))
        case _                                                                                        => ExecutableParser.ThenAndWith(self, that, zip.zip)
      }

  ///////  ///////////////////////////////////////////////////////////////

  type SelfT[A] = CLIExecutableParser[A] | NonCLIExecutableParser[A] | ExecutableParser.Root[A]

  sealed trait Root[A] extends ExecutableParser[A]

  case object Empty extends ExecutableParser.Root[Unit] {

    override type CliT = Unit
    override type NonCliT = Unit

    override val cliParser: ArgsParser[CliT] = ArgsParser.Empty
    override val nonCliParser: NonCLIExecutableParser[NonCliT] = NonCLIExecutableParser.Empty
    override def zipResult(cli: CliT, nonCli: NonCliT): Unit = ()

  }

  final case class Both[C, NC, B](
      cliParser: ArgsParser[C],
      nonCliParser: NonCLIExecutableParser[NC],
      zip: (C, NC) => B,
  ) extends ExecutableParser.Root[B] {

    override type CliT = C
    override type NonCliT = NC

    override def zipResult(cli: CliT, nonCli: NonCliT): B = zip(cli, nonCli)

  }

  final case class ThenAndWith[A, B, C](
      a: ExecutableParser[A],
      b: ExecutableParser[B],
      zip: (A, B) => C,
  ) extends ExecutableParser.Root[C] {

    override type CliT = (a.CliT, b.CliT)
    override type NonCliT = (a.NonCliT, b.NonCliT)

    override val cliParser: ArgsParser[CliT] = a.cliParser ^>>&& b.cliParser
    override val nonCliParser: NonCLIExecutableParser[NonCliT] = a.nonCliParser ## b.nonCliParser
    override def zipResult(cli: CliT, nonCli: NonCliT): C =
      zip(
        a.zipResult(cli._1, nonCli._1),
        b.zipResult(cli._2, nonCli._2),
      )

  }

  final case class Mapped[A, B](
      a: ExecutableParser[A],
      f: A => B,
  ) extends ExecutableParser.Root[B] {

    override type CliT = a.CliT
    override type NonCliT = a.NonCliT

    override val cliParser: ArgsParser[CliT] = a.cliParser
    override val nonCliParser: NonCLIExecutableParser[NonCliT] = a.nonCliParser

    // TODO (KR) : could potentially support a `MapOrFail` here, only if `zipResult` was related to an `either`
    override def zipResult(cli: CliT, nonCli: NonCliT): B =
      f(a.zipResult(cli, nonCli))

  }

  // Exposes `inner` with its CLI parser's auto short names globally resolved; the non-CLI side and
  // `zipResult` are delegated untouched, so parse results are identical to `inner`.
  final case class CliShortNamesResolved[A](inner: ExecutableParser[A]) extends ExecutableParser.Root[A] {

    override type CliT = inner.CliT
    override type NonCliT = inner.NonCliT

    override val cliParser: ArgsParser[CliT] = inner.cliParser.resolveAutoShortNames
    override val nonCliParser: NonCLIExecutableParser[NonCliT] = inner.nonCliParser
    override def zipResult(cli: CliT, nonCli: NonCliT): A = inner.zipResult(cli, nonCli)

  }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      CLIExecutableParser
//////////////////////////////////////////////////////////////////////////////////////////////////////

final case class CLIExecutableParser[A](cliParser: ArgsParser[A]) extends ExecutableParser[A] {

  override type CliT = A
  override type NonCliT = Unit

  override val nonCliParser: NonCLIExecutableParser[Unit] = NonCLIExecutableParser.Empty
  override def zipResult(cli: CliT, nonCli: NonCliT): A = cli

  override def as[B](f: => B): CLIExecutableParser[B] = CLIExecutableParser(cliParser.as(f))
  override def map[B](f: A => B): CLIExecutableParser[B] = CLIExecutableParser(cliParser.map(f))

}
object CLIExecutableParser {

  extension [A](self: CLIExecutableParser[A])
    def ^>>&&[B](that: CLIExecutableParser[B])(using zip: Zip[A, B]): CLIExecutableParser[zip.Out] =
      CLIExecutableParser(self.cliParser ^>>&& that.cliParser)

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      NonCLIExecutableParser
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait NonCLIExecutableParser[A] extends ExecutableParser[A] {

  override type CliT = Unit
  override type NonCliT = A

  override val cliParser: ArgsParser[Unit] = ArgsParser.Empty
  override val nonCliParser: NonCLIExecutableParser[A] = this
  override def zipResult(cli: CliT, nonCli: NonCliT): A = nonCli

  // Evaluate this non-CLI source (environment / config) as an effect. Env reads and config file IO
  // are genuinely effectful, so this is a `ZIO` rather than a pure value. See cli-decisions.md (D6).
  def load: IO[String, A]

  override def as[B](f: => B): NonCLIExecutableParser[B] = NonCLIExecutableParser.Mapped(this, _ => f)
  override def map[B](f: A => B): NonCLIExecutableParser[B] = NonCLIExecutableParser.Mapped(this, f)

}
object NonCLIExecutableParser {

  extension [A](self: NonCLIExecutableParser[A])
    def ##[B](that: NonCLIExecutableParser[B])(using zip: Zip[A, B]): NonCLIExecutableParser[zip.Out] =
      (self, that, zip) match
        case (_, NonCLIExecutableParser.Empty, _: Zip.ZipIdUnit[?])  => self.asInstanceOf[NonCLIExecutableParser[zip.Out]]
        case (NonCLIExecutableParser.Empty, _, _: Zip.ZipUnitId[?])  => that.asInstanceOf[NonCLIExecutableParser[zip.Out]]
        case (_, NonCLIExecutableParser.Empty, _: Zip.ZipIdId[?, ?]) => self.map(zip.zip(_, ()))
        case (NonCLIExecutableParser.Empty, _, _: Zip.ZipIdId[?, ?]) => that.map(zip.zip((), _))
        case _                                                       => NonCLIExecutableParser.AndWith(self, that, zip.zip)

  ///////  ///////////////////////////////////////////////////////////////

  private def envValue(varName: String): Option[String] = Option(java.lang.System.getenv(varName))

  // Decode the raw env-var string directly via whichever arm of the `AnySchemaT` union we hold.
  private def decodeEnv[A](schema: AnySchemaT[A], raw: String): Either[String, A] =
    schema match
      case s: PlainTextSchema[A @unchecked] => s.decode(raw)
      case s: JsonSchema[A @unchecked]      => s.decode(raw)

  // Resolve a config value/path (inline JSON / file / merged dir) and decode it via the param's JsonDecoder
  // (which `derives JsonCodec` or `derives JsonSchema` both provide — matching the old `@config`).
  private def loadConfig[A](decoder: oxygen.json.JsonDecoder[A], raw: String): Either[String, A] =
    oxygen.executable.generic.ConfigLoader.loadDecoded(raw, decoder)

  ///////  ///////////////////////////////////////////////////////////////

  case object Empty extends NonCLIExecutableParser[Unit] {
    override def load: IO[String, Unit] = ZIO.unit
  }

  final case class AndWith[A, B, C](
      a: NonCLIExecutableParser[A],
      b: NonCLIExecutableParser[B],
      zip: (A, B) => C,
  ) extends NonCLIExecutableParser[C] {
    override def load: IO[String, C] = a.load.zipWith(b.load)(zip)
  }

  final case class Mapped[A, B](
      a: NonCLIExecutableParser[A],
      f: A => B,
  ) extends NonCLIExecutableParser[B] {
    override def load: IO[String, B] = a.load.map(f)
  }

  /////// EnvVar ///////////////////////////////////////////////////////////////

  // `read` carries presence: `None` means the underlying env var is unset (a valid state that
  // `Optional`/`Default` can recover from); `load` then treats a bare `EnvVar` as required.
  sealed trait EnvVar[A] extends NonCLIExecutableParser[A] {
    def read: IO[String, Option[A]]
    protected def missingMessage: String = "Required environment variable is not set"
    override final def load: IO[String, A] =
      read.flatMap {
        case Some(value) => ZIO.succeed(value)
        case None        => ZIO.fail(missingMessage)
      }
  }

  final case class SingleEnvVar[A](varName: String, schema: AnySchemaT[A]) extends EnvVar[A] {
    override protected def missingMessage: String = s"Environment variable $varName is not set"
    override def read: IO[String, Option[A]] =
      ZIO.succeed(envValue(varName)).flatMap {
        case None      => ZIO.none
        case Some(raw) => ZIO.fromEither(decodeEnv(schema, raw)).asSome
      }
  }

  final case class OptionalEnvVar[A](inner: EnvVar[A]) extends EnvVar[Option[A]] {
    override def read: IO[String, Option[Option[A]]] = inner.read.asSome
  }

  final case class DefaultEnvVar[A](inner: EnvVar[A], default: A) extends EnvVar[A] {
    override def read: IO[String, Option[A]] = inner.read.map(opt => opt.getOrElse(default).some)
  }

  /////// EnvVarConfig ///////////////////////////////////////////////////////////////

  sealed trait EnvVarConfig[A] extends NonCLIExecutableParser[A] {
    def read: IO[String, Option[A]]
    protected def missingMessage: String = "Required config environment variable is not set"
    override final def load: IO[String, A] =
      read.flatMap {
        case Some(value) => ZIO.succeed(value)
        case None        => ZIO.fail(missingMessage)
      }
  }

  final case class SingleEnvConfig[A](varName: EnvVar[String], decoder: oxygen.json.JsonDecoder[A]) extends EnvVarConfig[A] {
    override protected def missingMessage: String = varName match
      case SingleEnvVar(name, _) => s"Environment variable $name is not set"
      case _                     => "Required config environment variable is not set"
    override def read: IO[String, Option[A]] =
      varName.read.flatMap {
        case None          => ZIO.none
        case Some(rawPath) => ZIO.fromEither(loadConfig(decoder, rawPath)).asSome
      }
  }

  final case class OptionalEnvConfig[A](inner: EnvVarConfig[A]) extends EnvVarConfig[Option[A]] {
    override def read: IO[String, Option[Option[A]]] = inner.read.asSome
  }

  final case class DefaultEnvConfig[A](inner: EnvVarConfig[A], default: A) extends EnvVarConfig[A] {
    override def read: IO[String, Option[A]] = inner.read.map(opt => opt.getOrElse(default).some)
  }

}
