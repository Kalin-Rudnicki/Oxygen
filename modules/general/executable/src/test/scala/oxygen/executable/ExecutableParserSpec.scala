package oxygen.executable

import oxygen.cli.*
import oxygen.predef.test.*
import oxygen.schema.PlainTextSchema
import zio.*

object ExecutableParserSpec extends OxygenSpecDefault {

  // An env var name that is overwhelmingly unlikely to be set in any environment running this test.
  private val unsetVar: String = "OXYGEN_EXECUTABLE_PARSER2_SPEC__DEFINITELY_UNSET"
  private val stringSchema: oxygen.schema.AnySchemaT[String] = summon[PlainTextSchema[String]]

  // A trivial leaf app that just returns a fixed exit code, so we can exercise `execute`.
  private def fixedExitApp(code: Int): CompiledCliApp.Effect[Unit, Unit, Any] =
    new CompiledCliApp.Effect[Unit, Unit, Any] {
      override lazy val effectParser: ExecutableParser[Unit] = ExecutableParser.Empty
      override def effect(instances: Unit, effectArgs: Unit): ZIO[FullR, ExecutableError, ExitCode] =
        ZIO.succeed(ExitCode(code))
    }

  override def testSpec: TestSpec =
    suite("ExecutableParserSpec")(
      suite("env loading is effectful")(
        test("required env var, unset -> effect fails with a message") {
          val parser = NonCLIExecutableParser.SingleEnvVar(unsetVar, stringSchema)
          parser.load.either.map { result =>
            assertTrue(result == Left(s"Required environment variable [$unsetVar] is not set"))
          }
        },
        test("optional env var, unset -> effect succeeds with None") {
          val parser = NonCLIExecutableParser.OptionalEnvVar(NonCLIExecutableParser.SingleEnvVar(unsetVar, stringSchema))
          parser.load.map(result => assertTrue(result == None))
        },
        test("default env var, unset -> effect succeeds with the default") {
          val parser = NonCLIExecutableParser.DefaultEnvVar(NonCLIExecutableParser.SingleEnvVar(unsetVar, stringSchema), "fallback")
          parser.load.map(result => assertTrue(result == "fallback"))
        },
      ),
      suite("execute")(
        test("leaf app runs its effect and returns the exit code") {
          ZIO.scoped(fixedExitApp(42).execute((), Args.empty)).map(ec => assertTrue(ec == ExitCode(42)))
        },
        test("sub-commands dispatch on the leading positional") {
          val app: CompiledCliApp[Unit, Any] =
            new CompiledCliApp.SubCommands[Unit, Any] {
              override val subCommands: Seq[(String, Lazy[CompiledCliApp[Unit, Any]])] =
                Seq("go" -> Lazy(fixedExitApp(7)))
            }
          ZIO.scoped(app.execute((), Args(PositionalArgs(PositionalArg(0, "go") :: Nil), NamedArgs.empty)))
            .map(ec => assertTrue(ec == ExitCode(7)))
        },
        test("unknown sub-command exits with usage-error code") {
          val app: CompiledCliApp[Unit, Any] =
            new CompiledCliApp.SubCommands[Unit, Any] {
              override val subCommands: Seq[(String, Lazy[CompiledCliApp[Unit, Any]])] =
                Seq("go" -> Lazy(fixedExitApp(7)))
            }
          ZIO.scoped(app.execute((), Args(PositionalArgs(PositionalArg(0, "nope") :: Nil), NamedArgs.empty)))
            .map(ec => assertTrue(ec == ExecutableError.usageErrorExitCode))
        },
      ),
    )

}
