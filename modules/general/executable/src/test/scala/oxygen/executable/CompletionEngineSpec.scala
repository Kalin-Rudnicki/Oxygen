package oxygen.executable

import oxygen.cli.*
import oxygen.predef.test.*
import zio.*

object CompletionEngineSpec extends OxygenSpecDefault {

  private val flagsParser: ArgsParser[?] = ArgsParser.helpOnly(Help.Flag("verbose", None, SubHelp.Empty))

  private val client: CompiledCliApp[Any] =
    CompiledCliApp.Impl(
      rootParser = flagsParser,
      helpParser = flagsParser,
      subCommands = Map.empty,
      runFn = _ => ZIO.succeed(ExitCode.success),
    )

  private val seedParser: ArgsParser[?] =
    ArgsParser.helpOnly(Help.Named("seed", Some('s'), Help.Empty, SubHelp.Empty))

  private val seedAndEnvParser: ArgsParser[?] =
    ArgsParser.helpOnly(Help.Named("shard", None, Help.Empty, SubHelp.Empty)) ^>> seedParser

  private val capsuleLike: CompiledCliApp[Any] =
    CompiledCliApp.Impl(
      rootParser = seedParser,
      helpParser = seedAndEnvParser,
      subCommands = Map.empty,
      runFn = _ => ZIO.succeed(ExitCode.success),
    )

  private val leaf: CompiledCliApp[Any] =
    CompiledCliApp.Impl(
      rootParser = ArgsParser.unit,
      helpParser = ArgsParser.unit,
      subCommands = Map.empty,
      runFn = _ => ZIO.succeed(ExitCode.success),
    )

  private val deepL2: CompiledCliApp[Any] =
    CompiledCliApp.Impl(
      rootParser = ArgsParser.unit,
      helpParser = ArgsParser.unit,
      subCommands = Map("down" -> leaf),
      runFn = _ => ZIO.succeed(ExitCode.success),
    )

  private val deepL1: CompiledCliApp[Any] =
    CompiledCliApp.Impl(
      rootParser = ArgsParser.unit,
      helpParser = ArgsParser.unit,
      subCommands = Map("level" -> deepL2),
      runFn = _ => ZIO.succeed(ExitCode.success),
    )

  private val root: CompiledCliApp[Any] =
    CompiledCliApp.Impl(
      rootParser = ArgsParser.helpOnly(Help.Flag("my-opt", None, SubHelp.Empty)),
      helpParser = ArgsParser.helpOnly(Help.Flag("my-opt", None, SubHelp.Empty)),
      subCommands = Map("client" -> client, "server" -> client, "deep" -> deepL1, "capsule" -> capsuleLike),
      runFn = _ => ZIO.succeed(ExitCode.success),
    )

  private def complete(app: CompiledCliApp[Any], args: List[String], argIdx: Int): Task[List[String]] =
    CompletionEngine.complete(app, CompletionRequest(args.length, argIdx, args, "\n"))

  override def testSpec: TestSpec =
    suite("CompletionEngineSpec")(
      test("offers subcommands and builtins at empty root") {
        complete(root, Nil, 0).map { out =>
          assertTrue(
            out.contains("client"),
            out.contains("server"),
            out.contains("--help"),
          )
        }
      },
      test("filters subcommands by prefix") {
        complete(root, List("ser"), 0).map { out =>
          assertTrue(out == List("server"))
        }
      },
      test("drills into exact subcommand match") {
        complete(root, List("client"), 0).map { out =>
          assertTrue(out.contains("--verbose"), out.contains("--help"))
        }
      },
      test("completes flags inside a selected subcommand") {
        complete(root, List("server", "--ver"), 1).map { out =>
          assertTrue(out == List("--verbose"))
        }
      },
      test("offers nested subcommands after a selected parent subcommand") {
        complete(root, List("deep", ""), 1).map { out =>
          assertTrue(out.contains("level"), out.contains("--help"))
        }
      },
      test("walks multiple nested subcommand levels") {
        complete(root, List("deep", "level", ""), 2).map { out =>
          assertTrue(out.contains("down"))
        }
      },
      test("completes help-only flags after a nested subcommand is selected") {
        complete(root, List("capsule", "--sha"), 1).map { out =>
          assertTrue(out == List("--shard"))
        }
      },
      test("offers env and command flags at an empty cursor inside a nested subcommand") {
        complete(root, List("capsule", ""), 1).map { out =>
          assertTrue(out.contains("--shard"), out.contains("--seed"))
        }
      },
    )

}
