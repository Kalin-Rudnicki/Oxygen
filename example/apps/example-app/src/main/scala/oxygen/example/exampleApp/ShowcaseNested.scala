package oxygen.example.exampleApp

import oxygen.cli.*
import oxygen.executable.*
import oxygen.schema.instances.jsonCodecFromSchema
import zio.*

/** One level deep — several sibling subcommands with different param styles. */
final case class CatalogApp() extends CliApp[RootCtx, Any] derives CompiledCliApp.DeriveSubApp {

  @command
  def list(
      @named @shortName('f') format: OutputFormat = OutputFormat.Text,
      @named limit: Option[Int],
      @flag @longName("show-all") showAll: Boolean = false,
  ): Effect =
    ZIO.logInfo(s"catalog list format=$format limit=$limit showAll=$showAll")

  @command("describe")
  def describeItem(
      @positional id: String,
      @named @shortName('v') verbose: Boolean = false,
  ): Effect =
    ZIO.logInfo(s"catalog describe id=$id verbose=$verbose")

  @command
  def paint(
      @custom color: RgbColor,
      @named label: Option[String],
  ): Effect =
    ZIO.logInfo(s"catalog paint color=$color label=$label")

}

/** Nested `def env` — provides [[WorkspaceCtx]] on top of [[RootCtx]]. */
final case class WorkspaceApp() extends CliApp[RootCtx, WorkspaceCtx] derives CompiledCliApp.DeriveSubApp {

  def env(
      @named @shortName('w') workspaceId: String,
      @toggle
      @longName.trueName("online")
      @longName.falseName("offline")
      @shortName.trueName('O')
      @shortName.falseName('o')
      connected: Boolean,
  ): EnvLayer =
    ZLayer.succeed(WorkspaceCtx(workspaceId, connected))

  @command
  def sync(
      @named paths: List[String],
      @flag @longName("dry-run") dryRun: Boolean = true,
  ): Effect =
    for {
      root <- ZIO.service[RootCtx]
      ws <- ZIO.service[WorkspaceCtx]
      _ <- ZIO.logInfo(s"workspace sync host=${root.host} ws=$ws paths=$paths dryRun=$dryRun")
    } yield ()

  @command
  def deploy(
      @envConfig("TASK_CONFIG") task: TaskConfig,
      @named @shortName('p') priority: Priority = Priority.Normal,
  ): Effect =
    ZIO.serviceWithZIO[WorkspaceCtx] { ws =>
      ZIO.logInfo(s"workspace deploy ws=$ws task=$task priority=$priority")
    }

}

/** Four levels deep: `showcase deep level down bottom …` */
final case class DeepL1() extends CliApp[RootCtx, Any] derives CompiledCliApp.DeriveSubApp {

  @command
  def level: DeepL2 = DeepL2()

}

final case class DeepL2() extends CliApp[RootCtx, Any] derives CompiledCliApp.DeriveSubApp {

  @command
  def down: DeepL3 = DeepL3()

}

final case class DeepL3() extends CliApp[RootCtx, Any] derives CompiledCliApp.DeriveSubApp {

  @command
  def bottom: DeepL4 = DeepL4()

}

final case class DeepL4() extends CliApp[RootCtx, Any] derives CompiledCliApp.DeriveSubApp {

  @execute
  def run(
      @named @shortName('t') token: String,
      @toggle
      @longName.truePrefix("enable")
      @longName.falsePrefix("disable")
      @doc("Toggle tracing", "for this leaf command")
      tracing: Boolean,
      @named repeats: List[Int] = Nil,
      @positional note: Option[String],
  ): Effect =
    ZIO.serviceWithZIO[RootCtx] { root =>
      ZIO.logInfo(s"deep leaf host=${root.host} token=$token tracing=$tracing repeats=$repeats note=$note")
    }

}

/** Constructor param passed into a nested app (`showcase capsule …`). */
final case class CapsuleApp(
    seed: Int,
) extends CliApp[RootCtx, Any] derives CompiledCliApp.DeriveSubApp {

  @execute
  def run(
      @named label: String = "default",
      @flag @longName("sealed") isSealed: Boolean = false,
  ): Effect =
    ZIO.logInfo(s"capsule seed=$seed label=$label sealed=$isSealed")

}
