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

/**
  * Non-case-class sub-app (`showcase gadget …`): a plain `trait` with an *abstract* `@execute`. A sub-app
  * is never instantiated by the derivation macro — the parent `@command` supplies the concrete instance —
  * so it needn't be a case class. Only runnable *roots* must be an instantiable case class / object.
  */
trait GadgetApp extends CliApp[RootCtx, Any] derives CompiledCliApp.DeriveSubApp {
  @execute
  def run(
      @named @shortName('n') name: String,
      @flag loud: Boolean = false,
  ): Effect
}

/**
  * Non-case-class sub-app that also has an env layer (`showcase widget …`): a `trait` requiring `RootCtx`
  * and providing `GizmoCtx` via a (concrete) `def env`, with an *abstract* `@execute`. The env params are
  * parsed and the layer prepended just like a case-class app; only the effect body comes from the parent.
  */
trait WidgetApp extends CliApp[RootCtx, GizmoCtx] derives CompiledCliApp.DeriveSubApp {

  def env(
      @named @shortName('g') gizmoId: String,
      @flag calibrated: Boolean = false,
  ): EnvLayer =
    ZLayer.succeed(GizmoCtx(gizmoId, calibrated))

  @execute
  def run(@named @shortName('p') power: Int = 1): Effect

}

/**
  * Like [[WidgetApp]], but the env layer itself is *abstract* (`showcase contraption …`): the trait only
  * declares `def env(...): EnvLayer` (with its CLI param annotations) and leaves both `env` and `@execute`
  * for the parent to implement. The derivation reads the env params off the abstract declaration and calls
  * the (parent-supplied) `env` at runtime — so the child decides how the parsed params become a layer.
  */
trait ContraptionApp extends CliApp[RootCtx, GizmoCtx] derives CompiledCliApp.DeriveSubApp {

  def env(
      @named @shortName('g') gizmoId: String,
      @flag calibrated: Boolean = false,
  ): EnvLayer

  @execute
  def run(@named @shortName('p') power: Int = 1): Effect

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
