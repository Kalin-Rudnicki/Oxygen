package oxygen.example.exampleApp

import oxygen.cli.*
import oxygen.executable.*
import oxygen.predef.core.*
import oxygen.schema.instances.jsonCodecFromSchema
import zio.*

/**
  * CLI feature showcase — flat commands, one-level groups, four-level nesting,
  * and a wide mix of parameter annotations/types.
  */
final case class ShowcaseApp() extends CliApp[Any, RootCtx] {

  @doc("Root environment", "shared by every subcommand")
  def env(
      @envConfig("SHOWCASE_SETTINGS") settings: Option[ShowcaseSettings],
      @named host: String = "localhost",
      @toggle
      @longName.truePrefix("enable")
      @longName.falsePrefix("disable")
      @shortName.trueName('M')
      @shortName.falseName('m')
      metrics: Boolean,
      @named shard: Int = 0,
      @named @shortName('T') tags: List[String] = Nil,
      @named @shortName('q') quiet: Option[String],
  ): EnvLayer =
    ZLayer.succeed(RootCtx(host, metrics, shard, settings, tags, quiet))

  // ── flat effect commands ──────────────────────────────────────────────

  @doc("Minimal flat command")
  @command
  def ping(
      @positional message: String,
      @named @shortName('n') count: Int = 1,
  ): Effect =
    ZIO.replicateZIO(count)(ZIO.logInfo(s"ping: $message")).unit

  @command("kitchen-sink")
  def kitchenSink(
      @envConfig("TASK_CONFIG") task: TaskConfig,
      @envConfig("OPTIONAL_TASK") extra: Option[TaskConfig],
      @named @shortName('i') ids: NonEmptyList[String],
      @named pair: (String, Int),
      @named batches: List[(Int, String)],
      @named priority: Priority = Priority.Normal,
      @flag @longName("verbose") verbose: Boolean = false,
      @flag @longName("dry-run") dryRun: Boolean = true,
      @custom accent: RgbColor,
      @positional tail: Option[String],
  ): Effect =
    ZIO.serviceWithZIO[RootCtx] { root =>
      ZIO.logInfo(
        s"kitchen-sink root=$root " +
          s"task=$task extra=$extra ids=$ids pair=$pair batches=$batches priority=$priority " +
          s"verbose=$verbose dryRun=$dryRun accent=$accent tail=$tail",
      )
    }

  @command("kitchen-sink-writes")
  def kitchenSinkWrites(
      @toggle
      @longName.trueName("allow-writes")
      @longName.falseName("deny-writes")
      writes: Boolean,
  ): Effect =
    ZIO.logInfo(s"kitchen-sink-writes writes=$writes")

  @command
  def echo(
      @positional @longName("line") lines: List[String],
      @named @shortName('f') format: OutputFormat = OutputFormat.Text,
  ): Effect =
    ZIO.logInfo(s"echo format=$format lines=${lines.mkString(" | ")}")

  @doc("Fails via ZIO.fail (becomes a defect through runEffect.orDie)")
  @command("fail-demo")
  def failDemo(): Effect =
    ZIO.fail(new RuntimeException("intentional fail"))

  @doc("Dies via ZIO.die")
  @command("die-demo")
  def dieDemo(): Effect =
    ZIO.die(new RuntimeException("intentional die"))

  @doc("Pick a config file", "tab-completes under example/apps/example-app/configs (run from repo root)")
  @command("pick-config")
  def pickConfig(
      @named @longName("file") file: GlobalPath["example/apps/example-app/configs"],
  ): Effect =
    ZIO.logInfo(s"pick-config file=$file")

  // ── one level deep ────────────────────────────────────────────────────

  @command
  def catalog: CatalogApp = CatalogApp()

  @command("workspace")
  def openWorkspace: WorkspaceApp = WorkspaceApp()

  // ── four levels deep ──────────────────────────────────────────────────

  @command
  def deep: DeepL1 = DeepL1()

  // ── nested app with ctor param + @execute ─────────────────────────────

  @command
  def capsule(
      @named @shortName('s') seed: Int,
  ): CapsuleApp = CapsuleApp(seed)

}
object ShowcaseApp extends CliApp.Executable[ShowcaseApp](CliApp.derive)
