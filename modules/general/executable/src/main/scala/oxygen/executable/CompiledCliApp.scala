package oxygen.executable

import oxygen.cli.*
import oxygen.predef.core.*
import zio.*

sealed trait CompiledCliApp[In, RIn] {

  // Parse `args`, build the instance(s) + layer, and run the effect. Parse failures render `Help`
  // and exit non-zero; the error channel carries only genuine runtime (`ExecutableError`) failures.
  def execute(in: In, args: Args): ZIO[RIn & Scope, ExecutableError, ExitCode]

  // Tab-completion entry point: given a `CompletionRequest` (the typed words + cursor), what completions apply?
  def complete(request: CompletionRequest): Task[List[String]]

  // Help for the leaf command(s) reachable from `remaining`, each titled with its full path (`prefix`
  // extended by the commands taken to reach it). Intermediate command groups never appear — only leaves,
  // so a four-deep chain shows a single `[Command] deep level down bottom`, not one entry per level.
  // `helpFor` shows full help for the drilled-to command (title + @doc + args). When `remaining` lands on a
  // group of sub-commands, `helpType` decides the listing form: `--help` (`summaryAt`) shows each leaf's
  // name+path + @doc only; `--help-extra` (`detailedAt`) additionally expands each leaf's args.
  def helpFor(prefix: List[String], remaining: List[String], helpType: HelpType): Help
  def summaryAt(prefix: List[String]): Help
  def detailedAt(prefix: List[String]): Help
  final def help: Help = helpFor(Nil, Nil, HelpType.Help)

  // Machine-readable help (one object per leaf command) for `OXYGEN_CLI_JSON=true`.
  def helpJsonCommands(prefix: List[String], remaining: List[String]): List[oxygen.json.Json]
  final def helpJson(prefix: List[String], remaining: List[String]): oxygen.json.Json =
    oxygen.json.Json.arr(helpJsonCommands(prefix, remaining)*)

  def select[In2, _InstancesArgs](
      parser: ExecutableParser[_InstancesArgs],
  )(
      make: (In2, _InstancesArgs) => In,
  ): CompiledCliApp[In2, RIn]

  def prependLayer[_LayerArgs, RIn2, ProvidedR: EnvironmentTag](
      parser: ExecutableParser[_LayerArgs],
  )(
      make: (In, _LayerArgs) => ZLayer[RIn2, ExecutableError, ProvidedR],
  )(using ev: (RIn2 & ProvidedR) =:= RIn): CompiledCliApp[In, RIn2]

}
object CompiledCliApp {

  // Parse failures are not part of the effect's error channel: they're rendered as help text and
  // mapped to a failing exit code, mirroring the old runtime's `printHelp`.
  private def renderParseFail(fail: CliParseResult.Fail): UIO[ExitCode] =
    Console.printLine(fail.help.toString).orDie.as(ExecutableError.usageErrorExitCode)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Single[In, RIn] extends CompiledCliApp[In, RIn] { self =>

    type InstancesArgs
    type LayerArgs
    type EffectArgs

    type Instances

    type ROut

    final type FullR = RIn & ROut

    lazy val instancesParser: ExecutableParser[InstancesArgs]
    lazy val layerParser: ExecutableParser[LayerArgs]
    lazy val effectParser: ExecutableParser[EffectArgs]

    // `.resolveAutoShortNames` runs the global two-phase short-name pass over the whole command's named
    // args (instances + layer + effect) exactly once, so two params can't silently both claim `-x`.
    final lazy val fullParser: ExecutableParser[(InstancesArgs, LayerArgs, EffectArgs)] =
      (instancesParser ^>>&&## layerParser ^>>&&## effectParser).resolveAutoShortNames

    def instances(in: In, instancesArgs: InstancesArgs): Instances
    def effect(instances: Instances, effectArgs: EffectArgs): ZIO[FullR, ExecutableError, ExitCode]

    // Provide every accumulated layer to `effect`, recursively, one `prependLayer` at a time. Each
    // `PrependLayer` provides only its own `ProvidedR` (which has a concrete `EnvironmentTag` via its
    // context bound), so the accumulated `ROut` type (an abstract `inner.ROut & ProvidedR` intersection)
    // is never reflected into a tag — which is what produced a phantom-service `ZEnvironment` defect.
    def provideAll(effect: ZIO[FullR, ExecutableError, ExitCode], instances: Instances, layerArgs: LayerArgs): ZIO[RIn, ExecutableError, ExitCode]

    override final def complete(request: CompletionRequest): Task[List[String]] =
      fullParser.complete(request, request.currentValue)

    // Doc lines from the method's `@doc` annotation (set by the derivation); shown under the title.
    def commandDocs: List[String] = Nil

    // A leaf command always shows its full help (title + @doc + args); `helpType` only matters at a group.
    override final def helpFor(prefix: List[String], remaining: List[String], @scala.annotation.unused helpType: HelpType): Help =
      val body: Help = commandDocs match
        case Nil => fullParser.help
        case ds  => Help.And(Help.Raw(ds.mkString("\n")), fullParser.help)
      if prefix.isEmpty then body
      else Help.And(Help.CommandTitle(prefix.mkString(" ")), body)

    // Listing form: title + @doc only, no args.
    override final def summaryAt(prefix: List[String]): Help =
      val docHelp: Help = commandDocs match
        case Nil => Help.Empty
        case ds  => Help.Raw(ds.mkString("\n"))
      if prefix.isEmpty then docHelp
      else Help.And(Help.CommandTitle(prefix.mkString(" ")), docHelp)

    // Detailed form: the full leaf help (title + @doc + args).
    override final def detailedAt(prefix: List[String]): Help = helpFor(prefix, Nil, HelpType.Help)

    override final def helpJsonCommands(prefix: List[String], remaining: List[String]): List[oxygen.json.Json] = {
      val fields: List[(String, oxygen.json.Json)] =
        List("command" -> oxygen.json.Json.string(prefix.mkString(" "))) :::
          (if commandDocs.nonEmpty then List("doc" -> oxygen.json.Json.arr(commandDocs.map(oxygen.json.Json.string)*)) else Nil) :::
          List("params" -> CliHelp.paramsJson(fullParser.help))
      List(oxygen.json.Json.obj(fields*))
    }

    override final def execute(in: In, args: Args): ZIO[RIn & Scope, ExecutableError, ExitCode] =
      fullParser.parse(args).foldZIO(
        CompiledCliApp.renderParseFail,
        { case (instancesArgs, layerArgs, effectArgs) =>
          val theInstances: Instances = instances(in, instancesArgs)
          provideAll(effect(theInstances, effectArgs), theInstances, layerArgs)
        },
      )

    override final def select[In2, _InstancesArgs](parser: ExecutableParser[_InstancesArgs])(make: (In2, _InstancesArgs) => In): CompiledCliApp[In2, RIn] =
      new CompiledCliApp.Select[In2, _InstancesArgs, In, RIn] {
        override val inner: CompiledCliApp.Single[In, RIn] = self
        override val newInstancesParser: ExecutableParser[_InstancesArgs] = parser
        override val makeChild: (In2, _InstancesArgs) => In = make
      }

    override final def prependLayer[_LayerArgs, RIn2, ProvidedR: EnvironmentTag](
        parser: ExecutableParser[_LayerArgs],
    )(
        make: (In, _LayerArgs) => ZLayer[RIn2, ExecutableError, ProvidedR],
    )(using ev: (RIn2 & ProvidedR) =:= RIn): CompiledCliApp[In, RIn2] =
      new CompiledCliApp.PrependLayer[In, _LayerArgs, RIn2, ProvidedR] {
        // TODO (KR) : do I have the types right here? I think its missing some constraints
        override val inner: CompiledCliApp.Single[In, RIn2 & ProvidedR] = self.asInstanceOf[CompiledCliApp.Single[In, RIn2 & ProvidedR]]
        override val newLayerParser: ExecutableParser[_LayerArgs] = parser
        override val newLayer: (In, _LayerArgs) => ZLayer[RIn2, ExecutableError, ProvidedR] = make
      }

  }

  abstract class Effect[Klass, _EffectArgs, R] extends CompiledCliApp.Single[Klass, R] {

    override final type InstancesArgs = Unit
    override final type LayerArgs = Unit
    override final type EffectArgs = _EffectArgs
    override final type Instances = Klass
    override final type ROut = Any

    override final lazy val instancesParser: ExecutableParser[InstancesArgs] = ExecutableParser.Empty
    override final lazy val layerParser: ExecutableParser[LayerArgs] = ExecutableParser.Empty

    override final def instances(in: Klass, instancesArgs: InstancesArgs): Instances = in
    override final def provideAll(effect: ZIO[FullR, ExecutableError, ExitCode], instances: Instances, layerArgs: LayerArgs): ZIO[R, ExecutableError, ExitCode] =
      effect // FullR = R & Any =:= R; a leaf adds no layer

  }

  abstract class PrependLayer[Klass, _LayerArgs, RIn2, ProvidedR: EnvironmentTag] extends CompiledCliApp.Single[Klass, RIn2] {

    val inner: CompiledCliApp.Single[Klass, RIn2 & ProvidedR]
    val newLayerParser: ExecutableParser[_LayerArgs]
    val newLayer: (Klass, _LayerArgs) => ZLayer[RIn2, ExecutableError, ProvidedR]

    override final type InstancesArgs = inner.InstancesArgs
    override final type LayerArgs = (_LayerArgs, inner.LayerArgs)
    override final type EffectArgs = inner.EffectArgs
    override final type Instances = (Klass, inner.Instances)
    override final type ROut = inner.ROut & ProvidedR

    override final lazy val instancesParser: ExecutableParser[InstancesArgs] = inner.instancesParser
    override final lazy val layerParser: ExecutableParser[LayerArgs] = newLayerParser ^>>&&## inner.layerParser
    override final lazy val effectParser: ExecutableParser[EffectArgs] = inner.effectParser

    override final def commandDocs: List[String] = inner.commandDocs
    override final def instances(in: Klass, instancesArgs: InstancesArgs): Instances =
      (in, inner.instances(in, instancesArgs))
    override final def effect(instances: Instances, effectArgs: EffectArgs): ZIO[FullR, ExecutableError, ExitCode] =
      inner.effect(instances._2, effectArgs)
    override final def provideAll(effect: ZIO[FullR, ExecutableError, ExitCode], instances: Instances, layerArgs: LayerArgs): ZIO[RIn2, ExecutableError, ExitCode] =
      // Provide inner's layers first (leaving `RIn2 & ProvidedR`), then provide this layer's `ProvidedR`.
      inner.provideAll(effect, instances._2, layerArgs._2).provideSomeLayer[RIn2](newLayer(instances._1, layerArgs._1))

  }

  abstract class Select[Klass, _InstancesArgs, Klass2, RIn] extends CompiledCliApp.Single[Klass, RIn] {

    val inner: CompiledCliApp.Single[Klass2, RIn]
    val newInstancesParser: ExecutableParser[_InstancesArgs]
    val makeChild: (Klass, _InstancesArgs) => Klass2

    override final type InstancesArgs = (_InstancesArgs, inner.InstancesArgs)
    override final type LayerArgs = inner.LayerArgs
    override final type EffectArgs = inner.EffectArgs
    override final type Instances = (Klass, inner.Instances)
    override final type ROut = inner.ROut

    override final lazy val instancesParser: ExecutableParser[InstancesArgs] = newInstancesParser ^>>&&## inner.instancesParser
    override final lazy val layerParser: ExecutableParser[LayerArgs] = inner.layerParser
    override final lazy val effectParser: ExecutableParser[EffectArgs] = inner.effectParser

    override final def commandDocs: List[String] = inner.commandDocs
    override final def instances(in: Klass, instancesArgs: InstancesArgs): Instances =
      (in, inner.instances(makeChild(in, instancesArgs._1), instancesArgs._2))
    override final def effect(instances: Instances, effectArgs: EffectArgs): ZIO[FullR, ExecutableError, ExitCode] =
      inner.effect(instances._2, effectArgs)
    override final def provideAll(effect: ZIO[FullR, ExecutableError, ExitCode], instances: Instances, layerArgs: LayerArgs): ZIO[RIn, ExecutableError, ExitCode] =
      inner.provideAll(effect, instances._2, layerArgs) // Select adds no layer

  }

  abstract class SubCommands[In, RIn] extends CompiledCliApp[In, RIn] { self =>

    val subCommands: Seq[(String, Lazy[CompiledCliApp[In, RIn]])]

    final lazy val subCommandMap: Map[String, Lazy[CompiledCliApp[In, RIn]]] = subCommands.toMap

    private def knownCommands: String = subCommands.map(_._1).sorted.mkString(", ")
    private def commandNames: List[String] = subCommands.map(_._1).sorted.toList

    override final def complete(request: CompletionRequest): Task[List[String]] =
      if request.argIdx <= 0 then
        // completing the command token itself — offer command names plus the built-in flags
        // (`--help` / `--help-extra`), which apply at a sub-command position too.
        val value = request.currentValue
        if value.startsWith("-") then ZIO.succeed(CliHelp.builtinFlagCompletions(value))
        else
          val matches = commandNames.filter(_.startsWith(value))
          val cmds = if matches.nonEmpty then matches else commandNames
          ZIO.succeed(CliHelp.mergeCompletions(CliHelp.builtinFlagCompletions(value), cmds))
      else
        // drill into the named sub-command, shifting the request past it
        request.args.headOption.flatMap(subCommandMap.get) match
          case Some(child) => child.value.complete(CompletionRequest(request.numWords - 1, request.argIdx - 1, request.args.drop(1), request.joinStr))
          case None        => ZIO.succeed(commandNames)

    // Drill into the named sub-command if `remaining` points at one; otherwise list every leaf reachable
    // from here, each with its full path. Intermediate groups contribute no entry of their own. At a group,
    // `--help` lists leaves as summaries (no args); `--help-extra` expands each leaf's args.
    override final def helpFor(prefix: List[String], remaining: List[String], helpType: HelpType): Help =
      remaining match
        case head :: tail => subCommandMap.get(head).fold(groupHelp(prefix, helpType))(_.value.helpFor(prefix :+ head, tail, helpType))
        case Nil          => groupHelp(prefix, helpType)

    private def groupHelp(prefix: List[String], helpType: HelpType): Help = helpType match
      case HelpType.Help      => summaryAt(prefix)
      case HelpType.HelpExtra => detailedAt(prefix)

    override final def summaryAt(prefix: List[String]): Help =
      joinBlocks(sortedChildren.map { case (name, child) => child.value.summaryAt(prefix :+ name) })

    override final def detailedAt(prefix: List[String]): Help =
      joinBlocks(sortedChildren.map { case (name, child) => child.value.detailedAt(prefix :+ name) })

    private def sortedChildren: List[(String, Lazy[CompiledCliApp[In, RIn]])] = subCommands.toList.sortBy(_._1)

    private def joinBlocks(blocks: List[Help]): Help = blocks match
      case Nil          => Help.Empty
      case head :: tail => tail.foldLeft(head) { (acc, h) => Help.And(Help.And(acc, Help.BlankLine), h) }

    override final def helpJsonCommands(prefix: List[String], remaining: List[String]): List[oxygen.json.Json] =
      remaining match
        case head :: tail => subCommandMap.get(head).fold(allLeavesJson(prefix))(_.value.helpJsonCommands(prefix :+ head, tail))
        case Nil          => allLeavesJson(prefix)

    private def allLeavesJson(prefix: List[String]): List[oxygen.json.Json] =
      subCommands.toList.sortBy(_._1).flatMap { case (name, child) => child.value.helpJsonCommands(prefix :+ name, Nil) }

    override final def execute(in: In, args: Args): ZIO[RIn & Scope, ExecutableError, ExitCode] =
      args.positional.args match
        case pHead :: pTail =>
          subCommandMap.get(pHead.value) match
            case Some(child) => child.value.execute(in, Args(PositionalArgs(pTail), args.named))
            case None        => Console.printLine(s"Unknown command '${pHead.value}'. Known commands: $knownCommands").orDie.as(ExecutableError.usageErrorExitCode)
        case Nil =>
          Console.printLine(s"Missing command. Known commands: $knownCommands").orDie.as(ExecutableError.usageErrorExitCode)

    override final def select[In2, _InstancesArgs](
        parser: ExecutableParser[_InstancesArgs],
    )(
        make: (In2, _InstancesArgs) => In,
    ): CompiledCliApp[In2, RIn] =
      new CompiledCliApp.SubCommands[In2, RIn] {
        override val subCommands: Seq[(String, Lazy[CompiledCliApp[In2, RIn]])] =
          self.subCommands.map { (name, child) => (name, Lazy { child.value.select(parser)(make) }) }
      }

    override final def prependLayer[_LayerArgs, RIn2, ProvidedR: EnvironmentTag](
        parser: ExecutableParser[_LayerArgs],
    )(
        make: (In, _LayerArgs) => ZLayer[RIn2, ExecutableError, ProvidedR],
    )(using ev: (RIn2 & ProvidedR) =:= RIn): CompiledCliApp[In, RIn2] =
      new CompiledCliApp.SubCommands[In, RIn2] {
        override val subCommands: Seq[(String, Lazy[CompiledCliApp[In, RIn2]])] =
          self.subCommands.map { (name, child) => (name, Lazy { child.value.prependLayer(parser)(make) }) }
      }

  }

}
