package oxygen.predef

// TODO (KR) : dont export zio things here, only do that in predef.test
// TODO (KR) : export a few extra types there
object zio {
  export _root_.zio.{Cause, Chunk, EnvironmentTag, FiberRef, LogLevel, LogSpan, Ref, Runtime, Schedule, Scope, StackTrace, Tag, Trace, ZIOAspect}
  export _root_.zio.{Clock, Console, Random, System}
  export _root_.zio.{durationInt, durationLong, DurationOps, DurationSyntax}
  export _root_.zio.{IO, RIO, Task, UIO, URIO, ZIO}
  export _root_.zio.{Layer, RLayer, TaskLayer, ULayer, URLayer, ZLayer}
  export oxygen.zio.{GlobalLayer, GlobalMemoizedEffect, ZIOAspectAtLeastR, ZIOAspectPoly}
  export oxygen.zio.instances.given
  export oxygen.zio.logging.LogLevels
  export oxygen.zio.syntax.log.*
  export oxygen.zio.syntax.seq.*
  export oxygen.zio.system.Command
}
