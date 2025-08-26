package oxygen.ui.web.internal

import zio.*

final class UIRuntime[+Env: HasNoScope] private (runtime: Runtime[Env]) {

  def execute(effect: URIO[Env, Unit]): UIO[Unit] =
    runtime.run { effect.catchAllCause { ZIO.logErrorCause("Uncaught error made it to UIRuntime", _) } }

  def unsafeExecute(effect: URIO[Env, Unit])(using Unsafe): Unit =
    try {
      runtime.unsafe.run { effect.catchAllCause { ZIO.logErrorCause("Uncaught error made it to UIRuntime", _) } }
    } catch {
      case e if e.getMessage.contains("block") =>
    }

}
object UIRuntime {
  private[web] def make[Env: HasNoScope]: URIO[Env, UIRuntime[Env]] = ZIO.runtime[Env].map(UIRuntime(_))
}
