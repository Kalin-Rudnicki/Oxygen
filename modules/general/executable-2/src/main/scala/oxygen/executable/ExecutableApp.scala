package oxygen.executable

import oxygen.meta.k0.ZeroArgClass

abstract class ExecutableApp {

  protected def app: CompiledCliApp[Any]

}
object ExecutableApp {

  abstract class Derived[A](using derive: DeriveCliApp[Any, A], klass: ZeroArgClass[A]) extends ExecutableApp {
    override protected final def app: CompiledCliApp[Any] = derive.app(klass.instantiate)
  }

}
