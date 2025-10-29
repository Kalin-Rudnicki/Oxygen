package oxygen.storage

import oxygen.predef.zio.*

trait Atomically extends ZIOAspectPoly.Impl {

  def atomicallyScoped: URIO[Scope, Unit]
  def atomically[R, E, A](effect: ZIO[R, E, A]): ZIO[R, E, A]

  override final def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] = atomically(effect)

}
object Atomically {

  val atomically: ZIOAspectAtLeastR[Atomically] =
    new ZIOAspectAtLeastR.Impl[Atomically] {
      override def apply[R <: Atomically, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
        ZIO.serviceWithZIO[Atomically](_.atomically(effect))
    }

  final class NoOp extends Atomically {
    override def atomicallyScoped: URIO[Scope, Unit] = ZIO.unit
    override def atomically[R, E, A](effect: ZIO[R, E, A]): ZIO[R, E, A] = effect
  }
  object NoOp {

    val layer: ULayer[Atomically] =
      ZLayer.succeed { new NoOp }

  }

}
