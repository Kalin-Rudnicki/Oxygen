package oxygen.storage

import oxygen.predef.zio.*
import zio.*

trait Atomically extends ZIOAspectPoly.Impl {

  def atomicallyScoped: URIO[Scope, Unit]
  def atomically[R, E, A](effect: ZIO[R, E, A]): ZIO[R, E, A]

  /**
    * Weaker sibling of [[atomicallyScoped]] : ensures the surrounding effect runs inside *a* transaction,
    * but does not add a nested savepoint when one is already open.
    *
    * When executed outside any other `atomically`/`ensureAtomic` block, this opens a real transaction
    * (identical to [[atomicallyScoped]]'s outermost behavior).
    *
    * When executed while already inside a transaction or savepoint, this is a no-op : the effect runs
    * inline, with no new savepoint and no extra `BEGIN`.
    */
  def ensureAtomicScoped: URIO[Scope, Unit]

  /** Effect-wrapping form of [[ensureAtomicScoped]]. */
  def ensureAtomic[R, E, A](effect: ZIO[R, E, A]): ZIO[R, E, A]

  override final def apply[R, E, A](effect: ZIO[R, E, A])(using trace: Trace): ZIO[R, E, A] = atomically(effect)

}
object Atomically {

  val atomically: ZIOAspectAtLeastR[Atomically] =
    new ZIOAspectAtLeastR.Impl[Atomically] {
      override def apply[R <: Atomically, E, A](effect: ZIO[R, E, A])(using trace: Trace): ZIO[R, E, A] =
        ZIO.serviceWithZIO[Atomically](_.atomically(effect))
    }

  val ensureAtomic: ZIOAspectAtLeastR[Atomically] =
    new ZIOAspectAtLeastR.Impl[Atomically] {
      override def apply[R <: Atomically, E, A](effect: ZIO[R, E, A])(using trace: Trace): ZIO[R, E, A] =
        ZIO.serviceWithZIO[Atomically](_.ensureAtomic(effect))
    }

  final class NoOp extends Atomically {
    override def atomicallyScoped: URIO[Scope, Unit] = ZIO.unit
    override def atomically[R, E, A](effect: ZIO[R, E, A]): ZIO[R, E, A] = effect
    override def ensureAtomicScoped: URIO[Scope, Unit] = ZIO.unit
    override def ensureAtomic[R, E, A](effect: ZIO[R, E, A]): ZIO[R, E, A] = effect
  }
  object NoOp {

    val layer: ULayer[Atomically] =
      ZLayer.succeed { new NoOp }

  }

}
