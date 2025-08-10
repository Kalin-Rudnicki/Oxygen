package oxygen.zio

import zio.*

trait FiberRefModificationR[-_R] extends ZIOAspectAtLeastR[_R] {

  /**
    * Runs the specified effect using [[FiberRef.locally]]/[[FiberRef.locallyWith]]
    */
  override def apply[R <: _R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A]

  /**
    * Updates the [[FiberRef]] using [[FiberRef.set]]/[[FiberRef.update]], with no expectation of it being set back.
    */
  def set: URIO[_R, Unit]

  /**
    * Updates the [[FiberRef]] using [[FiberRef.locallyScoped]]/[[FiberRef.locallyScopedWith]], where it will be set back when the scope is closed.
    */
  def setScoped: URIO[_R & Scope, Unit]

  final def toLayer: URLayer[_R, Unit] =
    ZLayer.scoped { setScoped }

  final def >>>[R2](that: FiberRefModificationR[R2]): FiberRefModificationR[_R & R2] =
    FiberRefModificationR.AndThen(this, that)

}
object FiberRefModificationR {

  private final case class AndThen[_R](a: FiberRefModificationR[_R], b: FiberRefModificationR[_R]) extends FiberRefModificationR[_R] {

    override def apply[R <: _R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
      b(a(effect))

    override def set: URIO[_R, Unit] =
      a.set *> b.set

    override def setScoped: URIO[_R & Scope, Unit] =
      a.setScoped *> b.setScoped

  }

  private final case class SetURIO[_R, RefT](fiberRef: FiberRef[RefT], f: URIO[_R, RefT]) extends FiberRefModificationR[_R] {

    override def apply[R <: _R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
      f.flatMap(fiberRef.locally(_)(effect))

    override def set: URIO[_R, Unit] =
      f.flatMap(fiberRef.set)

    override def setScoped: URIO[_R & Scope, Unit] =
      f.flatMap(fiberRef.locallyScoped)

  }

  private final case class UpdateURIO[_R, RefT](fiberRef: FiberRef[RefT], f: RefT => URIO[_R, RefT]) extends FiberRefModificationR[_R] {

    override def apply[R <: _R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
      fiberRef.getWith(f).flatMap(fiberRef.locally(_)(effect))

    override def set: URIO[_R, Unit] =
      fiberRef.getWith(f).flatMap(fiberRef.set)

    override def setScoped: URIO[_R & Scope, Unit] =
      fiberRef.getWith(f).flatMap(fiberRef.locallyScoped)

  }

  def setURIO[R, RefT](fiberRef: FiberRef[RefT])(f: URIO[R, RefT]): FiberRefModificationR[R] = FiberRefModificationR.SetURIO(fiberRef, f)
  def updateURIO[R, RefT](fiberRef: FiberRef[RefT])(f: RefT => URIO[R, RefT]): FiberRefModificationR[R] = FiberRefModificationR.UpdateURIO(fiberRef, f)

}

trait FiberRefModification extends FiberRefModificationR[Any] {

  /**
    * Runs the specified effect using [[FiberRef.locally]]/[[FiberRef.locallyWith]]
    */
  override def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A]

  /**
    * Updates the [[FiberRef]] using [[FiberRef.set]]/[[FiberRef.update]], with no expectation of it being set back.
    */
  override def set: UIO[Unit]

  /**
    * Updates the [[FiberRef]] using [[FiberRef.locallyScoped]]/[[FiberRef.locallyScopedWith]], where it will be set back when the scope is closed.
    */
  override def setScoped: URIO[Scope, Unit]

  final def >>>(that: FiberRefModification): FiberRefModification =
    FiberRefModification.AndThen(this, that)

}
object FiberRefModification {

  private final case class Set[RefT](fiberRef: FiberRef[RefT], f: RefT) extends FiberRefModification {

    override def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
      fiberRef.locally(f)(effect)

    override def set: UIO[Unit] =
      fiberRef.set(f)

    override def setScoped: URIO[Scope, Unit] =
      fiberRef.locallyScoped(f)

  }

  private final case class SetUIO[RefT](fiberRef: FiberRef[RefT], f: UIO[RefT]) extends FiberRefModification {

    override def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
      f.flatMap(fiberRef.locally(_)(effect))

    override def set: UIO[Unit] =
      f.flatMap(fiberRef.set)

    override def setScoped: URIO[Scope, Unit] =
      f.flatMap(fiberRef.locallyScoped)

  }

  private final case class Update[RefT](fiberRef: FiberRef[RefT], f: RefT => RefT) extends FiberRefModification {

    override def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
      fiberRef.locallyWith(f)(effect)

    override def set: UIO[Unit] =
      fiberRef.update(f)

    override def setScoped: URIO[Scope, Unit] =
      fiberRef.locallyScopedWith(f)

  }

  private final case class UpdateUIO[RefT](fiberRef: FiberRef[RefT], f: RefT => UIO[RefT]) extends FiberRefModification {

    override def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
      fiberRef.getWith(f).flatMap(fiberRef.locally(_)(effect))

    override def set: UIO[Unit] =
      fiberRef.getWith(f).flatMap(fiberRef.set)

    override def setScoped: URIO[Scope, Unit] =
      fiberRef.getWith(f).flatMap(fiberRef.locallyScoped)

  }

  private final case class AndThen(a: FiberRefModification, b: FiberRefModification) extends FiberRefModification {

    override def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
      b(a(effect))

    override def set: UIO[Unit] =
      a.set *> b.set

    override def setScoped: URIO[Scope, Unit] =
      a.setScoped *> b.setScoped

  }

  def set[RefT](fiberRef: FiberRef[RefT])(f: RefT): FiberRefModification = FiberRefModification.Set(fiberRef, f)
  def setUIO[RefT](fiberRef: FiberRef[RefT])(f: UIO[RefT]): FiberRefModification = FiberRefModification.SetUIO(fiberRef, f)
  def update[RefT](fiberRef: FiberRef[RefT])(f: RefT => RefT): FiberRefModification = FiberRefModification.Update(fiberRef, f)
  def updateUIO[RefT](fiberRef: FiberRef[RefT])(f: RefT => UIO[RefT]): FiberRefModification = FiberRefModification.UpdateUIO(fiberRef, f)

}

implicit class FiberRefModificationOps[RefT](self: FiberRef[RefT]) {

  object modification {

    def set(f: RefT): FiberRefModification = FiberRefModification.set(self)(f)
    def setUIO(f: UIO[RefT]): FiberRefModification = FiberRefModification.setUIO(self)(f)
    def update(f: RefT => RefT): FiberRefModification = FiberRefModification.update(self)(f)
    def updateUIO(f: RefT => UIO[RefT]): FiberRefModification = FiberRefModification.updateUIO(self)(f)

    def setURIO[R](f: URIO[R, RefT]): FiberRefModificationR[R] = FiberRefModificationR.setURIO(self)(f)
    def updateURIO[R](f: RefT => URIO[R, RefT]): FiberRefModificationR[R] = FiberRefModificationR.updateURIO(self)(f)

  }

}
