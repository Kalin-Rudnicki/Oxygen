package oxygen.zio

import zio.*

type ZIOAspectPoly = ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any]
object ZIOAspectPoly {

  /**
    * Primary purpose is that intellij auto-expansion of extends usually messes up `zio:` + `zio.Trace` and all the useless `<:` `>:`
    */
  trait Impl extends ZIOAspectPoly {
    override def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A]
  }

  def fiberRefLocallyWith[RefT](fiberRef: FiberRef[RefT])(f: RefT => RefT): ZIOAspectPoly =
    new ZIOAspectPoly.Impl {
      override def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
        fiberRef.locallyWith(f)(effect)
    }
  def fiberRefLocallyWithUIO[RefT](fiberRef: FiberRef[RefT])(f: RefT => UIO[RefT]): ZIOAspectPoly =
    new ZIOAspectPoly.Impl {
      override def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
        fiberRef.getWith(f).flatMap(fiberRef.locally(_)(effect))
    }
  def fiberRefLocally[RefT](fiberRef: FiberRef[RefT])(f: RefT): ZIOAspectPoly =
    new ZIOAspectPoly.Impl {
      override def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
        fiberRef.locally(f)(effect)
    }
  def fiberRefLocallyUIO[RefT](fiberRef: FiberRef[RefT])(f: UIO[RefT]): ZIOAspectPoly =
    new ZIOAspectPoly.Impl {
      override def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
        f.flatMap(fiberRef.locally(_)(effect))
    }

}

type ZIOAspectAtLeastR[R] = ZIOAspect[Nothing, R, Nothing, Any, Nothing, Any]
object ZIOAspectAtLeastR {

  trait Impl[_R] extends ZIOAspectAtLeastR[_R] {
    override def apply[R <: _R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A]
  }

  def fiberRefLocallyWithURIO[_R, RefT](fiberRef: FiberRef[RefT])(f: RefT => URIO[_R, RefT]): ZIOAspectAtLeastR[_R] =
    new ZIOAspectAtLeastR.Impl[_R] {
      override def apply[R <: _R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
        fiberRef.getWith(f).flatMap(fiberRef.locally(_)(effect))
    }
  def fiberRefLocallyURIO[_R, RefT](fiberRef: FiberRef[RefT])(f: URIO[_R, RefT]): ZIOAspectAtLeastR[_R] =
    new ZIOAspectAtLeastR.Impl[_R] {
      override def apply[R <: _R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
        f.flatMap(fiberRef.locally(_)(effect))
    }

}

private[zio] implicit class FiberRefAnnotationOps[RefT](self: FiberRef[RefT]) {

  def locallyWithAnnotation(f: RefT => RefT): ZIOAspectPoly = ZIOAspectPoly.fiberRefLocallyWith(self)(f)
  def locallyWithUIOAnnotation(f: RefT => UIO[RefT]): ZIOAspectPoly = ZIOAspectPoly.fiberRefLocallyWithUIO(self)(f)
  def locallyAnnotation(f: RefT): ZIOAspectPoly = ZIOAspectPoly.fiberRefLocally(self)(f)
  def locallyUIOAnnotation(f: UIO[RefT]): ZIOAspectPoly = ZIOAspectPoly.fiberRefLocallyUIO(self)(f)

  def locallyWithURIOAnnotation[R](f: RefT => URIO[R, RefT]): ZIOAspectAtLeastR[R] = ZIOAspectAtLeastR.fiberRefLocallyWithURIO(self)(f)
  def locallyURIOAnnotation[R](f: URIO[R, RefT]): ZIOAspectAtLeastR[R] = ZIOAspectAtLeastR.fiberRefLocallyURIO(self)(f)

}
