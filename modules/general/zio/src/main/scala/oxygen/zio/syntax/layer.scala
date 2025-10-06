package oxygen.zio.syntax

import zio.*

object layer {

  extension [R, E, A](self: ZLayer[R, E, A]) {

    def projectZIO[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B])(using aTag: Tag[A], bTag: Tag[B]): ZLayer[R1, E1, B] =
      self.flatMap { aEnv => ZLayer.fromZIO { f(aEnv.get[A]) } }

    def projectScopedZIO[R1 <: R, E1 >: E, B](f: A => ZIO[R1 & Scope, E1, B])(using aTag: Tag[A], bTag: Tag[B]): ZLayer[R1, E1, B] =
      self.flatMap { aEnv => ZLayer.scoped[R1] { f(aEnv.get[A]) } }

    def projectZLayer[R1 <: R, E1 >: E, B](f: A => ZLayer[R1, E1, B])(using aTag: Tag[A]): ZLayer[R1, E1, B] =
      self.flatMap { aEnv => f(aEnv.get[A]) }

  }

}
