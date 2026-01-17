package oxygen.zio.syntax

import oxygen.core.error.*
import oxygen.predef.core.*
import oxygen.zio.ZioCauses
import zio.*
import zio.stream.*

object error {

  extension [R, E, A](self: ZIO[R, E, A]) {

    /**
      * Maps the current error cause to a [[oxygen.core.error.Error]], retaining ALL underlying ZIO causes.
      * [[Cause.Fail]] => [[Cause.Fail]]
      * else:          => [[Cause.Die]]
      */
    def addErrorContext(message: => Text.Auto): ZIO[R, Error, A] =
      self.mapErrorCause { FromZioCause.addErrorContext(message, _) }

    /**
      * Will attempt to maintain the same failure mode as the underlying cause, eg:
      * [[Cause.Fail]] -> [[Cause.Fail]]
      * [[Cause.Die]] -> [[Cause.Die]]
      * [[Cause.Interrupt]] -> [[Cause.Die]]
      * [[Cause.Empty]] -> [[Cause.Die]]
      */
    def convertCausesAuto[E2 <: Error](f: ZioCauses => E2)(using Trace): ZIO[R, E2, A] =
      self.catchAllCause { cause =>
        val (causes, causeType): (ZioCauses, ZioCauses.CauseType) = ZioCauses.fromCauseWithType(cause)
        val typedError: E2 = f(causes)
        if causeType.isFailure then ZIO.fail(typedError)
        else ZIO.die(typedError)
      }

    /**
      * Will convert any zio [[Cause]] to a [[Cause.Fail]].
      */
    def convertCausesFail[E2](f: ZioCauses => E2)(using Trace): ZIO[R, E2, A] =
      self.catchAllCause { cause =>
        val causes: ZioCauses = ZioCauses.fromCause(cause)
        val typedError: E2 = f(causes)
        ZIO.fail(typedError)
      }

    /**
      * Will convert any zio [[Cause]] to a [[Cause.Die]].
      */
    def convertCausesDie[E2 <: Error](f: ZioCauses => E2)(using Trace): ZIO[R, Nothing, A] =
      self.catchAllCause { cause =>
        val causes: ZioCauses = ZioCauses.fromCause(cause)
        val typedError: E2 = f(causes)
        ZIO.die(typedError)
      }

  }

  extension [R, E, A](self: ZLayer[R, E, A]) {

    /**
      * Maps the current error cause to a [[oxygen.core.error.Error]], retaining ALL underlying ZIO causes.
      * [[Cause.Fail]] => [[Cause.Fail]]
      * else:          => [[Cause.Die]]
      */
    def addErrorContext(message: => Text.Auto): ZLayer[R, Error, A] =
      self.mapErrorCause { FromZioCause.addErrorContext(message, _) }

    /**
      * Will attempt to maintain the same failure mode as the underlying cause, eg:
      * [[Cause.Fail]] -> [[Cause.Fail]]
      * [[Cause.Die]] -> [[Cause.Die]]
      * [[Cause.Interrupt]] -> [[Cause.Die]]
      * [[Cause.Empty]] -> [[Cause.Die]]
      */
    def convertCausesAuto[E2 <: Error](f: ZioCauses => E2)(using Trace): ZLayer[R, E2, A] =
      self.catchAllCause { cause =>
        val (causes, causeType): (ZioCauses, ZioCauses.CauseType) = ZioCauses.fromCauseWithType(cause)
        val typedError: E2 = f(causes)
        if causeType.isFailure then ZLayer.fail(typedError)
        else ZLayer.die(typedError)
      }

    /**
      * Will convert any zio [[Cause]] to a [[Cause.Fail]].
      */
    def convertCausesFail[E2](f: ZioCauses => E2)(using Trace): ZLayer[R, E2, A] =
      self.catchAllCause { cause =>
        val causes: ZioCauses = ZioCauses.fromCause(cause)
        val typedError: E2 = f(causes)
        ZLayer.fail(typedError)
      }

    /**
      * Will convert any zio [[Cause]] to a [[Cause.Die]].
      */
    def convertCausesDie[E2 <: Error](f: ZioCauses => E2)(using Trace): ZLayer[R, Nothing, A] =
      self.catchAllCause { cause =>
        val causes: ZioCauses = ZioCauses.fromCause(cause)
        val typedError: E2 = f(causes)
        ZLayer.die(typedError)
      }

  }

  extension [R, E, A](self: ZStream[R, E, A]) {

    /**
      * Maps the current error cause to a [[oxygen.core.error.Error]], retaining ALL underlying ZIO causes.
      * [[Cause.Fail]] => [[Cause.Fail]]
      * else:          => [[Cause.Die]]
      */
    def addErrorContext(message: => Text.Auto): ZStream[R, Error, A] =
      self.mapErrorCause { FromZioCause.addErrorContext(message, _) }

    /**
      * Will attempt to maintain the same failure mode as the underlying cause, eg:
      * [[Cause.Fail]] -> [[Cause.Fail]]
      * [[Cause.Die]] -> [[Cause.Die]]
      * [[Cause.Interrupt]] -> [[Cause.Die]]
      * [[Cause.Empty]] -> [[Cause.Die]]
      */
    def convertCausesAuto[E2 <: Error](f: ZioCauses => E2)(using Trace): ZStream[R, E2, A] =
      self.catchAllCause { cause =>
        val (causes, causeType): (ZioCauses, ZioCauses.CauseType) = ZioCauses.fromCauseWithType(cause)
        val typedError: E2 = f(causes)
        if causeType.isFailure then ZStream.fail(typedError)
        else ZStream.die(typedError)
      }

    /**
      * Will convert any zio [[Cause]] to a [[Cause.Fail]].
      */
    def convertCausesFail[E2](f: ZioCauses => E2)(using Trace): ZStream[R, E2, A] =
      self.catchAllCause { cause =>
        val causes: ZioCauses = ZioCauses.fromCause(cause)
        val typedError: E2 = f(causes)
        ZStream.fail(typedError)
      }

    /**
      * Will convert any zio [[Cause]] to a [[Cause.Die]].
      */
    def convertCausesDie[E2 <: Error](f: ZioCauses => E2)(using Trace): ZStream[R, Nothing, A] =
      self.catchAllCause { cause =>
        val causes: ZioCauses = ZioCauses.fromCause(cause)
        val typedError: E2 = f(causes)
        ZStream.die(typedError)
      }

  }

  extension (self: ZIO.type) {

    def attempting[E <: Error, A](causesToError: ZioCauses => E)(thunk: => A)(using Trace): IO[E, A] =
      ZIO.attempt { thunk }.convertCausesFail(causesToError)

  }

}
