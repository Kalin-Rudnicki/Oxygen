package oxygen.zio.syntax

import oxygen.core.Text
import oxygen.core.error.*
import zio.*

object error {

  extension [R, E, A](self: ZIO[R, E, A]) {

    /**
      * Maps the current error cause to a [[oxygen.core.error.Error]], retaining ALL underlying ZIO causes.
      * [[Cause.Fail]] => [[Cause.Fail]]
      * else:          => [[Cause.Die]]
      */
    def addErrorContext(message: => Text.Auto): ZIO[R, Error, A] =
      self.mapErrorCause { FromZioCause.addErrorContext(message, _) }

  }

  extension [R, E, A](self: ZLayer[R, E, A]) {

    /**
      * Maps the current error cause to a [[oxygen.core.error.Error]], retaining ALL underlying ZIO causes.
      * [[Cause.Fail]] => [[Cause.Fail]]
      * else:          => [[Cause.Die]]
      */
    def addErrorContext(message: => Text.Auto): ZLayer[R, Error, A] =
      self.mapErrorCause { FromZioCause.addErrorContext(message, _) }

  }

}
