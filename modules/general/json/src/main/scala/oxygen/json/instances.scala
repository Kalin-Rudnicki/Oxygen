package oxygen.json

import oxygen.predef.core.*

object instances {

  given typeTagDecoder: JsonDecoder[TypeTag[?]] = JsonDecoder.string.map { _ => TypeTag[Any] }
  given throwableReprDecoder: JsonDecoder[ThrowableRepr] = JsonDecoder.derived

  given typeTagEncoder: JsonEncoder[TypeTag[?]] = JsonEncoder.string.contramap(_.prefixObject)
  given throwableReprEncoder: JsonEncoder[ThrowableRepr] = JsonEncoder.derived

  object throwable {

    object encoded {

      given throwableDecoder: JsonDecoder[Throwable] = throwableReprDecoder.map(_.toThrowable)
      given throwableEncoder: JsonEncoder[Throwable] = throwableReprEncoder.contramap(ThrowableRepr.fromThrowable)

    }

    object string {

      given throwableDecoder: JsonDecoder[Throwable] = JsonDecoder.string.map(new RuntimeException(_))
      given throwableEncoder: JsonEncoder[Throwable] = JsonEncoder.string.contramap(_.safeGetMessage)

    }

  }

}
