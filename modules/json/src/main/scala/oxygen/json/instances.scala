package oxygen.json

import oxygen.predef.core.*

object instances {

  given typeTagDecoder: JsonDecoder[TypeTag[?]] = JsonDecoder.string.map { _ => TypeTag[Any] }
  given throwableReprDecoder: JsonDecoder[ThrowableRepr] = JsonDecoder.derived
  given throwableDecoder: JsonDecoder[Throwable] = throwableReprDecoder.map(_.toThrowable)

  given typeTagEncoder: JsonEncoder[TypeTag[?]] = JsonEncoder.string.contramap(_.prefixObject)
  given throwableReprEncoder: JsonEncoder[ThrowableRepr] = JsonEncoder.derived
  given throwableEncoder: JsonEncoder[Throwable] = throwableReprEncoder.contramap(ThrowableRepr.fromThrowable)

}
