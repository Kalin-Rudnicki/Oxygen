package oxygen.json

import oxygen.predef.core.*

object instances {

  given typeTagDecoder: JsonDecoder[TypeTag[?]] = JsonDecoder.string.map { _ => TypeTag[Any] }
  given throwableReprDecoder: JsonDecoder[ThrowableRepr] = JsonDecoder.derived

  given typeTagEncoder: JsonEncoder[TypeTag[?]] = JsonEncoder.string.contramap(_.prefixObject)
  given throwableReprEncoder: JsonEncoder[ThrowableRepr] = JsonEncoder.derived

}
