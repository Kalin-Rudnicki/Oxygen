package oxygen.json

import oxygen.predef.core.*

object instances {

  given typeTagDecoder: JsonDecoder[TypeTag[? <: AnyKind]] = JsonDecoder.string.map { _ => TypeTag[Any] }
  given typeTagEncoder: JsonEncoder[TypeTag[? <: AnyKind]] = JsonEncoder.string.contramap(_.prefixObject)

  private given encodedErrorTraceDecoder: JsonDecoder[Error.EncodedError.Trace] = JsonDecoder.derived
  private given encodedErrorDecoder: JsonDecoder[Error.EncodedError] = JsonDecoder.derived
  given errorDecoder: JsonDecoder[Error] = encodedErrorDecoder.map(_.toError)
  given throwableDecoder: JsonDecoder[Throwable] = errorDecoder.map { e => e }

  private given encodedErrorTraceEncoder: JsonEncoder[Error.EncodedError.Trace] = JsonEncoder.derived
  private given encodedErrorEncoder: JsonEncoder[Error.EncodedError] = JsonEncoder.derived
  given errorEncoder: JsonEncoder[Error] = encodedErrorEncoder.contramap(Error.EncodedError.fromError)
  given throwableEncoder: JsonEncoder[Throwable] = errorEncoder.contramap(Error.fromThrowable)

}
