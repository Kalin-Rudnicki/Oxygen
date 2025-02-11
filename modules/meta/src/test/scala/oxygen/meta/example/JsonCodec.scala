package oxygen.meta.example

import oxygen.meta.*
import oxygen.predef.core.*

final case class JsonCodec[A](enc: JsonEncoder[A], dec: JsonDecoder[A]) extends JsonEncoder[A], JsonDecoder[A] {
  override def encode(a: A): JsonAST = enc.encode(a)
  override def decode(json: JsonAST): Either[JError, A] = dec.decode(json)
}
object JsonCodec {

  inline def derived[A]: JsonCodec[A] = JsonCodec[A](JsonEncoder.derived[A], JsonDecoder.derived[A])

}
