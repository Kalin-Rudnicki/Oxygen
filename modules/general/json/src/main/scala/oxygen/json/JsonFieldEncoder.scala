package oxygen.json

import oxygen.predef.core.*

final case class JsonFieldEncoder[A](encode: A => String) {
  def contramap[B](f: B => A): JsonFieldEncoder[B] = JsonFieldEncoder(b => encode(f(b)))
}
object JsonFieldEncoder extends JsonFieldEncoderLowPriority.LowPriority1 {

  given string: JsonFieldEncoder[String] = JsonFieldEncoder(identity)

}

object JsonFieldEncoderLowPriority {

  trait LowPriority1 {

    given fromStringEncoder: [A: StringEncoder as enc] => JsonFieldEncoder[A] = JsonFieldEncoder.string.contramap(enc.encode)

  }

}
