package oxygen.json

final case class JsonFieldEncoder[A](encode: A => String) {
  def contramap[B](f: B => A): JsonFieldEncoder[B] = JsonFieldEncoder(b => encode(f(b)))
}
object JsonFieldEncoder {

  // TODO (KR) :
  given string: JsonFieldEncoder[String] = JsonFieldEncoder(identity)

}
