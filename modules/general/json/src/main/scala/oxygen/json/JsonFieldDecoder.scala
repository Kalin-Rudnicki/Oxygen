package oxygen.json

import oxygen.predef.core.*

final case class JsonFieldDecoder[A](decode: String => Either[String, A]) {
  def map[B](f: A => B): JsonFieldDecoder[B] = JsonFieldDecoder(decode(_).map(f))
  def mapOrFail[B](f: A => Either[String, B]): JsonFieldDecoder[B] = JsonFieldDecoder(decode(_).flatMap(f))
}
object JsonFieldDecoder {

  given string: JsonFieldDecoder[String] = JsonFieldDecoder(_.asRight)

}
