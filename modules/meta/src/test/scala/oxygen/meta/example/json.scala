package oxygen.meta.example

import oxygen.predef.core.*

sealed trait JsonAST
object JsonAST {
  final case class JString(value: String) extends JsonAST
  final case class JNumber(value: Double) extends JsonAST
  final case class JBoolean(value: Boolean) extends JsonAST
  case object JNull extends JsonAST

  final case class JArray(elements: Seq[JsonAST]) extends JsonAST
  object JArray {
    def of(elements: JsonAST*): JArray = JArray(elements)
  }

  final case class JObject(fields: Map[String, JsonAST]) extends JsonAST
  object JObject {
    def apply(fields: (String, JsonAST)*): JObject = JObject(fields.toMap)
  }
}

sealed trait JError
object JError {
  final case class InvalidKey(key: String, message: String) extends JError
  final case class InvalidJson(message: String) extends JError
  final case class InField(fieldName: String, error: JError) extends JError
  final case class InIndex(index: Int, error: JError) extends JError
}

trait KeyDecoder[A] {
  def decode(key: String): Either[String, A]
}
object KeyDecoder {

  given KeyDecoder[String] = _.asRight
  given KeyDecoder[Int] = s => s.toIntOption.toRight(s"Invalid Int key: $s")
  given KeyDecoder[Boolean] = {
    case "true"  => true.asRight
    case "false" => false.asRight
    case _       => "Invalid Boolean key".asLeft
  }

}

trait KeyEncoder[A] {
  def encode(key: A): String
}
object KeyEncoder {

  given KeyEncoder[String] = _.toString
  given KeyEncoder[Int] = _.toString
  given KeyEncoder[Boolean] = _.toString

}
