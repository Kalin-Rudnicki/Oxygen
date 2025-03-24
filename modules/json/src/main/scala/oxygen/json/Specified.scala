package oxygen.json

import oxygen.predef.core.*
import zio.json.*
import zio.json.ast.Json
import zio.json.internal.{RetractReader, Write}

sealed trait Specified[+A] {

  final def toOption: Option[A] = this match
    case Specified.WasSpecified(value) => value.some
    case Specified.WasNotSpecified     => None

  override final def toString: String = this match
    case Specified.WasSpecified(value) => value.toString
    case Specified.WasNotSpecified     => "<<unspecified>>"

}
object Specified {

  final case class WasSpecified[+A](value: A) extends Specified[A]
  case object WasNotSpecified extends Specified[Nothing]

  def fromOption[A](value: Option[A]): Specified[A] = value match
    case Some(value) => Specified.WasSpecified(value)
    case None        => Specified.WasNotSpecified

  given jsonEncoder: [A: {JsonEncoder as je}] => JsonEncoder[Specified[A]] =
    new JsonEncoder[Specified[A]] {

      override def unsafeEncode(a: Specified[A], indent: Option[Int], out: Write): Unit = a match
        case WasSpecified(value) => je.unsafeEncode(value, indent, out)
        case WasNotSpecified     => Json.Null.encoder.unsafeEncode(Json.Null, indent, out)

      override def isNothing(a: Specified[A]): Boolean =
        a.toOption.isEmpty

      override def toJsonAST(a: Specified[A]): Either[String, Json] = a match
        case WasSpecified(value) => je.toJsonAST(value)
        case WasNotSpecified     => Json.Null.encoder.toJsonAST(Json.Null)

    }

  given jsonDecoder: [A: {JsonDecoder as jd}] => JsonDecoder[Specified[A]] =
    new JsonDecoder[Specified[A]] {

      override def unsafeDecode(trace: List[JsonError], in: RetractReader): Specified[A] =
        Specified.WasSpecified(jd.unsafeDecode(trace, in))

      override def unsafeDecodeMissing(trace: List[JsonError]): Specified[A] =
        Specified.WasNotSpecified

      override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Specified[A] =
        Specified.WasSpecified(jd.unsafeFromJsonAST(trace, json))

    }

  given jsonCodec: [A: {JsonCodec as jc}] => JsonCodec[Specified[A]] =
    JsonCodec(jsonEncoder[A], jsonDecoder[A])

}

given idToSpecified: [A] => Conversion[A, Specified.WasSpecified[A]] = Specified.WasSpecified(_)
