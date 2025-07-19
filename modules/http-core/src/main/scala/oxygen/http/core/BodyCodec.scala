package oxygen.http.core

import oxygen.http.model.*
import oxygen.json.*
import oxygen.predef.core.*
import zio.*

trait BodyCodec[A] {

  def decode(body: HttpBody): ZIO[Scope, DecodingFailure.Cause, A]
  def encode(value: A): HttpBody

  def transform[B](ab: A => B, ba: B => A): BodyCodec[B] = BodyCodec.Transform(this, ab, ba)
  def transformOrFail[B](ab: A => Either[String, B], ba: B => A): BodyCodec[B] = BodyCodec.TransformOrFail(this, ab, ba)

}
object BodyCodec extends BodyCodecLowPriority.LowPriority1 {

  trait Text[A] extends BodyCodec[A] {

    def decodeText(body: HttpBody.Text): Either[DecodingFailure.Cause, A]
    def encodeText(value: A): HttpBody.Text

    override final def decode(body: HttpBody): ZIO[Scope, DecodingFailure.Cause, A] =
      body match {
        case HttpBody.Empty        => ZIO.fail(DecodingFailure.Cause.MissingRequired)
        case body: HttpBody.Text   => ZIO.fromEither(decodeText(body))
        case body: HttpBody.Bytes  => ZIO.fromEither(decodeText(HttpBody.Text(new String(body.body), body.contentType)))
        case body: HttpBody.Stream =>
          val stream: ZIO[Scope, DecodingFailure.Cause, String] =
            body.contentLength match {
              case Some(contentLength) if contentLength > Int.MaxValue => ZIO.fail(DecodingFailure.Cause.DecodeError("too large to fit into a String"))
              case Some(contentLength)                                 => body.stream.flatMap { stream => ZIO.succeed { new String(stream.readNBytes(contentLength.toInt), body.charset) } }
              case None                                                => body.stream.flatMap { stream => ZIO.succeed { new String(stream.readAllBytes(), body.charset) } }
            }

          stream.flatMap { string => ZIO.fromEither(decodeText(HttpBody.Text(string, body.contentType))) }
      }

    override final def encode(value: A): HttpBody = encodeText(value)

    // compiler doesn't seem to like abstract override type refinement on a parent that is implemented...?   :(
    override def transform[B](ab: A => B, ba: B => A): BodyCodec.Text[B] = throw new RuntimeException("must implement `transform`")
    override def transformOrFail[B](ab: A => Either[String, B], ba: B => A): BodyCodec.Text[B] = throw new RuntimeException("must implement `transformOrFail`")

  }

  trait Plain[A] extends BodyCodec.Text[A] {

    override final def transform[B](ab: A => B, ba: B => A): BodyCodec.Plain[B] = BodyCodec.Plain.Transform(this, ab, ba)
    override final def transformOrFail[B](ab: A => Either[String, B], ba: B => A): BodyCodec.Plain[B] = BodyCodec.Plain.TransformOrFail(this, ab, ba)

  }
  object Plain {

    case object Str extends BodyCodec.Plain[String] {
      override def decodeText(body: HttpBody.Text): Either[DecodingFailure.Cause, String] = body.body.asRight
      override def encodeText(value: String): HttpBody.Text = HttpBody.plain(value)
    }

    final case class Required[A](codec: StringCodec[A]) extends BodyCodec.Plain[A] {

      override def decodeText(body: HttpBody.Text): Either[DecodingFailure.Cause, A] =
        codec.decoder.decode(body.body) match
          case Right(value)                 => value.asRight
          case Left(_) if body.body.isEmpty => DecodingFailure.Cause.MissingRequired.asLeft
          case Left(error)                  => DecodingFailure.Cause.DecodeError(error).asLeft

      override def encodeText(value: A): HttpBody.Text =
        HttpBody.plain(codec.encoder.encode(value))

    }

    final case class Transform[A, B](a: BodyCodec.Plain[A], ab: A => B, ba: B => A) extends BodyCodec.Plain[B] {
      override def decodeText(body: HttpBody.Text): Either[DecodingFailure.Cause, B] = a.decodeText(body).map(ab)
      override def encodeText(value: B): HttpBody.Text = a.encodeText(ba(value))
    }
    final case class TransformOrFail[A, B](a: BodyCodec.Plain[A], ab: A => Either[String, B], ba: B => A) extends BodyCodec.Plain[B] {
      override def decodeText(body: HttpBody.Text): Either[DecodingFailure.Cause, B] = a.decodeText(body).flatMap(ab(_).leftMap(DecodingFailure.Cause.DecodeError(_)))
      override def encodeText(value: B): HttpBody.Text = a.encodeText(ba(value))
    }

    given string: BodyCodec.Plain[String] = BodyCodec.Plain.Str
    given typed: [A: StringCodec as codec] => BodyCodec.Plain[A] = BodyCodec.Plain.Required(codec)

  }

  trait Json[A] extends BodyCodec.Text[A] {

    override final def transform[B](ab: A => B, ba: B => A): BodyCodec.Json[B] = BodyCodec.Json.Transform(this, ab, ba)
    override final def transformOrFail[B](ab: A => Either[String, B], ba: B => A): BodyCodec.Json[B] = BodyCodec.Json.TransformOrFail(this, ab, ba)

  }
  object Json {

    final case class Required[A](codec: JsonCodec[A]) extends BodyCodec.Json[A] {

      override def decodeText(body: HttpBody.Text): Either[DecodingFailure.Cause, A] =
        codec.decoder.decodeJsonString(body.body) match
          case Right(value)                 => value.asRight
          case Left(_) if body.body.isEmpty => DecodingFailure.Cause.MissingRequired.asLeft
          case Left(error)                  => DecodingFailure.Cause.DecodeError(error.toString).asLeft

      override def encodeText(value: A): HttpBody.Text =
        HttpBody.json(codec.encoder.encodeJsonStringCompact(value))

    }

    final case class Transform[A, B](a: BodyCodec.Json[A], ab: A => B, ba: B => A) extends BodyCodec.Json[B] {
      override def decodeText(body: HttpBody.Text): Either[DecodingFailure.Cause, B] = a.decodeText(body).map(ab)
      override def encodeText(value: B): HttpBody.Text = a.encodeText(ba(value))
    }
    final case class TransformOrFail[A, B](a: BodyCodec.Json[A], ab: A => Either[String, B], ba: B => A) extends BodyCodec.Json[B] {
      override def decodeText(body: HttpBody.Text): Either[DecodingFailure.Cause, B] = a.decodeText(body).flatMap(ab(_).leftMap(DecodingFailure.Cause.DecodeError(_)))
      override def encodeText(value: B): HttpBody.Text = a.encodeText(ba(value))
    }

    given typed: [A: JsonCodec as codec] => BodyCodec.Json[A] = BodyCodec.Json.Required(codec)

  }

  case object Empty extends BodyCodec[Unit] {
    override def decode(body: HttpBody): ZIO[Scope, DecodingFailure.Cause, Unit] = ZIO.unit
    override def encode(value: Unit): HttpBody = HttpBody.emptyPlain
  }

  case object DNE extends BodyCodec[Nothing] {
    override def decode(body: HttpBody): ZIO[Scope, DecodingFailure.Cause, Nothing] = ZIO.fail(DecodingFailure.Cause.DecodeError("can not decode body"))
    override def encode(value: Nothing): HttpBody = throw new RuntimeException("tried encoding nothing?")
  }

  final case class Transform[A, B](a: BodyCodec[A], ab: A => B, ba: B => A) extends BodyCodec[B] {
    override def decode(body: HttpBody): ZIO[Scope, DecodingFailure.Cause, B] = a.decode(body).map(ab)
    override def encode(value: B): HttpBody = a.encode(ba(value))
  }
  final case class TransformOrFail[A, B](a: BodyCodec[A], ab: A => Either[String, B], ba: B => A) extends BodyCodec[B] {
    override def decode(body: HttpBody): ZIO[Scope, DecodingFailure.Cause, B] = a.decode(body).flatMap { aValue => ZIO.fromEither(ab(aValue).leftMap(DecodingFailure.Cause.DecodeError(_))) }
    override def encode(value: B): HttpBody = a.encode(ba(value))
  }

  given unit: BodyCodec[Unit] = BodyCodec.Empty
  given dne: BodyCodec[Nothing] = BodyCodec.DNE
  given string: BodyCodec[String] = BodyCodec.Plain.Str

}

object BodyCodecLowPriority {

  trait LowPriority1 {

    given typedJson: [A: JsonCodec as codec] => BodyCodec[A] = BodyCodec.Json.Required(codec)

  }

}
