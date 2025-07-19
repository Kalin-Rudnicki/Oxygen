package oxygen.http.core

import oxygen.http.model.*
import oxygen.json.*
import oxygen.predef.core.*

trait ParamCodec[A] {

  def decode(params: List[String]): Either[DecodingFailure.Cause, A]
  def encode(value: A): List[String]

  // TODO (KR) : transform

}
object ParamCodec {

  trait Plain[A] extends ParamCodec[A]
  object Plain extends ParamCodecLowPriority.PlainLowPriority1 {

    final case class Required[A](codec: StringCodec[A]) extends ParamCodec.Plain[A] {

      override def decode(params: List[String]): Either[DecodingFailure.Cause, A] = params match
        case param :: Nil => codec.decoder.decode(param).leftMap(DecodingFailure.Cause.DecodeError(_))
        case Nil          => DecodingFailure.Cause.MissingRequired.asLeft
        case _            => DecodingFailure.Cause.ManyNotSupported(params.size).asLeft

      override def encode(value: A): List[String] =
        codec.encoder.encode(value) :: Nil

    }

    final case class Optional[A](codec: StringCodec[A]) extends ParamCodec.Plain[Option[A]] {

      override def decode(params: List[String]): Either[DecodingFailure.Cause, Option[A]] = params match
        case param :: Nil => codec.decoder.decode(param).bimap(DecodingFailure.Cause.DecodeError(_), _.some)
        case Nil          => None.asRight
        case _            => DecodingFailure.Cause.ManyNotSupported(params.size).asLeft

      override def encode(value: Option[A]): List[String] =
        value.map(codec.encoder.encode).toList

    }

    final case class Many[A](codec: StringCodec[A]) extends ParamCodec.Plain[List[A]] {

      override def decode(params: List[String]): Either[DecodingFailure.Cause, List[A]] =
        params.traverse(codec.decoder.decode).leftMap(DecodingFailure.Cause.DecodeError(_))

      override def encode(value: List[A]): List[String] =
        value.map(codec.encoder.encode)

    }

    final case class ManyNonEmpty[A](codec: StringCodec[A]) extends ParamCodec.Plain[NonEmptyList[A]] {

      override def decode(params: List[String]): Either[DecodingFailure.Cause, NonEmptyList[A]] =
        params.toNonEmpty
          .toRight(DecodingFailure.Cause.MissingRequired)
          .flatMap(_.traverse(codec.decoder.decode).leftMap(DecodingFailure.Cause.DecodeError(_)))

      override def encode(value: NonEmptyList[A]): List[String] =
        value.toList.map(codec.encoder.encode)
    }

    given required: [A: StringCodec as codec] => ParamCodec.Plain[A] = ParamCodec.Plain.Required(codec)

  }

  trait Json[A] extends ParamCodec[A]
  object Json extends ParamCodecLowPriority.JsonLowPriority1 {

    final case class Required[A](codec: JsonCodec[A]) extends ParamCodec.Json[A] {

      override def decode(params: List[String]): Either[DecodingFailure.Cause, A] = params match
        case param :: Nil => codec.decoder.decodeJsonString(param).leftMap(e => DecodingFailure.Cause.DecodeError(e.getMessage))
        case Nil          => DecodingFailure.Cause.MissingRequired.asLeft
        case _            => DecodingFailure.Cause.ManyNotSupported(params.size).asLeft

      override def encode(value: A): List[String] =
        codec.encoder.encodeJsonStringCompact(value) :: Nil

    }

    final case class Optional[A](codec: JsonCodec[A]) extends ParamCodec.Json[Option[A]] {

      override def decode(params: List[String]): Either[DecodingFailure.Cause, Option[A]] = params match
        case param :: Nil => codec.decoder.decodeJsonString(param).bimap(e => DecodingFailure.Cause.DecodeError(e.getMessage), _.some)
        case Nil          => None.asRight
        case _            => DecodingFailure.Cause.ManyNotSupported(params.size).asLeft

      override def encode(value: Option[A]): List[String] =
        value.map(codec.encoder.encodeJsonStringCompact).toList

    }

    final case class Many[A](codec: JsonCodec[A]) extends ParamCodec.Json[List[A]] {

      override def decode(params: List[String]): Either[DecodingFailure.Cause, List[A]] =
        params.traverse(codec.decoder.decodeJsonString).leftMap(e => DecodingFailure.Cause.DecodeError(e.getMessage))

      override def encode(value: List[A]): List[String] =
        value.map(codec.encoder.encodeJsonStringCompact)

    }

    final case class ManyNonEmpty[A](codec: JsonCodec[A]) extends ParamCodec.Json[NonEmptyList[A]] {

      override def decode(params: List[String]): Either[DecodingFailure.Cause, NonEmptyList[A]] =
        params.toNonEmpty
          .toRight(DecodingFailure.Cause.MissingRequired)
          .flatMap(_.traverse(codec.decoder.decodeJsonString).leftMap(e => DecodingFailure.Cause.DecodeError(e.getMessage)))

      override def encode(value: NonEmptyList[A]): List[String] =
        value.toList.map(codec.encoder.encodeJsonStringCompact)
    }

    given required: [A: JsonCodec as codec] => ParamCodec.Json[A] = ParamCodec.Json.Required(codec)

  }

}

object ParamCodecLowPriority {

  trait PlainLowPriority1 {

    given optional: [A: StringCodec as codec] => ParamCodec.Plain[Option[A]] = ParamCodec.Plain.Optional(codec)
    given many: [A: StringCodec as codec] => ParamCodec.Plain[List[A]] = ParamCodec.Plain.Many(codec)
    given manyNonEmpty: [A: StringCodec as codec] => ParamCodec.Plain[NonEmptyList[A]] = ParamCodec.Plain.ManyNonEmpty(codec)

  }

  trait JsonLowPriority1 {

    given optional: [A: JsonCodec as codec] => ParamCodec.Json[Option[A]] = ParamCodec.Json.Optional(codec)
    given many: [A: JsonCodec as codec] => ParamCodec.Json[List[A]] = ParamCodec.Json.Many(codec)
    given manyNonEmpty: [A: JsonCodec as codec] => ParamCodec.Json[NonEmptyList[A]] = ParamCodec.Json.ManyNonEmpty(codec)

  }

}
