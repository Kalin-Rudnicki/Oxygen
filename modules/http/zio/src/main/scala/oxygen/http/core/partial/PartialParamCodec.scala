package oxygen.http.core.partial

import oxygen.core.syntax.functor.*
import oxygen.http.core.DecodingFailureCause
import oxygen.http.schema.ParamType
import oxygen.http.schema.partial.PartialParamSchema
import oxygen.predef.core.*
import oxygen.schema.*

trait PartialParamCodec[A] {

  val partialParamSchema: PartialParamSchema

  def decode(params: List[String]): Either[DecodingFailureCause, A]
  def encode(value: A): List[String]

}
object PartialParamCodec extends PartialParamCodecLowPriority.LowPriority1 {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Plain
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Plain[A] extends PartialParamCodec[A]
  object Plain extends PartialParamCodecLowPriority.PlainLowPriority1 {

    // =====| Givens |=====

    given optional: [A: PlainTextSchema as schema] => PartialParamCodec.Plain[Option[A]] = PartialParamCodec.Plain.Optional(schema)
    given manyNonEmpty: [A: PlainTextSchema as schema] => PartialParamCodec.Plain[NonEmptyList[A]] = PartialParamCodec.Plain.ManyRequired(schema)

    // =====| Instances |=====

    final case class Required[A](schema: PlainTextSchema[A]) extends Plain[A] {

      override val partialParamSchema: PartialParamSchema = PartialParamSchema(schema, ParamType.Param.Required)

      override def decode(params: List[String]): Either[DecodingFailureCause, A] =
        params match
          case Nil          => DecodingFailureCause.MissingRequired.asLeft
          case param :: Nil => schema.decode(param).leftMap(DecodingFailureCause.DecodeError(_, DecodingFailureCause.DecodeInput.InputValues(params)))
          case _            => DecodingFailureCause.ManyNotSupported(params.size).asLeft

      override def encode(value: A): List[String] =
        schema.encode(value) :: Nil

    }

    final case class Optional[A](schema: PlainTextSchema[A]) extends Plain[Option[A]] {

      override val partialParamSchema: PartialParamSchema = PartialParamSchema(schema, ParamType.Param.Optional)

      override def decode(params: List[String]): Either[DecodingFailureCause, Option[A]] =
        params match
          case Nil          => None.asRight
          case param :: Nil => schema.decode(param).bimap(DecodingFailureCause.DecodeError(_, DecodingFailureCause.DecodeInput.InputValues(params)), _.some)
          case _            => DecodingFailureCause.ManyNotSupported(params.size).asLeft

      override def encode(value: Option[A]): List[String] =
        value.map(schema.encode).toList

    }

    final case class ManyOptional[S[_]: SeqOps as seqOps, A](schema: PlainTextSchema[A]) extends Plain[S[A]] {

      override val partialParamSchema: PartialParamSchema = PartialParamSchema(schema, ParamType.Param.ManyOptional)

      override def decode(params: List[String]): Either[DecodingFailureCause, S[A]] =
        params.into[S].traverse(schema.decode).leftMap(DecodingFailureCause.DecodeError(_, DecodingFailureCause.DecodeInput.InputValues(params)))

      override def encode(value: S[A]): List[String] =
        value.map(schema.encode).into[List]

    }

    final case class ManyRequired[A](schema: PlainTextSchema[A]) extends Plain[NonEmptyList[A]] {

      override val partialParamSchema: PartialParamSchema = PartialParamSchema(schema, ParamType.Param.ManyRequired)

      override def decode(params: List[String]): Either[DecodingFailureCause, NonEmptyList[A]] =
        NonEmptyList
          .fromList(params)
          .toRight(DecodingFailureCause.MissingRequired)
          .flatMap(_.traverse(schema.decode).leftMap(DecodingFailureCause.DecodeError(_, DecodingFailureCause.DecodeInput.InputValues(params))))

      override def encode(value: NonEmptyList[A]): List[String] =
        value.toList.map(schema.encode)

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Json
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Json[A] extends PartialParamCodec[A]
  object Json extends PartialParamCodecLowPriority.JsonLowPriority1 {

    // =====| Givens |=====

    given optional: [A: JsonSchema as schema] => PartialParamCodec.Json[Option[A]] = PartialParamCodec.Json.Optional(schema)
    given manyNonEmpty: [A: JsonSchema as schema] => PartialParamCodec.Json[NonEmptyList[A]] = PartialParamCodec.Json.ManyNonEmpty(schema)

    // =====| Instances |=====

    final case class Required[A](schema: JsonSchema[A]) extends Json[A] {

      override val partialParamSchema: PartialParamSchema = PartialParamSchema(schema, ParamType.Param.Required)

      override def decode(params: List[String]): Either[DecodingFailureCause, A] =
        params match
          case Nil          => DecodingFailureCause.MissingRequired.asLeft
          case param :: Nil => schema.decode(param).leftMap(DecodingFailureCause.DecodeError(_, DecodingFailureCause.DecodeInput.InputValues(params)))
          case _            => DecodingFailureCause.ManyNotSupported(params.size).asLeft

      override def encode(value: A): List[String] =
        schema.encode(value) :: Nil

    }

    final case class Optional[A](schema: JsonSchema[A]) extends Json[Option[A]] {

      override val partialParamSchema: PartialParamSchema = PartialParamSchema(schema, ParamType.Param.Optional)

      override def decode(params: List[String]): Either[DecodingFailureCause, Option[A]] =
        params match
          case Nil          => None.asRight
          case param :: Nil => schema.decode(param).bimap(DecodingFailureCause.DecodeError(_, DecodingFailureCause.DecodeInput.InputValues(params)), _.some)
          case _            => DecodingFailureCause.ManyNotSupported(params.size).asLeft

      override def encode(value: Option[A]): List[String] =
        value.map(schema.encode).toList

    }

    final case class Many[S[_]: SeqOps as seqOps, A](schema: JsonSchema[A]) extends Json[S[A]] {

      override val partialParamSchema: PartialParamSchema = PartialParamSchema(schema, ParamType.Param.ManyOptional)

      override def decode(params: List[String]): Either[DecodingFailureCause, S[A]] =
        params.into[S].traverse(schema.decode).leftMap(DecodingFailureCause.DecodeError(_, DecodingFailureCause.DecodeInput.InputValues(params)))

      override def encode(value: S[A]): List[String] =
        value.map(schema.encode).into[List]

    }

    final case class ManyNonEmpty[A](schema: JsonSchema[A]) extends Json[NonEmptyList[A]] {

      override val partialParamSchema: PartialParamSchema = PartialParamSchema(schema, ParamType.Param.ManyRequired)

      override def decode(params: List[String]): Either[DecodingFailureCause, NonEmptyList[A]] =
        NonEmptyList
          .fromList(params)
          .toRight(DecodingFailureCause.MissingRequired)
          .flatMap(_.traverse(schema.decode).leftMap(DecodingFailureCause.DecodeError(_, DecodingFailureCause.DecodeInput.InputValues(params))))

      override def encode(value: NonEmptyList[A]): List[String] =
        value.toList.map(schema.encode)

    }

  }

}

object PartialParamCodecLowPriority {

  // =====| PartialParamCodec |=====

  trait LowPriority1 extends LowPriority2 {

    given fromPlain: [A: PartialParamCodec.Plain as codec] => PartialParamCodec[A] = codec

  }

  trait LowPriority2 {

    given fromJson: [A: PartialParamCodec.Json as codec] => PartialParamCodec[A] = codec

  }

  // =====| PartialParamCodec.Plain |=====

  trait PlainLowPriority1 extends PlainLowPriority2 {

    given many: [S[_]: SeqOps, A: PlainTextSchema as schema] => PartialParamCodec.Plain[S[A]] = PartialParamCodec.Plain.ManyOptional(schema)

  }

  trait PlainLowPriority2 {

    given required: [A: PlainTextSchema as schema] => PartialParamCodec.Plain[A] = PartialParamCodec.Plain.Required(schema)

  }

  // =====| PartialParamCodec.Json |=====

  trait JsonLowPriority1 extends JsonLowPriority2 {

    given many: [S[_]: SeqOps, A: JsonSchema as schema] => PartialParamCodec.Json[S[A]] = PartialParamCodec.Json.Many(schema)

  }

  trait JsonLowPriority2 {

    given required: [A: JsonSchema as schema] => PartialParamCodec.Json[A] = PartialParamCodec.Json.Required(schema)

  }

}
