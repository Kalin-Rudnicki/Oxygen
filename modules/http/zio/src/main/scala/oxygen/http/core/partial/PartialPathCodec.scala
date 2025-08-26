package oxygen.http.core.partial

import oxygen.http.schema.ParamType
import oxygen.http.schema.partial.PartialPathSchema
import oxygen.predef.core.*
import oxygen.schema.*

sealed trait PartialPathCodec[A] {

  val partialPathSchema: PartialPathSchema

  def decode(paths: List[String]): Option[(A, List[String])]

  def encode(value: A): Growable[String]

}
object PartialPathCodec extends PartialPathCodecLowPriority.LowPriority1 {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Plain
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Plain[A] extends PartialPathCodec[A]
  object Plain extends PartialPathCodecLowPriority.PlainLowPriority1 {

    // =====| Givens |=====

    given singleString: PartialPathCodec.Plain[String] = PartialPathCodec.Plain.SingleString
    given restString: PartialPathCodec.Plain[List[String]] = PartialPathCodec.Plain.RestString
    given nonEmptyRestString: PartialPathCodec.Plain[NonEmptyList[String]] = PartialPathCodec.Plain.NonEmptyRestString

    // =====| Instances |=====

    case object SingleString extends PartialPathCodec.Plain[String] {

      override val partialPathSchema: PartialPathSchema = PartialPathSchema(PlainTextSchema.string, ParamType.Path.Single)

      override def encode(value: String): Growable[String] = Growable.single(value)

      override def decode(paths: List[String]): Option[(String, List[String])] = paths match
        case head :: tail => (head, tail).some
        case _            => None

    }

    final case class SingleEncoded[A](schema: PlainTextSchema[A]) extends PartialPathCodec.Plain[A] {

      override val partialPathSchema: PartialPathSchema = PartialPathSchema(schema, ParamType.Path.Single)

      override def decode(paths: List[String]): Option[(A, List[String])] = paths match
        case head :: tail => schema.decode(head).toOption.map((_, tail))
        case _            => None

      override def encode(value: A): Growable[String] =
        Growable.single(schema.encode(value))

    }

    case object RestString extends PartialPathCodec.Plain[List[String]] {

      override val partialPathSchema: PartialPathSchema = PartialPathSchema(PlainTextSchema.string, ParamType.Path.Rest)

      override def decode(paths: List[String]): Option[(List[String], List[String])] =
        (paths, Nil).some

      override def encode(value: List[String]): Growable[String] =
        Growable.many(value)

    }

    final case class RestEncoded[A](schema: PlainTextSchema[A]) extends PartialPathCodec.Plain[List[A]] {

      override val partialPathSchema: PartialPathSchema = PartialPathSchema(schema, ParamType.Path.Rest)

      override def decode(paths: List[String]): Option[(List[A], List[String])] =
        paths.traverse(schema.decode(_).toOption).map((_, Nil))

      override def encode(value: List[A]): Growable[String] =
        Growable.many(value).map(schema.encode)

    }

    case object NonEmptyRestString extends PartialPathCodec.Plain[NonEmptyList[String]] {

      override val partialPathSchema: PartialPathSchema = PartialPathSchema(PlainTextSchema.string, ParamType.Path.NonEmptyRest)

      override def decode(paths: List[String]): Option[(NonEmptyList[String], List[String])] =
        NonEmptyList.fromList(paths).map((_, Nil))

      override def encode(value: NonEmptyList[String]): Growable[String] =
        Growable.many(value)

    }

    final case class NonEmptyRestEncoded[A](schema: PlainTextSchema[A]) extends PartialPathCodec.Plain[NonEmptyList[A]] {

      override val partialPathSchema: PartialPathSchema = PartialPathSchema(schema, ParamType.Path.NonEmptyRest)

      override def decode(paths: List[String]): Option[(NonEmptyList[A], List[String])] =
        NonEmptyList.fromList(paths).flatMap(_.traverse(schema.decode(_).toOption)).map((_, Nil))

      override def encode(value: NonEmptyList[A]): Growable[String] =
        Growable.many(value).map(schema.encode)

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Json
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Json[A] extends PartialPathCodec[A]
  object Json extends PartialPathCodecLowPriority.JsonLowPriority1 {

    // =====| Givens |=====

    given restEncoded: [A: JsonSchema as schema] => PartialPathCodec.Json[List[A]] = PartialPathCodec.Json.RestEncoded(schema)
    given nonEmptyRestEncoded: [A: JsonSchema as schema] => PartialPathCodec.Json[NonEmptyList[A]] = PartialPathCodec.Json.NonEmptyRestEncoded(schema)

    // =====| Instances |=====

    final case class SingleEncoded[A](schema: JsonSchema[A]) extends PartialPathCodec.Json[A] {

      override val partialPathSchema: PartialPathSchema = PartialPathSchema(schema, ParamType.Path.Single)

      override def decode(paths: List[String]): Option[(A, List[String])] = paths match
        case head :: tail => schema.decode(head).toOption.map((_, tail))
        case _            => None

      override def encode(value: A): Growable[String] =
        Growable.single(schema.encode(value))

    }

    final case class RestEncoded[A](schema: JsonSchema[A]) extends PartialPathCodec.Json[List[A]] {

      override val partialPathSchema: PartialPathSchema = PartialPathSchema(schema, ParamType.Path.Rest)

      override def decode(paths: List[String]): Option[(List[A], List[String])] =
        paths.traverse(schema.decode(_).toOption).map((_, Nil))

      override def encode(value: List[A]): Growable[String] =
        Growable.many(value).map(schema.encode)

    }

    final case class NonEmptyRestEncoded[A](schema: JsonSchema[A]) extends PartialPathCodec.Json[NonEmptyList[A]] {

      override val partialPathSchema: PartialPathSchema = PartialPathSchema(schema, ParamType.Path.NonEmptyRest)

      override def decode(paths: List[String]): Option[(NonEmptyList[A], List[String])] =
        NonEmptyList.fromList(paths).flatMap(_.traverse(schema.decode(_).toOption)).map((_, Nil))

      override def encode(value: NonEmptyList[A]): Growable[String] =
        Growable.many(value).map(schema.encode)

    }

  }

}

object PartialPathCodecLowPriority {

  // =====| PartialPathCodec |=====

  trait LowPriority1 extends LowPriority2 {

    given fromPlain: [A: PartialPathCodec.Plain as codec] => PartialPathCodec[A] = codec

  }

  trait LowPriority2 {

    given fromJson: [A: PartialPathCodec.Json as codec] => PartialPathCodec[A] = codec

  }

  // =====| PartialPathCodec.Plain |=====

  trait PlainLowPriority1 extends PlainLowPriority2 {

    given restEncoded: [A: PlainTextSchema as schema] => PartialPathCodec.Plain[List[A]] = PartialPathCodec.Plain.RestEncoded(schema)
    given nonEmptyRestEncoded: [A: PlainTextSchema as schema] => PartialPathCodec.Plain[NonEmptyList[A]] = PartialPathCodec.Plain.NonEmptyRestEncoded(schema)

  }

  trait PlainLowPriority2 {

    given singleEncoded: [A: PlainTextSchema as schema] => PartialPathCodec.Plain[A] = PartialPathCodec.Plain.SingleEncoded(schema)

  }

  // =====| PartialPathCodec.Json |=====

  trait JsonLowPriority1 {

    given singleEncoded: [A: JsonSchema as schema] => PartialPathCodec.Json[A] = PartialPathCodec.Json.SingleEncoded(schema)

  }

}
