package oxygen.http.core.partial

import oxygen.http.core.DecodingFailureCause
import oxygen.http.schema.partial.PartialBodySchema
import oxygen.predef.core.*
import oxygen.schema.*
import zio.*
import zio.http.{Body, MediaType}

sealed trait PartialBodyCodec[A] {

  val partialBodySchema: PartialBodySchema

  def decode(body: Body): ZIO[Scope, DecodingFailureCause, A]
  def encode(value: A): Body

}
object PartialBodyCodec extends PartialBodyCodecLowPriority.LowPriority1 {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given empty: PartialBodyCodec[Unit] = PartialBodyCodec.Empty

  // TODO (KR) : multipart

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  case object Empty extends PartialBodyCodec[Unit] {

    override val partialBodySchema: PartialBodySchema = PartialBodySchema.Empty

    override def decode(body: Body): ZIO[Scope, DecodingFailureCause, Unit] = ZIO.unit

    override def encode(value: Unit): Body = Body.empty

  }

  sealed trait Single[A] extends PartialBodyCodec[A] {
    override val partialBodySchema: PartialBodySchema.Single
  }

  final case class Plain[A](schema: PlainTextSchema[A]) extends PartialBodyCodec.Single[A] {

    override val partialBodySchema: PartialBodySchema.Single = PartialBodySchema.Single(schema)

    override def decode(body: Body): ZIO[Scope, DecodingFailureCause, A] =
      body.asString
        .mapError(DecodingFailureCause.ExecutionFailure(_))
        .flatMap { stringBody => ZIO.fromEither(schema.decode(stringBody)).mapError(DecodingFailureCause.DecodeError(_)) }

    override def encode(value: A): Body =
      Body.fromString(schema.encode(value)).contentType(Body.ContentType(MediaType.text.plain))

  }
  object Plain {
    given fromSchema: [A: PlainTextSchema as schema] => PartialBodyCodec.Plain[A] = PartialBodyCodec.Plain(schema)
  }

  final case class Json[A](schema: JsonSchema[A]) extends PartialBodyCodec.Single[A] {

    override val partialBodySchema: PartialBodySchema.Single = PartialBodySchema.Single(schema)

    override def decode(body: Body): ZIO[Scope, DecodingFailureCause, A] =
      body.asString
        .mapError(DecodingFailureCause.ExecutionFailure(_))
        .flatMap { stringBody => ZIO.fromEither(schema.decode(stringBody).leftMap(DecodingFailureCause.DecodeError(_))) }

    override def encode(value: A): Body =
      Body.fromString(schema.encode(value)).contentType(Body.ContentType(MediaType.application.json))

  }
  object Json {
    given fromSchema: [A: JsonSchema as schema] => PartialBodyCodec.Json[A] = PartialBodyCodec.Json(schema)
  }

}

object PartialBodyCodecLowPriority {

  trait LowPriority1 extends LowPriority2 {

    given fromPlain: [A: PartialBodyCodec.Plain as codec] => PartialBodyCodec[A] = codec

  }

  trait LowPriority2 {

    given fromJson: [A: PartialBodyCodec.Json as codec] => PartialBodyCodec[A] = codec

  }

}
