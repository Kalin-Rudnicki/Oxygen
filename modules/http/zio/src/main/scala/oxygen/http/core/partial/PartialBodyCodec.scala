package oxygen.http.core.partial

import oxygen.http.core.{BodyUtil, DecodingFailureCause, ReadOnlyCachedHttpBody, ZioHttpCompat}
import oxygen.http.schema.partial.PartialBodySchema
import oxygen.predef.core.*
import oxygen.schema.*
import zio.*
import zio.http.{Body, MediaType, ServerSentEvent}
import zio.stream.*

sealed trait PartialBodyCodec[A] {

  val partialBodySchema: PartialBodySchema

  def decode(body: ReadOnlyCachedHttpBody): ZIO[Scope, DecodingFailureCause, A]
  def encode(value: A): Body

  final def withMediaType(mediaType: MediaType): PartialBodyCodec[A] = PartialBodyCodec.WithMediaType(this, mediaType)

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

    override def decode(body: ReadOnlyCachedHttpBody): ZIO[Scope, DecodingFailureCause, Unit] = ZIO.unit

    override def encode(value: Unit): Body = Body.empty

  }

  final case class WithMediaType[A](
      underlying: PartialBodyCodec[A],
      mediaType: MediaType,
  ) extends PartialBodyCodec[A] {
    override val partialBodySchema: PartialBodySchema = underlying.partialBodySchema
    override def decode(body: ReadOnlyCachedHttpBody): ZIO[Scope, DecodingFailureCause, A] = underlying.decode(body)
    override def encode(value: A): Body = underlying.encode(value).contentType(mediaType)
  }

  sealed trait Single[A] extends PartialBodyCodec[A] {
    override val partialBodySchema: PartialBodySchema.Single
  }

  final case class Plain[A](schema: PlainTextSchema[A]) extends PartialBodyCodec.Single[A] {

    override val partialBodySchema: PartialBodySchema.Single = PartialBodySchema.Single(schema)

    override def decode(body: ReadOnlyCachedHttpBody): ZIO[Scope, DecodingFailureCause, A] =
      body.asString
        .mapError(DecodingFailureCause.ExecutionFailure(_))
        .flatMap { stringBody =>
          ZIO.fromEither(schema.decode(stringBody)).mapError(DecodingFailureCause.DecodeError(_, DecodingFailureCause.DecodeInput.Body(stringBody)))
        }

    override def encode(value: A): Body =
      BodyUtil.fromString(schema.encode(value), MediaType.text.plain)

  }
  object Plain {
    given fromSchema: [A: PlainTextSchema as schema] => PartialBodyCodec.Plain[A] = PartialBodyCodec.Plain(schema)
  }

  final case class Json[A](schema: JsonSchema[A]) extends PartialBodyCodec.Single[A] {

    override val partialBodySchema: PartialBodySchema.Single = PartialBodySchema.Single(schema)

    override def decode(body: ReadOnlyCachedHttpBody): ZIO[Scope, DecodingFailureCause, A] =
      body.asString
        .mapError(DecodingFailureCause.ExecutionFailure(_))
        .flatMap { stringBody => ZIO.fromEither(schema.decode(stringBody).leftMap(DecodingFailureCause.DecodeError(_, DecodingFailureCause.DecodeInput.Body(stringBody)))) }

    override def encode(value: A): Body =
      BodyUtil.fromString(schema.encode(value), MediaType.application.json)

  }
  object Json {
    given fromSchema: [A: JsonSchema as schema] => PartialBodyCodec.Json[A] = PartialBodyCodec.Json(schema)
  }

  final case class ServerSentEvents[A](schema: AnySchemaT[A]) extends PartialBodyCodec[Stream[DecodingFailureCause, ServerSentEvent[A]]] {

    override val partialBodySchema: PartialBodySchema.ServerSentEvents = PartialBodySchema.ServerSentEvents(schema)

    override def decode(body: ReadOnlyCachedHttpBody): ZIO[Scope, DecodingFailureCause, Stream[DecodingFailureCause, ServerSentEvent[A]]] =
      ZIO.succeed {
        val rawSSE: Stream[Throwable, ServerSentEvent[String]] =
          body.asStream >>> ZioHttpCompat.rawEventBinaryCodec.streamDecoder
        val decodedSSE: Stream[DecodingFailureCause, ServerSentEvent[A]] =
          rawSSE.mapError { e => DecodingFailureCause.ExecutionFailure(e) }.mapZIO { event =>
            schema.decode(event.data) match {
              case Right(value) => ZIO.succeed(event.copy(data = value))
              case Left(error)  => ZIO.fail(DecodingFailureCause.DecodeError(error, DecodingFailureCause.DecodeInput.BodySSE(event.data)))
            }
          }

        decodedSSE
      }

    override def encode(value: Stream[DecodingFailureCause, ServerSentEvent[A]]): Body = {
      val encodedSSE: Stream[Throwable, ServerSentEvent[String]] =
        value
          .orDieWith { c => new RuntimeException(c.toString) } // really its only the responses that can fail
          .map { event => event.copy(data = schema.encode(event.data)) }
      val rawBytes: Stream[Throwable, Byte] =
        encodedSSE >>> ZioHttpCompat.rawEventBinaryCodec.streamEncoder

      Body.fromStreamChunked(rawBytes).contentType(MediaType.text.`event-stream`)
    }

  }

  final case class LineStream[A](schema: AnySchemaT[A]) extends PartialBodyCodec[Stream[DecodingFailureCause, A]] {

    override val partialBodySchema: PartialBodySchema.ServerSentEvents = PartialBodySchema.ServerSentEvents(schema)

    override def decode(body: ReadOnlyCachedHttpBody): ZIO[Scope, DecodingFailureCause, Stream[DecodingFailureCause, A]] =
      ZIO.succeed {
        val rawLines: Stream[Throwable, String] =
          body.asStream >>> ZPipeline.utf8Decode >>> ZPipeline.splitLines
        val decodedSSE: Stream[DecodingFailureCause, A] =
          rawLines.mapError { e => DecodingFailureCause.ExecutionFailure(e) }.mapZIO { event =>
            schema.decode(event) match {
              case Right(value) => ZIO.succeed(value)
              case Left(error)  => ZIO.fail(DecodingFailureCause.DecodeError(error, DecodingFailureCause.DecodeInput.BodySSE(event)))
            }
          }

        decodedSSE
      }

    override def encode(value: Stream[DecodingFailureCause, A]): Body = {
      val encodedLines: Stream[Throwable, String] =
        value
          .orDieWith { c => new RuntimeException(c.toString) } // really its only the responses that can fail
          .map { event => schema.encode(event) }
      val rawBytes: Stream[Throwable, Byte] =
        (encodedLines >>> ZPipeline.intersperse("\n")) >>> ZPipeline.utf8Encode

      Body.fromStreamChunked(rawBytes).contentType(MediaType.text.`event-stream`)
    }

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
