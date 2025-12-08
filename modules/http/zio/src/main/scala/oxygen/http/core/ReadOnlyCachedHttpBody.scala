package oxygen.http.core

import oxygen.http.core.ReadOnlyCachedHttpBody.CacheStatus
import zio.*
import zio.http.{Body, Boundary, Charsets, Form, ServerSentEvent, StreamingForm}
import zio.http.multipart.mixed.MultipartMixed
import zio.json.JsonDecoder
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.stream.ZStream

/**
  * This class asserts that there are two things you want to do with a http body:
  * A. Read it all at once
  * B. Stream it
  *
  * It supports:
  * 1. If you do (A), you can do (A) as many times as you want after that.
  * 2. If you do (A), (B) should be allowed by succeeding with the cached stream bytes
  * 3. If you do (B) before (A), you can never again do (A)
  * 4. If you do (B) before (A), you can never again do (B)
  */
final class ReadOnlyCachedHttpBody(
    rawBody: Body,
    cached: Ref[ReadOnlyCachedHttpBody.CacheStatus],
) extends Body {

  def getCache: UIO[ReadOnlyCachedHttpBody.CacheStatus] = cached.get

  def optionalCachedBody: Task[Option[String]] =
    cached.get.flatMap {
      case CacheStatus.Unused        => asString.asSome
      case CacheStatus.Cached(bytes) => ZIO.some(new String(bytes, Charsets.Http))
      case CacheStatus.Invalid       => ZIO.none
    }

  ///////  ///////////////////////////////////////////////////////////////

  private def constByteArray[A](f: Array[Byte] => Task[A]): Task[A] =
    cached.get.flatMap {
      case CacheStatus.Unused        => asArray.tap { bytes => cached.set(CacheStatus.Cached(bytes)) }.flatMap(f)
      case CacheStatus.Cached(bytes) => f(bytes)
      case CacheStatus.Invalid       => ZIO.fail(new RuntimeException("Attempted to read http bytes after non-recoverable operation (like streaming)"))
    }

  private def markNonRecoverable: Task[Unit] =
    cached.get.flatMap {
      case CacheStatus.Unused    => cached.set(CacheStatus.Invalid)
      case CacheStatus.Cached(_) => ZIO.fail(new RuntimeException("HTTP body already used"))
      case CacheStatus.Invalid   => ZIO.fail(new RuntimeException("HTTP body already used"))
    }

  private def notSupported(action: String): Throwable = new RuntimeException(s"`ReadOnlyCachedHttpBody` does not support action: $action")

  private lazy val httpBoundary: Option[Boundary] = contentType.flatMap(_.boundary)

  ///////  ///////////////////////////////////////////////////////////////

  override def asArray(using trace: Trace): Task[Array[Byte]] =
    constByteArray(ZIO.succeed(_))

  override def asChunk(using trace: Trace): Task[Chunk[Byte]] =
    constByteArray { bytes => ZIO.succeed(Chunk.fromArray(bytes)) }

  override def asJson[A](using schema: Schema[A], trace: Trace): Task[A] = {
    import zio.schema.codec.JsonCodec
    to[A](using JsonCodec.schemaBasedBinaryCodec[A])
  }

  override def asJsonFromCodec[A](using decoder: JsonDecoder[A], trace: Trace): Task[A] =
    asString.flatMap { str =>
      ZIO
        .fromEither(decoder.decodeJson(str))
        .mapError(err => new RuntimeException(s"Failed to decode JSON: $err"))
    }

  override def asMultipartForm(using trace: Trace): Task[Form] =
    httpBoundary match {
      case Some(boundary) =>
        StreamingForm(asStream, boundary).collectAll
      case _ =>
        for {
          bytes <- asChunk
          form <- Form.fromMultipartBytes(bytes, Charsets.Http, None)
        } yield form
    }

  override def asMultipartFormStream(using trace: Trace): Task[StreamingForm] =
    httpBoundary match {
      case Some(boundary) => markNonRecoverable.as(StreamingForm(asStream, boundary))
      case None           => ZIO.fail(new IllegalStateException("Cannot decode body as streaming multipart/form-data without a known boundary"))
    }

  override def asMultipartMixed(using trace: Trace): Task[MultipartMixed] =
    ZIO
      .fromOption { MultipartMixed.fromBody(this) }
      .orElseFail(new IllegalStateException("Cannot decode body as multipart/mixed without a known boundary"))

  override def asServerSentEvents[T: Schema](using trace: Trace): ZStream[Any, Throwable, ServerSentEvent[T]] = {
    val codec = ServerSentEvent.defaultBinaryCodec[T]
    asStream >>> codec.streamDecoder
  }

  override def asURLEncodedForm(using trace: Trace): Task[Form] =
    asString.flatMap(string => ZIO.fromEither(Form.fromURLEncoded(string, Charsets.Http)))

  override def materialize(using trace: Trace): UIO[Body] =
    ZIO.succeed(this)

  override def to[A](using codec: BinaryCodec[A], trace: Trace): Task[A] =
    asChunk.flatMap(bytes => ZIO.fromEither(codec.decode(bytes)))

  override def asStream(using trace: Trace): ZStream[Any, Throwable, Byte] =
    ZStream.fromZIO(cached.get).flatMap {
      case CacheStatus.Unused        => ZStream.fromZIO(markNonRecoverable) *> rawBody.asStream
      case CacheStatus.Cached(bytes) => ZStream.fromChunk(Chunk.fromArray(bytes))
      case CacheStatus.Invalid       => ZStream.fail(new RuntimeException("HTTP body already used"))
    }

  override def contentType: Option[Body.ContentType] =
    rawBody.contentType

  override def contentType(newContentType: Body.ContentType): Body =
    throw notSupported("set content type")

  override def isComplete: Boolean =
    rawBody.isComplete

  override def isEmpty: Boolean =
    rawBody.isEmpty

  override def knownContentLength: Option[Long] =
    rawBody.knownContentLength

}
object ReadOnlyCachedHttpBody {

  enum CacheStatus {
    case Unused
    case Cached(bytes: Array[Byte])
    case Invalid
  }

}
