package oxygen.http.core

import oxygen.predef.core.*
import oxygen.schema.*
import zio.*
import zio.http.*
import zio.schema.codec.{BinaryCodec, DecodeError}
import zio.stream.ZPipeline

object ZioHttpCompat {

  private val standardHeaderTypes: Set[Header.HeaderType] =
    Set(
      Header.ContentLength,
      Header.ContentEncoding,
      Header.ContentTransferEncoding,
      Header.AcceptEncoding,
      Header.Connection,
      Header.Server,
      Header.Date,
    )

  def filterStandardHeaders(headers: Headers): Headers =
    Headers.fromIterable { headers.filterNot { h => standardHeaderTypes.contains(h.headerType) } }

  // TODO (KR) : remove if ZIO-http merges and publishes my changes
  private def encodeSSE[A](event: ServerSentEvent[A], f: A => String): String = {
    val dataString: String = f(event.data)

    val dataLines: Array[String] = dataString.split("\n")
    val isComment = dataString.startsWith(":")

    val initialCapacity: Int =
      (
        // 6 for "data: ", the data itself, and the newlines
        ((if isComment then 0 else 6) + dataString.length + dataLines.length)
        // 24 because 7 for "event: ", 1 for the newline, 16 for the event type itself
          + (if event.eventType.isEmpty then 0 else 24)
          // 21 because 4 for "id: ", 1 for the newline, 16 for the id itself
          + (if event.id.isEmpty then 0 else 21)
          // 24 because 7 for "retry: ", 1 for the newline, 16 for the retry value
          + (if event.retry.isEmpty then 0 else 24)
          // for the final newline
          + 1
      )

    val sb = new java.lang.StringBuilder(initialCapacity)
    event.eventType.foreach { et =>
      sb.append("event: ")
      val iterator = et.linesIterator
      var hasNext = iterator.hasNext
      while hasNext do {
        sb.append(iterator.next())
        hasNext = iterator.hasNext
        if hasNext then sb.append(' ')
      }
      sb.append('\n')
    }
    dataLines.foreach { line =>
      if isComment then sb.append(line).append('\n')
      else sb.append("data: ").append(line).append('\n')
    }
    event.id.foreach { i =>
      sb.append("id: ")
      val iterator = i.linesIterator
      var hasNext = iterator.hasNext
      while hasNext do {
        sb.append(iterator.next())
        hasNext = iterator.hasNext
        if hasNext then sb.append(' ')
      }
      sb.append('\n')
    }
    event.retry.foreach { r =>
      sb.append("retry: ").append(r.toMillis).append('\n')
    }
    sb.append('\n').toString
  }

  // TODO (KR) : remove if ZIO-http merges and publishes my changes
  val rawEventLinesBinaryCodec: BinaryCodec[ServerSentEvent[Chunk[String]]] =
    new BinaryCodec[ServerSentEvent[Chunk[String]]] {

      override def decode(whole: Chunk[Byte]): Either[DecodeError, ServerSentEvent[Chunk[String]]] = {
        val event = processEvent(Chunk.fromArray(whole.asString(Charsets.Utf8).split("\n")))
        if event.data.isEmpty && event.retry.isEmpty then DecodeError.EmptyContent("Neither 'data' nor 'retry' fields specified").asLeft
        else event.asRight
      }

      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, ServerSentEvent[Chunk[String]]] =
        ZPipeline.utf8Decode.orDie >>>
          ZPipeline.splitOn("\n") >>>
          ZPipeline
            .scan[String, (Boolean, Chunk[String])](false -> Chunk.empty) {
              case ((true, _), "")        => true -> Chunk.empty
              case ((true, _), line)      => false -> Chunk(line)
              case ((false, lines), "")   => true -> lines
              case ((false, lines), line) => false -> (lines :+ line)
            }
            .filter { case (completed, event) => completed && event.nonEmpty }
            .map { case (_, lines) => processEvent(lines) }
            .filter(event => event.data.nonEmpty || event.retry.nonEmpty)

      private def processEvent(lines: Chunk[String]): ServerSentEvent[Chunk[String]] =
        lines.foldLeft(ServerSentEvent(data = Chunk.empty[String])) { case (event, line) =>
          val fieldType = "(data|event|id|retry)(:|$)".r.findPrefixOf(line)
          fieldType match {
            case Some("data:")  => event.copy(data = event.data :+ line.replaceFirst("data: ?", ""))
            case Some("data")   => event.copy(data = event.data :+ "")
            case Some("event:") =>
              event.copy(eventType = Some(line.replaceFirst("event: ?", "")).filter(_.nonEmpty))
            case Some("event")  => event.copy(eventType = None)
            case Some("retry:") =>
              event.copy(retry = line.replaceFirst("retry: ?", "").toIntOption.filter(_ >= 0).map(_.milliseconds))
            case Some("retry") => event.copy(retry = None)
            case Some("id:")   => event.copy(id = Some(line.replaceFirst("id: ?", "")).filter(_.nonEmpty))
            case Some("id")    => event.copy(id = None)
            case _             => event
          }
        }

      override def encode(value: ServerSentEvent[Chunk[String]]): Chunk[Byte] =
        Chunk.fromArray(encodeSSE(value, _.mkString("\n")).getBytes(Charsets.Utf8))

      override def streamEncoder: ZPipeline[Any, Nothing, ServerSentEvent[Chunk[String]], Byte] =
        ZPipeline.mapChunks(value => value.flatMap(encodeSSE(_, _.mkString("\n")).getBytes(Charsets.Utf8)))

    }

  val rawEventBinaryCodec: BinaryCodec[ServerSentEvent[String]] =
    new BinaryCodec[ServerSentEvent[String]] {

      override def decode(whole: Chunk[Byte]): Either[DecodeError, ServerSentEvent[String]] = rawEventLinesBinaryCodec.decode(whole).map { e => e.copy(data = e.data.mkString("\n")) }

      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, ServerSentEvent[String]] = rawEventLinesBinaryCodec.streamDecoder.map { e => e.copy(data = e.data.mkString("\n")) }

      override def encode(value: ServerSentEvent[String]): Chunk[Byte] = Chunk.fromArray(encodeSSE(value, identity).getBytes(Charsets.Utf8))

      override def streamEncoder: ZPipeline[Any, Nothing, ServerSentEvent[String], Byte] = ZPipeline.mapChunks(value => value.flatMap(encodeSSE(_, identity).getBytes(Charsets.Utf8)))

    }

  // TODO (KR) : remove if ZIO-schema merges and publishes my changes
  extension [A](self: BinaryCodec[A]) {

    def transform[B](ab: A => B, ba: B => A): BinaryCodec[B] =
      new BinaryCodec[B] {
        override def decode(whole: Chunk[Byte]): Either[DecodeError, B] = self.decode(whole).map(ab)
        override def streamDecoder: ZPipeline[Any, DecodeError, Byte, B] = self.streamDecoder.map(ab)
        override def encode(value: B): Chunk[Byte] = self.encode(ba(value))
        override def streamEncoder: ZPipeline[Any, Nothing, B, Byte] = self.streamEncoder.contramap(ba)
      }

    def transformOrFail[B](ab: A => Either[String, B], ba: B => A): BinaryCodec[B] =
      new BinaryCodec[B] {
        override def decode(whole: Chunk[Byte]): Either[DecodeError, B] = self.decode(whole).flatMap(ab(_).leftMap(DecodeError.ReadError(Cause.Empty, _)))
        override def streamDecoder: ZPipeline[Any, DecodeError, Byte, B] = self.streamDecoder.mapZIO { a => ZIO.fromEither { ab(a).leftMap(DecodeError.ReadError(Cause.Empty, _)) } }
        override def encode(value: B): Chunk[Byte] = self.encode(ba(value))
        override def streamEncoder: ZPipeline[Any, Nothing, B, Byte] = self.streamEncoder.contramap(ba)
      }

  }

  def sseBinaryCodecFromOxygenSchema[A](schema: AnySchemaT[A]): BinaryCodec[ServerSentEvent[A]] =
    rawEventBinaryCodec.transformOrFail(
      e => schema.decode(e.data).map { a => e.copy(data = a) },
      e => e.copy(data = schema.encode(e.data)),
    )

  extension (self: Body)
    def optMediaType(tpe: Option[MediaType]): Body = tpe match
      case Some(tpe) => self.contentType(tpe)
      case None      => self

}
