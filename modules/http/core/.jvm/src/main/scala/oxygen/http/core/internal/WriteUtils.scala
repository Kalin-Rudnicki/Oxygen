package oxygen.http.core.internal

import java.io.OutputStream
import java.net.*
import java.nio.charset.{Charset, StandardCharsets}
import oxygen.http.model.*
import oxygen.predef.core.*
import zio.*

object WriteUtils {

  private def writeContentTypeAndLength(
      bufferedOutputStream: OutputStream,
  )(
      contentType: ContentType,
      charset: Charset,
      contentLength: Option[Long],
  ): Unit = {
    bufferedOutputStream.write(HttpBytes.PartialHeaders.contentType)
    bufferedOutputStream.write(contentType.showBytes)
    bufferedOutputStream.write(HttpBytes.startCharset)
    bufferedOutputStream.write(charset.name.getBytes)
    bufferedOutputStream.write(HttpBytes.newline)
    contentLength match {
      case Some(contentLength) =>
        bufferedOutputStream.write(HttpBytes.PartialHeaders.contentLength)
        bufferedOutputStream.write(contentLength.toString.getBytes)
      case None =>
        bufferedOutputStream.write(HttpBytes.FullHeaders.transferEncodingChunked)
    }
    bufferedOutputStream.write(HttpBytes.newline)
    bufferedOutputStream.write(HttpBytes.newline)
    bufferedOutputStream.flush()
  }

  def writeHeaders(
      bufferedOutputStream: OutputStream,
  )(
      headers: Headers,
  ): Unit =
    if (headers.rawHeaders.nonEmpty) {
      val headersIter: Iterator[(String, String)] = headers.rawHeaders.iterator
      var tup: (String, String) = null

      while (headersIter.hasNext) {
        tup = headersIter.next()

        bufferedOutputStream.write(URLEncoder.encode(tup._1, StandardCharsets.UTF_8).getBytes)
        bufferedOutputStream.write(HttpBytes.colon)
        bufferedOutputStream.write(URLEncoder.encode(tup._2, StandardCharsets.UTF_8).getBytes)
        bufferedOutputStream.write(HttpBytes.newline)
      }
    }

  def writeBodyHeadersAndBody(
      rawOutputStream: OutputStream,
      bufferedOutputStream: OutputStream,
  )(
      body: HttpBody,
      charset: Charset,
      streamChunkSize: Int,
  ): URIO[Scope, Unit] =
    body match {
      case HttpBody.Empty =>
        bufferedOutputStream.write(HttpBytes.newline)
        bufferedOutputStream.flush()
        ZIO.unit

      case HttpBody.Text(body, contentType) =>
        val bodyBytes: Array[Byte] = body.getBytes

        writeContentTypeAndLength(bufferedOutputStream)(contentType, charset, bodyBytes.length.some)
        bufferedOutputStream.flush()

        rawOutputStream.write(bodyBytes)
        rawOutputStream.flush()
        ZIO.unit

      case HttpBody.Bytes(bodyBytes, contentType, charset) =>
        writeContentTypeAndLength(bufferedOutputStream)(contentType, charset, bodyBytes.length.some)
        bufferedOutputStream.write(HttpBytes.newline)
        bufferedOutputStream.flush()

        rawOutputStream.write(bodyBytes)
        rawOutputStream.flush()
        ZIO.unit

      case HttpBody.Stream(stream, contentType, charset, cl @ Some(_)) =>
        writeContentTypeAndLength(bufferedOutputStream)(contentType, charset, cl)
        bufferedOutputStream.write(HttpBytes.newline)
        bufferedOutputStream.flush()

        stream.map { stream =>
          stream.transferTo(rawOutputStream)
          rawOutputStream.flush()
        }

      case HttpBody.Stream(stream, contentType, charset, None) =>
        writeContentTypeAndLength(bufferedOutputStream)(contentType, charset, None)
        bufferedOutputStream.write(HttpBytes.newline)
        bufferedOutputStream.flush()

        val transferBytes: Array[Byte] = new Array[Byte](streamChunkSize)

        stream.map { stream =>
          var numRead: Int = 0
          while ({ numRead = stream.read(transferBytes, 0, streamChunkSize); numRead >= 0 }) {
            if (numRead > 0) { // why would it return 0? what to do if it does? see: InputStream.transferTo
              rawOutputStream.write(numRead.toHexString.getBytes)
              rawOutputStream.write(HttpBytes.newline)
              rawOutputStream.write(transferBytes, 0, numRead)
              rawOutputStream.write(HttpBytes.newline)
            } else
              Thread.sleep(10, 0)
          }

          rawOutputStream.write(HttpBytes.doneStreaming)
          rawOutputStream.flush()
        }
    }

}
