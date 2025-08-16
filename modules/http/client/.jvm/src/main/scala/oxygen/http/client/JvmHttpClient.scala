package oxygen.http.client

import java.io.*
import java.net.*
import java.nio.charset.{Charset, StandardCharsets}
import javax.net.SocketFactory
import javax.net.ssl.*
import oxygen.http.core.*
import oxygen.http.core.internal.*
import oxygen.http.model.*
import oxygen.predef.core.*
import oxygen.zio.metrics.*
import zio.*

final case class JvmHttpClient(
    target: ConnectionTarget,
    streamChunkSize: Int = 8192,
    requestMiddlewares: Chunk[RequestMiddleware] = Chunk(RequestMiddleware.sendOxygenTracing),
    responseMiddlewares: Chunk[ResponseMiddleware] = Chunk(ResponseMiddleware.receiveOxygenTracing),
) extends HttpClient {

  private object HttpClientBytes {

    object FullHeaders {
      val host: Array[Byte] = s"Host: ${target.host}\r\n".getBytes
      val userAgent: Array[Byte] = s"User-Agent: OxygenHttpClient/${oxygen.core.BuildInfo.version}\r\n".getBytes
    }

    val headersStart: Array[Byte] =
      Array(
        FullHeaders.host,
        FullHeaders.userAgent,
        HttpBytes.FullHeaders.connectionClose,
        HttpBytes.FullHeaders.acceptAny,
      ).flatten

  }

  private val charset: Charset = StandardCharsets.UTF_8

  private def writeRequest(
      request: HttpRequest,
      rawOutputStream: OutputStream,
      bufferedOutputStream: OutputStream,
  ): URIO[Scope, Unit] = {

    // =====| Line 1 |=====

    bufferedOutputStream.write(request.method.method.getBytes)

    bufferedOutputStream.write(HttpBytes.space)

    if (request.paths.nonEmpty) {
      val pathsIter: Iterator[String] = request.paths.iterator
      while (pathsIter.hasNext) {
        bufferedOutputStream.write(HttpBytes.slash)
        bufferedOutputStream.write(URLEncoder.encode(pathsIter.next(), charset).getBytes)
      }
    } else
      bufferedOutputStream.write(HttpBytes.slash)

    if (request.queryParams.rawQueryParams.nonEmpty) {
      val queryParamsIter: Iterator[(String, String)] = request.queryParams.rawQueryParams.iterator
      var tup: (String, String) = queryParamsIter.next()

      bufferedOutputStream.write(HttpBytes.question)
      bufferedOutputStream.write(URLEncoder.encode(tup._1, charset).getBytes)
      bufferedOutputStream.write(HttpBytes.equal)
      bufferedOutputStream.write(URLEncoder.encode(tup._2, charset).getBytes)

      while (queryParamsIter.hasNext) {
        tup = queryParamsIter.next()

        bufferedOutputStream.write(HttpBytes.and)
        bufferedOutputStream.write(URLEncoder.encode(tup._1, charset).getBytes)
        bufferedOutputStream.write(HttpBytes.equal)
        bufferedOutputStream.write(URLEncoder.encode(tup._2, charset).getBytes)
      }
    }

    bufferedOutputStream.write(HttpBytes.httpVersionEOL)

    // =====| Headers |=====

    bufferedOutputStream.write(HttpClientBytes.headersStart)
    WriteUtils.writeHeaders(bufferedOutputStream)(request.headers)

    // =====| Body |=====

    WriteUtils.writeBodyHeadersAndBody(rawOutputStream, bufferedOutputStream)(request.body, charset, streamChunkSize)
  }

  private def parseResponse(reader: HttpBufferedReader): HttpResponse = {
    var contentType: ContentType = null
    var contentLength: Option[Long] = null
    var charset: Charset = StandardCharsets.UTF_8
    val headerBuilder = ArraySeq.newBuilder[(String, String)]

    reader.readUntilSpaceAndSkipSpaces() match
      case "HTTP/1.1" =>
      case protocol   => throw new RuntimeException(s"Client received unsupported HTTP protocol: $protocol")

    val code: HttpCode =
      reader.readUntilSpaceAndSkipSpaces().toIntOption match {
        case Some(code) => HttpCode(code)
        case None       => throw new RuntimeException("received non-int http code")
      }

    val statusText: String = reader.readUntilEOL()

    while (!reader.isEOL()) {
      val headerKey: String = reader.readUntilColonAndSkipSpaces()
      val headerValue: String = reader.readUntilEOL()

      headerKey.toLowerCase match {
        case "content-type" =>
          if (contentType != null)
            throw new RuntimeException("Content-Type specified multiple times")

          val colonIdx: Int = headerValue.indexOf(';')

          if (colonIdx != -1)
            charset = Charset.forName(headerValue.substring(colonIdx + 1).trim.stripPrefix("charset="))

          val contentTypeBase: String =
            if (colonIdx == -1) headerValue
            else headerValue.substring(0, colonIdx)

          val split2: Array[String] = contentTypeBase.split('/')

          if (split2.length != 2)
            throw new RuntimeException(s"Not a Content-Type: $contentTypeBase")

          contentType = ContentType(split2(0), split2(1))
        case "content-length" =>
          if (contentLength != null)
            throw new RuntimeException("Content-Length/Transfer-Encoding specified multiple times")

          val len: Long = headerValue.toLongOption match
            case Some(len) => len
            case None      => throw new RuntimeException("Content-Length is not a Long")

          if (len < 0)
            throw new RuntimeException("Content-Length < 0")

          contentLength = len.some
        case "transfer-encoding" =>
          if (contentLength != null)
            throw new RuntimeException("Content-Length/Transfer-Encoding specified multiple times")

          if (headerValue != "chunked")
            throw new RuntimeException(s"only 'chunked' Transfer-Encoding is supported, got: $headerValue")

          contentLength = None
        case _ =>
      }

      headerBuilder.addOne((headerKey, headerValue))
    }

    reader.consumeEOL(readNextChar = false)

    val body: HttpBody =
      (contentType == null, contentLength == null) match {
        case (false, false) =>
          contentLength match {
            case Some(contentLength) if contentLength <= Int.MaxValue =>
              val bytes: Array[Byte] = reader.readNBytes(contentLength.toInt)

              HttpBody.Bytes(bytes, contentType, charset)
            case Some(contentLength) =>
              HttpBody.Stream(ZIO.succeed(reader.rawStream()), contentType, charset, contentLength.some)
            case None =>
              HttpBody.Stream(reader.chunkedStream(), contentType, charset, None)
          }
        case (true, true) =>
          HttpBody.Empty
        case _ if contentLength == 0.some && contentType == null =>
          HttpBody.Bytes(new Array[Byte](0), ContentType.PlainText, charset)
        case _ =>
          throw new RuntimeException(s"Received bad combination of Content-Type/Content-Length, Content-Type=$contentType,Content-Length=$contentLength")
      }

    HttpResponse(
      statusCode = code,
      statusText = statusText,
      headers = Headers(headerBuilder.result()),
      body = body,
    )
  }

  private val sslFactory: Option[SocketFactory] =
    target.ssl match {
      case ConnectionTarget.SslConfig.NoSsl      => None
      case ConnectionTarget.SslConfig.DefaultSsl => SSLSocketFactory.getDefault.some
    }

  override def send(request: HttpRequest): ZIO[Scope, HttpClientError, HttpResponse] =
    (for { // TODO (KR) : reduce level of logging once things have been hardened
      request <- ZIO.foldLeft(requestMiddlewares)(request) { (req, mid) => mid.map(req) }

      _ <- ZIO.logDebug("--- HTTP Client : Preparing Request ---")
      socket <- sslFactory match
        case Some(sslFactory) => ZIO.attempt { sslFactory.createSocket(target.host, target.port) }.withFinalizerAuto
        case None             => ZIO.attempt { new Socket(target.host, target.port) }.withFinalizerAuto

      _ <- ZIO.logTrace("--- HTTP Client : Send Request ---")
      rawOutputStream = socket.getOutputStream
      bufferedOutputStream <- ZIO.attempt { BufferedOutputStream(rawOutputStream) }.withFinalizerAuto
      _ <- ZIO.attempt { writeRequest(request, rawOutputStream, bufferedOutputStream) }.flatten

      _ <- ZIO.logTrace("--- HTTP Client : Parse Response ---")
      inputStream <- ZIO.attempt { BufferedInputStream(socket.getInputStream) }.withFinalizerAuto
      httpReader = HttpBufferedReader(inputStream)
      response <- ZIO.attempt { parseResponse(httpReader) }
      response <- ZIO.foldLeft(responseMiddlewares)(response) { (res, mid) => mid.map(res) }

      _ <- ZIO.logTrace("--- HTTP Client : Request Handled ---")
    } yield response).mapError(HttpClientError(_)) @@ HttpClientMetrics.requestDuration.toAspect

}
