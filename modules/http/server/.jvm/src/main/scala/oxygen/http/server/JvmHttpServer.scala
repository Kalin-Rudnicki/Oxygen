package oxygen.http.server

import java.io.*
import java.net.*
import java.nio.charset.{Charset, StandardCharsets}
import java.util.UUID
import oxygen.http.core.*
import oxygen.http.core.internal.*
import oxygen.http.model.*
import oxygen.predef.core.*
import oxygen.zio.metrics.*
import oxygen.zio.syntax.log.*
import zio.*

final case class JvmHttpServer(
    streamChunkSize: Int = 8192,
    requestMiddlewares: Chunk[RequestMiddleware] = Chunk(RequestMiddleware.receiveOxygenTracing),
    responseMiddlewares: Chunk[ResponseMiddleware] = Chunk(ResponseMiddleware.sendOxygenTracing),
) extends HttpServer {

  private object HttpServerBytes {

    object FullHeaders {

      val server: Array[Byte] = s"Server: OxygenHttpServer/${oxygen.core.BuildInfo.version}\r\n".getBytes

    }

    val headersStart: Array[Byte] =
      Array(
        FullHeaders.server,
        HttpBytes.FullHeaders.connectionClose,
      ).flatten

  }

  private def makeServerSocket(config: HttpServer.Config): RIO[Scope, ServerSocket] =
    ZIO.attempt { new ServerSocket(config.port) }.withFinalizerAuto

  private def acceptConnection(serverSocket: ServerSocket): RIO[Scope, Socket] =
    ZIO.asyncZIO[Scope, Throwable, Socket] { register =>
      ZIO.succeed { register { ZIO.attempt { serverSocket.accept() }.withFinalizerAuto } }
    }

  private val charset: Charset = StandardCharsets.UTF_8

  private def parseRequest(
      reader: HttpBufferedReader,
  ): HttpRequest = {
    var contentType: ContentType = null
    var contentLength: Option[Long] = null
    var charset: Charset = StandardCharsets.UTF_8
    val headerBuilder = ArraySeq.newBuilder[(String, String)]

    val method: HttpMethod = HttpMethod(reader.readUntilSpaceAndSkipSpaces())
    val url: String = reader.readUntilSpaceAndSkipSpaces()
    val httpVersion = reader.readUntilEOL()

    httpVersion match
      case "HTTP/1.1" =>
      case protocol   => throw new RuntimeException(s"Server received unsupported HTTP protocol: $protocol")

    val (paths, queryParams) =
      url.split('?').toList.filter(_.nonEmpty) match {
        case pat :: Nil =>
          (
            ArraySeq.from(pat.split('/').iterator.filter(_.nonEmpty)).map(URLDecoder.decode(_, StandardCharsets.UTF_8)),
            ArraySeq.empty[(String, String)],
          )
        case pat :: queries :: Nil =>
          (
            ArraySeq.from(pat.split('/').iterator.filter(_.nonEmpty)).map(URLDecoder.decode(_, StandardCharsets.UTF_8)),
            ArraySeq.from(queries.split('&')).map {
              _.split('=').toList match {
                case k :: v :: Nil => (URLDecoder.decode(k, StandardCharsets.UTF_8), URLDecoder.decode(v, StandardCharsets.UTF_8))
                case _             => throw new RuntimeException(s"invalid query-params format: $queries")
              }
            },
          )
        case _ =>
          throw new RuntimeException(s"invalid URL format: $url")
      }

    if (paths.contains(".."))
      throw new RuntimeException("path contains '..'")

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

    // TODO (KR) : need to add content-length protection
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

    HttpRequest(
      method,
      paths,
      QueryParams(queryParams),
      headers = Headers(headerBuilder.result()),
      body = body,
    )
  }

  private def writeResponse(
      response: HttpResponse,
      rawOutputStream: OutputStream,
      bufferedOutputStream: OutputStream,
  ): URIO[Scope, Unit] = {

    // =====| Line 1 |=====

    bufferedOutputStream.write(HttpBytes.httpVersionSpace)
    bufferedOutputStream.write(response.statusCode.code.toString.getBytes)
    bufferedOutputStream.write(HttpBytes.space)
    bufferedOutputStream.write(response.statusCode.name.getBytes)
    bufferedOutputStream.write(HttpBytes.newline)

    // =====| Headers |=====

    bufferedOutputStream.write(HttpServerBytes.headersStart)
    // TODO (KR) : date
    WriteUtils.writeHeaders(bufferedOutputStream)(response.headers)

    // =====| Body |=====

    WriteUtils.writeBodyHeadersAndBody(rawOutputStream, bufferedOutputStream)(response.body, charset, streamChunkSize)
  }

  private def handleRequest(
      config: HttpServer.Config,
      socket: Socket,
      endpoints: Endpoints.Finalized,
  ): URIO[Scope, Unit] =
    for { // TODO (KR) : reduce level of logging once things have been hardened
      _ <- ZIO.logDebugAnnotated("--- HTTP Server : Received request ---", "address" -> socket.getInetAddress.toString)

      _ <- ZIO.logTrace("--- HTTP Server : Parse Request ---")
      inputStream <- ZIO.succeed(BufferedInputStream(socket.getInputStream)).withFinalizerAuto
      rawOutputStream = socket.getOutputStream
      bufferedOutputStream <- ZIO.succeed(BufferedOutputStream(rawOutputStream)).withFinalizerAuto
      httpReader = HttpBufferedReader(inputStream)
      isEof <- ZIO.attempt { httpReader.isEOF() }.orDie // TODO (KR) :
      normalRequest = for {
        request <- ZIO.attempt { parseRequest(httpReader) }.orDie // TODO (KR) :
        request <- ZIO.foldLeft(requestMiddlewares)(request) { (req, mid) => mid.map(req) }

        _ <- ZIO.logTrace("--- HTTP Server : Evaluate Request ---")
        response <- endpoints.eval(RequestContext(request, config.exposeInternalErrors))
        response <- ZIO.foldLeft(responseMiddlewares)(response) { (res, mid) => mid.map(res) }

        _ <- ZIO.logTrace("--- HTTP Server : Send Response ---")
        _ <- ZIO.attempt { writeResponse(response, rawOutputStream, bufferedOutputStream) }.flatten.orDie // TODO (KR) :

        _ <- ZIO.logTrace("--- HTTP Server : Request Handled ---")
      } yield ()

      _ <-
        if (isEof) ZIO.logWarning("Received empty request")
        else normalRequest
    } yield ()

  private def acceptAndHandleRequest(
      config: HttpServer.Config,
      serverSocket: ServerSocket,
      endpoints: Endpoints.Finalized,
      runningFibers: Ref[Map[UUID, Fiber[Nothing, Unit]]],
  ): Task[Unit] =
    Scope.make.flatMap { scope =>
      val eff: RIO[Scope, Unit] =
        for {
          socket <- acceptConnection(serverSocket)
          requestId <- Random.nextUUID
          effect = ZIO.uninterruptible {
            ZIO
              .interruptible(handleRequest(config, socket, endpoints))
              .exit
              .flatMap { exit =>
                ZIO.foreachDiscard(exit.causeOption)(ZIO.logErrorCause("Error handling request", _)) *>
                  runningFibers.update(_.removed(requestId)) *> scope.close(exit)
              }
          } @@ ZIOAspect.annotated("request-id", requestId.toString) @@ HttpServerMetrics.requestDuration.toAspect
          effectFiber <- effect.fork
          _ <- runningFibers.update(_.updated(requestId, effectFiber))
        } yield ()

      eff.provideEnvironment(ZEnvironment(scope))
    }

  override def start(config: HttpServer.Config, endpoints: Endpoints, middlewares: Seq[EndpointMiddleware]): RIO[Scope, Unit] =
    for {
      runningFibers <- Ref.make(Map.empty[UUID, Fiber[Nothing, Unit]])
      finalizedEndpoints = endpoints.applyMiddlewares(middlewares).finish
      interruptRunningFibers = for {
        runningFibers <- runningFibers.get
        _ <- ZIO.foreachParDiscard(runningFibers.values)(_.interrupt)
      } yield ()
      _ <- ZIO.addFinalizer { interruptRunningFibers }

      serverSocket <- makeServerSocket(config)
      _ <- acceptAndHandleRequest(config, serverSocket, finalizedEndpoints, runningFibers).forever.forkScoped
      _ <- ZIO.logInfo(s"Oxygen HTTP server started on port ${config.port}")
    } yield ()

}
