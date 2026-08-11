package oxygen.http.api

import oxygen.http.core.ZioHttpCompat.*
import oxygen.http.model.*
import oxygen.predef.core.*
import oxygen.predef.zio.*
import oxygen.schema.JsonSchema
import scala.annotation.experimental
import zio.*
import zio.http.{Body, Headers, MediaType, Status}

@experimental
sealed trait FileSystemResourceApi extends ResourceApi {
  protected val basePath: Path
}
object FileSystemResourceApi {

  enum ResponseMode derives StrictEnum {
    case Cache
    case Read
    case Stream
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Cache-Control
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Directives for a served resource's `Cache-Control` header. All fields optional/off by default,
    * so an unconfigured resource emits no header (unchanged behavior). Render is best-effort: only the
    * set directives are emitted, in a conventional order.
    */
  final case class CacheControl(
      maxAgeSeconds: Option[Long] = None,
      sMaxAgeSeconds: Option[Long] = None,
      visibility: Option[CacheControl.Visibility] = None,
      noCache: Boolean = false,
      noStore: Boolean = false,
      mustRevalidate: Boolean = false,
      immutable: Boolean = false,
  ) derives JsonSchema {

    /** `Cache-Control` header value; empty when no directives are set. */
    def render: String = {
      val parts = List.newBuilder[String]
      visibility.foreach {
        case CacheControl.Visibility.Public  => parts += "public"
        case CacheControl.Visibility.Private => parts += "private"
      }
      if noStore then parts += "no-store"
      if noCache then parts += "no-cache"
      maxAgeSeconds.foreach(s => parts += s"max-age=$s")
      sMaxAgeSeconds.foreach(s => parts += s"s-maxage=$s")
      if mustRevalidate then parts += "must-revalidate"
      if immutable then parts += "immutable"
      parts.result().mkString(", ")
    }

  }
  object CacheControl {

    enum Visibility derives StrictEnum {
      case Public, Private
    }

    /** `no-store` — never cache (sensitive / always-fresh). */
    val noStore: CacheControl = CacheControl(noStore = true)

    /** `no-cache, must-revalidate` — cache but revalidate every use (good for stable-named bundles like `main.js`). */
    val revalidate: CacheControl = CacheControl(noCache = true, mustRevalidate = true)

    /** `public, max-age=…[, immutable]` — long-lived caching (good for content-hashed assets). */
    def maxAge(seconds: Long, immutable: Boolean = false): CacheControl =
      CacheControl(maxAgeSeconds = Some(seconds), visibility = Some(Visibility.Public), immutable = immutable)

  }

  /**
    * A single cache rule: apply [[cacheControl]] to resources whose full path (segments joined by `/`)
    * is in [[paths]], or whose file extension (no dot, case-sensitive) is in [[extensions]].
    */
  final case class ResourceCacheRule(
      cacheControl: CacheControl,
      paths: List[String] = Nil,
      extensions: List[String] = Nil,
  ) derives JsonSchema {
    def matches(path: String, ext: Option[String]): Boolean =
      paths.contains(path) || ext.exists(extensions.contains)
  }

  /**
    * Cache-Control policy for served resources. First matching [[rules]] wins, else [[default]]
    * (if any). Empty policy ⇒ no `Cache-Control` header (default, unchanged behavior).
    */
  final case class ResourceCacheConfig(
      default: Option[CacheControl] = None,
      rules: List[ResourceCacheRule] = Nil,
  ) derives JsonSchema {

    def cacheControlFor(rest: List[String]): Option[CacheControl] = {
      val path = rest.mkString("/")
      val ext = rest.lastOption.flatMap { f =>
        val i = f.lastIndexOf('.')
        if i > 0 && i < f.length - 1 then Some(f.substring(i + 1)) else None
      }
      rules.find(_.matches(path, ext)).map(_.cacheControl).orElse(default)
    }

    def headerValueFor(rest: List[String]): Option[String] =
      cacheControlFor(rest).map(_.render).filter(_.nonEmpty)

  }
  object ResourceCacheConfig {
    val empty: ResourceCacheConfig = ResourceCacheConfig()
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Config / layer
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Config(
      basePath: String,
      responseMode: ResponseMode = ResponseMode.Stream,
      cacheControl: ResourceCacheConfig = ResourceCacheConfig.empty,
  ) derives JsonSchema

  val layer: RLayer[FileSystemResourceApi.Config, ResourceApi] =
    ZLayer {
      for {
        config <- ZIO.service[FileSystemResourceApi.Config]
        rawBasePath <- Path.of(config.basePath)
        basePath = rawBasePath.absolute.normalized
        status <- basePath.status
        _ <- status match
          case Path.Type.Directory      => ZIO.unit
          case Path.Status.DoesNotExist => ZIO.fail(Error(s"FileSystemResourceApi.basePath does not exist [basePath: ${rawBasePath.pathName}] [absBasePath: ${basePath.pathName}]"))
          case _                        => ZIO.fail(Error(s"FileSystemResourceApi.basePath is not a directory [basePath: ${rawBasePath.pathName}] [absBasePath: ${basePath.pathName}]"))
        api <- config.responseMode match
          case ResponseMode.Cache  => Ref.make(Map.empty[List[String], ByteContentWithType]).map(LiveCacheResourceApi(basePath, _, config.cacheControl))
          case ResponseMode.Read   => ZIO.succeed(LiveReadResourceApi(basePath, config.cacheControl))
          case ResponseMode.Stream => ZIO.succeed(LiveStreamResourceApi(basePath, config.cacheControl))
      } yield api
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Impls
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /////// Cache ///////////////////////////////////////////////////////////////

  final case class LiveCacheResourceApi(
      basePath: Path,
      cacheRef: Ref[Map[List[String], ByteContentWithType]],
      cacheControl: ResourceCacheConfig,
  ) extends ResourceApi {

    private def resolveAndCache(rest: List[String]): IO[ResourceApi.ApiError, ByteContentWithType] =
      cacheRef.get.flatMap { cache =>
        cache.get(rest) match {
          case Some(cached) => ZIO.succeed(cached)
          case None         => readContentWithType(basePath, rest).tap { c => cacheRef.update(_.updated(rest, c)) }
        }
      }

    override def resource(rest: List[String]): IO[ResourceApi.ApiError, RawSuccessResponse] =
      resolveAndCache(rest).map(contentWithTypeToSuccessResponse(_, cacheControlHeaders(cacheControl, rest)))

  }

  /////// Read ///////////////////////////////////////////////////////////////

  final case class LiveReadResourceApi(
      basePath: Path,
      cacheControl: ResourceCacheConfig,
  ) extends ResourceApi {

    override def resource(rest: List[String]): IO[ResourceApi.ApiError, RawSuccessResponse] =
      readContentWithType(basePath, rest).map(contentWithTypeToSuccessResponse(_, cacheControlHeaders(cacheControl, rest)))

  }

  /////// Stream ///////////////////////////////////////////////////////////////

  final case class LiveStreamResourceApi(
      basePath: Path,
      cacheControl: ResourceCacheConfig,
  ) extends ResourceApi {

    override def resource(rest: List[String]): IO[ResourceApi.ApiError, RawSuccessResponse] =
      for {
        (validResolvedPath, mediaType) <- resolvePathAndMediaType(basePath, rest)
        size <- validResolvedPath.size.orDie
        body = Body.fromStream(validResolvedPath.readByteStream, size).optMediaType(mediaType)
      } yield RawSuccessResponse(Status.Ok, cacheControlHeaders(cacheControl, rest), body)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def showPath(path: List[String]): String = path.mkString("/", "/", "")

  /** `Cache-Control` headers for this resource per the policy, or empty when none applies. */
  def cacheControlHeaders(cacheControl: ResourceCacheConfig, rest: List[String]): Headers =
    cacheControl.headerValueFor(rest).fold(Headers.empty)(v => Headers("cache-control", v))

  def resolvePath(basePath: Path, rest: List[String]): IO[ResourceApi.ApiError, Path] =
    for {
      _ <- ZIO.fail(ResourceApi.ApiError.MalformedPath(showPath(rest), "Resource path can not be empty")).whenDiscard { rest.isEmpty }
      _ <- ZIO.fail(ResourceApi.ApiError.MalformedPath(showPath(rest), "Resource path can not contain \"/\"")).whenDiscard { rest.exists(_.contains("/")) }
      _ <- ZIO.fail(ResourceApi.ApiError.MalformedPath(showPath(rest), "Resource path can not contain \"..\"")).whenDiscard { rest.contains("..") }
      resolvedPath <- ZIO.attempt { basePath.resolve(rest*) }.catchAllCause { c =>
        ZIO.logWarningCause(s"Defect resolving path [${rest.map(_.unesc).mkString(", ")}]", c) *>
          ZIO.fail(ResourceApi.ApiError.MalformedPath(showPath(rest), "The path you provided has very bad vibes..."))
      }
      status <- resolvedPath.status.orDie
      _ <- status match
        case Path.Type.File           => ZIO.unit
        case Path.Status.DoesNotExist => ZIO.fail(ResourceApi.ApiError.NoSuchPath(showPath(rest), None))
        case Path.Type.Directory      => ZIO.fail(ResourceApi.ApiError.NoSuchPath(showPath(rest), None)) // Do something different?
        case _                        => ZIO.fail(ResourceApi.ApiError.NoSuchPath(showPath(rest), None)) // Do something different?
    } yield resolvedPath

  def resolvePathAndMediaType(basePath: Path, rest: List[String]): IO[ResourceApi.ApiError, (Path, Option[MediaType])] =
    resolvePath(basePath, rest).map { validResolvedPath => (validResolvedPath, validResolvedPath.fileName.extension.flatMap(MediaType.forFileExtension)) }

  def readContentWithType(basePath: Path, rest: List[String]): IO[ResourceApi.ApiError, ByteContentWithType] =
    for {
      (validResolvedPath, mediaType) <- resolvePathAndMediaType(basePath, rest)
      content <- validResolvedPath.readBytes.orDie
    } yield ByteContentWithType(content, mediaType)

  def contentWithTypeToSuccessResponse(contentWithType: ByteContentWithType, headers: Headers = Headers.empty): RawSuccessResponse =
    RawSuccessResponse(
      status = Status.Ok,
      headers = headers,
      body = Body.fromArray(contentWithType.body).optMediaType(contentWithType.contentType),
    )

}
