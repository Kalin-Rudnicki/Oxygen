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

  final case class Config(
      basePath: String,
      responseMode: ResponseMode = ResponseMode.Stream,
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
          case ResponseMode.Cache  => Ref.make(Map.empty[List[String], ByteContentWithType]).map(LiveCacheResourceApi(basePath, _))
          case ResponseMode.Read   => ZIO.succeed(LiveReadResourceApi(basePath))
          case ResponseMode.Stream => ZIO.succeed(LiveStreamResourceApi(basePath))
      } yield api
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Impls
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /////// Cache ///////////////////////////////////////////////////////////////

  final case class LiveCacheResourceApi(
      basePath: Path,
      cacheRef: Ref[Map[List[String], ByteContentWithType]],
  ) extends ResourceApi {

    private def resolveAndCache(rest: List[String]): IO[ResourceApi.ApiError, ByteContentWithType] =
      cacheRef.get.flatMap { cache =>
        cache.get(rest) match {
          case Some(cached) => ZIO.succeed(cached)
          case None         => readContentWithType(basePath, rest).tap { c => cacheRef.update(_.updated(rest, c)) }
        }
      }

    override def resource(rest: List[String]): IO[ResourceApi.ApiError, RawSuccessResponse] =
      resolveAndCache(rest).map(contentWithTypeToSuccessResponse)

  }

  /////// Read ///////////////////////////////////////////////////////////////

  final case class LiveReadResourceApi(
      basePath: Path,
  ) extends ResourceApi {

    override def resource(rest: List[String]): IO[ResourceApi.ApiError, RawSuccessResponse] =
      readContentWithType(basePath, rest).map(contentWithTypeToSuccessResponse)

  }

  /////// Stream ///////////////////////////////////////////////////////////////

  final case class LiveStreamResourceApi(
      basePath: Path,
  ) extends ResourceApi {

    override def resource(rest: List[String]): IO[ResourceApi.ApiError, RawSuccessResponse] =
      for {
        (validResolvedPath, mediaType) <- resolvePathAndMediaType(basePath, rest)
        size <- validResolvedPath.size.orDie
        body = Body.fromStream(validResolvedPath.readByteStream, size).optMediaType(mediaType)
      } yield RawSuccessResponse(Status.Ok, Headers.empty, body)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def showPath(path: List[String]): String = path.mkString("/", "/", "")

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

  def contentWithTypeToSuccessResponse(contentWithType: ByteContentWithType): RawSuccessResponse =
    RawSuccessResponse(
      status = Status.Ok,
      headers = Headers.empty,
      body = Body.fromArray(contentWithType.body).optMediaType(contentWithType.contentType),
    )

}
