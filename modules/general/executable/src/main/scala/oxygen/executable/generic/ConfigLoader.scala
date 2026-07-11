package oxygen.executable.generic

import oxygen.json.{Json, JsonDecoder}
import oxygen.predef.core.*
import oxygen.predef.zio.*
import oxygen.yaml.YamlParser
import zio.*

private[executable] object ConfigLoader {

  def loadDecoded[T: JsonDecoder as decoder](varName: String, raw: String): IO[String, T] =
    resolveJson(raw)
      .flatMap { json => ZIO.fromEither { decoder.decodeJsonAST(json).leftMap(_.getMessage) } }
      .mapError { error => s"Error extracting environment variable config [$varName]: $error" }

  private def resolveJson(raw: String): IO[String, Json] =
    Path.normalizedAbsolute(raw).mapError(_.safeGetMessage).flatMap { path =>
      path.status.mapError(_.safeGetMessage).flatMap {
        case Path.Type.File           => readFileJson(path)
        case Path.Type.Directory      => mergeDirectoryJson(path)
        case Path.Status.DoesNotExist => ZIO.fail(s"Path does not exist (${path.pathName})")
        case _                        => ZIO.fail(s"Path is not a file or directory (${path.pathName})")
      }
    }

  // TODO (KR) : add support for yaml?
  private def readOptFileJson(file: Path): IO[String, Option[Json]] =
    file.fileName.extension match {
      case Some("json")         => file.readDecodeJson[Json].mapError(_.safeGetMessage).asSome
      case Some("yaml" | "yml") =>
        file.read.mapError(_.safeGetMessage)
          .flatMap { str => ZIO.fromEither { YamlParser.parseJson(str) }.mapError { e => s"Invalid yaml at (${file.pathName}): $e" } }.asSome
      case _ => ZIO.none
    }

  private def readFileJson(file: Path): IO[String, Json] =
    readOptFileJson(file).someOrFail(s"Invalid file extension (${file.pathName})")

  private def mergeDirectoryJson(dir: Path): IO[String, Json] =
    dir.childStream.mapError(_.safeGetMessage).mapZIO(readOptFileJson).collectSome.runCollect.flatMap {
      case children if children.isEmpty => ZIO.fail(s"Directory contains no valid config files (${dir.pathName})")
      case children                     => ZIO.succeed { children.reduceLeft(_ ++ _) }
    }

}
