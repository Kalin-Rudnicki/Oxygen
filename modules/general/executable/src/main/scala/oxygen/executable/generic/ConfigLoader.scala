package oxygen.executable.generic

import java.io.File
import oxygen.json.{Json, JsonDecoder, JsonError}
import oxygen.predef.core.*
import scala.io.Source
import scala.util.Using

private[executable] object ConfigLoader {

  def loadDecoded[T](raw: String, decoder: JsonDecoder[T]): Either[String, T] =
    resolveJson(raw).flatMap(json => decoder.decodeJsonAST(json).left.map(_.show))

  def loadSync[T](envVar: String, decoder: JsonDecoder[T]): Either[String, T] =
    Option(System.getenv(envVar)) match
      case None      => s"Environment variable $envVar is not set".asLeft
      case Some(raw) => loadDecoded(raw, decoder)

  private def resolveJson(raw: String): Either[String, Json] =
    val file = File(raw)
    if file.isFile then readFileJson(file)
    else if file.isDirectory then mergeDirectoryJson(file)
    else Json.parse(raw).left.map(_.show)

  private def readFileJson(file: File): Either[String, Json] =
    Using(Source.fromFile(file))(_.mkString).toEither.left.map(_.getMessage).flatMap { content =>
      Json.parse(content).left.map(_.show)
    }

  private def mergeDirectoryJson(dir: File): Either[String, Json] =
    val jsonFiles = Option(dir.listFiles()).toList.flatten.filter(f => f.isFile && f.getName.endsWith(".json"))
    if jsonFiles.isEmpty then s"Directory '${dir.getPath}' has no .json files".asLeft
    else
      jsonFiles.foldLeft[Either[String, Json]](Json.obj().asRight) { (acc, file) =>
        acc.flatMap { merged =>
          readFileJson(file).map(merged.merge)
        }
      }

  extension (error: JsonError) private def show: String = error.toString

}
