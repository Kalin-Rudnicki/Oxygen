package oxygen.example.webServer.api

import java.nio.file.{Files, Path}
import oxygen.example.api.UIApi
import oxygen.example.api.model.error.*
import oxygen.example.api.model.ui.HtmlResponse
import oxygen.http.model.Redirect
import oxygen.json.JsonCodec
import zio.*

final case class UIApiImpl(
    pageHtml: String,
    resourceDirPath: Path,
) extends UIApi {

  override def index(): UIO[Redirect] = ZIO.succeed(Redirect.path("/page"))

  override def getResource(rest: List[String]): IO[UIApiError, String] =
    for {
      _ <- ZIO.fail(UIApiError.BadRequest("Resource path can not contain '..'")).when(rest.contains(".."))
      resolved <- ZIO.attempt { resourceDirPath.resolve(rest.mkString("/")) }.orDie
      goodPath <- ZIO.attempt { Files.exists(resolved) && Files.isRegularFile(resolved) }.orDie
      _ <- ZIO.fail(UIApiError.ResourceNotFound(rest.mkString("/", "/", ""))).unlessDiscard(goodPath)
      contents <- ZIO.attempt { Files.readString(resolved) }.orDie
    } yield contents

  override def getPage(rest: List[String]): UIO[HtmlResponse] =
    ZIO.succeed(HtmlResponse(pageHtml))

}
object UIApiImpl {

  final case class Config(
      htmlPath: String,
      resourcePath: String,
  ) derives JsonCodec

  val layer: RLayer[Config, UIApi] =
    ZLayer {
      for {
        config <- ZIO.service[Config]

        resourceDirPath <- ZIO.attempt { Path.of(config.resourcePath) }
        _ <- ZIO.fail(new RuntimeException(s"Resource path '$resourceDirPath' does not exist")).unlessZIODiscard { ZIO.attempt { Files.exists(resourceDirPath) } }
        _ <- ZIO.fail(new RuntimeException(s"Resource path '$resourceDirPath' is not a directory")).unlessZIODiscard { ZIO.attempt { Files.isDirectory(resourceDirPath) } }

        htmlPath <- ZIO.attempt { Path.of(config.htmlPath) }
        _ <- ZIO.fail(new RuntimeException(s"Html path '$htmlPath' does not exist")).unlessZIODiscard { ZIO.attempt { Files.exists(htmlPath) } }
        _ <- ZIO.fail(new RuntimeException(s"Html path '$htmlPath' is not a directory")).unlessZIODiscard { ZIO.attempt { Files.isRegularFile(htmlPath) } }
        pageHtml <- ZIO.attempt { Files.readString(htmlPath) }

      } yield UIApiImpl(pageHtml, resourceDirPath)
    }

}
