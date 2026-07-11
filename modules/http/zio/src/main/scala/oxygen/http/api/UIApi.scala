package oxygen.http.api

import oxygen.http.client.DeriveClient
import oxygen.http.core.*
import oxygen.http.model.{PageHtmlResponse, Redirect}
import scala.annotation.experimental
import zio.*

@experimental
trait UIApi derives DeriveClient {

  @route.get("/")
  def index(): UIO[Redirect]

  @route.get("/page/%")
  def getPage(
      @param.path rest: List[String],
  ): UIO[PageHtmlResponse]

}
