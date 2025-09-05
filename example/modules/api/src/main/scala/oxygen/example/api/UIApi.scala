package oxygen.example.api

import oxygen.example.api.model.error.*
import oxygen.example.api.model.ui.HtmlResponse
import oxygen.http.client.DeriveClient
import oxygen.http.core.*
import oxygen.http.model.*
import zio.*

trait UIApi derives DeriveClient {

  @route.get("/")
  def index(): UIO[Redirect]

  @route.get("/res/%")
  def getResource(
      @param.path rest: List[String],
  ): IO[UIApiError, String]

  @route.get("/page/%")
  def getPage(
      @param.path rest: List[String],
  ): UIO[HtmlResponse]

}
