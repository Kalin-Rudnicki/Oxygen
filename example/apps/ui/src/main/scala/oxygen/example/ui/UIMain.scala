package oxygen.example.ui

import oxygen.example.api.*
import oxygen.example.ui.page as P
import oxygen.example.ui.service.LocalService
import oxygen.http.client.*
import oxygen.ui.web.*
import oxygen.ui.web.apispec.ApiSpecPage
import oxygen.ui.web.create.*
import oxygen.ui.web.defaults.*
import oxygen.ui.web.service.LocalStorage
import scala.collection.immutable.ArraySeq
import zio.*

object UIMain extends PageApp[UIMain.Env] {

  type Env =
    UserApi & ConnectionApi & StreamApi & LocalService & RawClient

  // override val logLevel: LogLevel = LogLevel.Debug

  /*
  override val jobs: Seq[GlobalJob[Env]] =
    Seq(
      GlobalJob.simplePoll("say-hi", 15.seconds)(5.seconds) {
        PageMessages.schedule(PageMessage.info("Hi there!"), 2.seconds)
      },
    )
   */

  override val styleSheets: ArraySeq[StyleSheet] = ArraySeq(
    normalizeCssReset,
    OxygenStyleVars.toCSS(OxygenStyleVarDefaults.CZR),
    InlinePseudoClassStyles.compiled,
    OxygenStyleSheet.compiled,
  )

  override val pages: ArraySeq[RoutablePage[Env]] = ArraySeq(
    P.index.IndexPage,
    P.login.LoginPage,
    P.register.RegisterPage,
    P.home.HomePage,
    P.profile.ProfilePage,
    ApiSpecPage,
    StylesPage,
    ComponentsPage,
  )

  override def layer: TaskLayer[Env] =
    ZLayer.make[Env](
      // clients
      ZLayer.succeed { Client.Config.relativeUrl },
      Client.layer.default,
      RawClient.default, // used directly by the ApiSpecPage to GET /oxygen/api-spec
      DeriveClient.clientLayer[UserApi],
      DeriveClient.clientLayer[ConnectionApi],
      DeriveClient.clientLayer[StreamApi],
      // other
      LocalStorage.live,
      LocalService.layer,
    )

}
