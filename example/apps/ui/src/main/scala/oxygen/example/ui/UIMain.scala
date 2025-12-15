package oxygen.example.ui

import oxygen.example.api.*
import oxygen.example.ui.page as P
import oxygen.example.ui.service.LocalService
import oxygen.http.client.*
import oxygen.ui.web.*
import oxygen.ui.web.create.*
import oxygen.ui.web.defaults.*
import oxygen.ui.web.service.LocalStorage
import scala.collection.immutable.ArraySeq
import zio.*

object UIMain extends PageApp[UIMain.Env] {

  type Env =
    UserApi & ConnectionApi & StreamApi & LocalService

  override val logLevel: LogLevel = LogLevel.Debug

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
    StylesPage,
    ComponentsPage,
  )

  override def layer: TaskLayer[Env] =
    ZLayer.make[Env](
      // clients
      Client.layer.localPort(3010),
      DeriveClient.clientLayer[UserApi],
      DeriveClient.clientLayer[ConnectionApi],
      DeriveClient.clientLayer[StreamApi],
      // other
      LocalStorage.live,
      LocalService.layer,
    )

}
