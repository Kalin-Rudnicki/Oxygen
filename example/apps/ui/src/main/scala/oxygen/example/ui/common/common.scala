package oxygen.example.ui.common

import oxygen.example.api.model.user.User
import oxygen.example.ui.page as P
import oxygen.ui.web.apispec.ApiSpecPage
import oxygen.ui.web.component.*
import oxygen.ui.web.create.given

val signedOutNavBar: TopBar.Const =
  signedOutNavBar(None)
def signedOutNavBar(loginRegisterEmail: Option[String]): TopBar.Const =
  TopBar.empty
    .leftItems(
      _.index("Oxygen Example").onClickPush(P.index.IndexPage.nav()),
      _("Showcase").onClickPush(P.showcase.pages.ShowcaseHubPage.nav()),
      _("Oxygen API Spec").onClickPush(ApiSpecPage.nav()),
    )
    .rightItems(
      _("Login").onClickPush(P.login.LoginPage.nav(loginRegisterEmail)),
      _("Sign Up").onClickPush(P.register.RegisterPage.nav(loginRegisterEmail)),
    )

def signedInNavBar(user: User): TopBar.Const =
  TopBar.empty
    .leftItems(
      _("Oxygen Example").index.onClickPush(P.index.IndexPage.nav()),
      _("Home").onClickPush(P.home.HomePage.nav()),
      _("Showcase").onClickPush(P.showcase.pages.ShowcaseHubPage.nav()),
      _("Oxygen API Spec").onClickPush(ApiSpecPage.nav()),
    )
    .rightItems(
      _("Add Payment Method").onClickPush(P.payment.AddPaymentMethodPage.nav()),
      _(user.firstName).onClickPush(P.profile.ProfilePage.nav()),
    )

def optionalSignedInNavBar(user: Option[User]): TopBar.Const =
  user.fold(signedOutNavBar)(signedInNavBar)
