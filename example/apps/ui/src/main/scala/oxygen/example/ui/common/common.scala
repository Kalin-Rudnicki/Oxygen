package oxygen.example.ui.common

import oxygen.example.api.model.user.User
import oxygen.example.ui.page as P
import oxygen.ui.web.component.*

val signedOutNavBar: NavBar.Const =
  signedOutNavBar(None)
def signedOutNavBar(loginRegisterEmail: Option[String]): NavBar.Const =
  NavBar.make()(
    _.simplePush("Oxygen Example")(P.index.IndexPage)(()),
  )(
    _.simplePush("Login")(P.login.LoginPage)(P.login.LoginPage.PageParams(loginRegisterEmail)),
    _.simplePush("Sign Up")(P.register.RegisterPage)(P.register.RegisterPage.PageParams(loginRegisterEmail)),
  )

def signedInNavBar(user: User): NavBar.Const =
  NavBar.make()(
    _.simplePush("Oxygen Example")(P.index.IndexPage)(()),
    _.simplePush("Home")(P.home.HomePage)(()),
  )(
    _.simplePush(user.firstName)(P.profile.ProfilePage)(()),
  )

def optionalSignedInNavBar(user: Option[User]): NavBar.Const =
  user.fold(signedOutNavBar)(signedInNavBar)
