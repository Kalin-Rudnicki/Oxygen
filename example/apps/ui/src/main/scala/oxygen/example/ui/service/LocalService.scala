package oxygen.example.ui.service

import oxygen.example.api.model.user.{User, UserToken}
import oxygen.example.ui.page as P
import oxygen.schema.*
import oxygen.schema.instances.given
import oxygen.ui.web.UIError
import oxygen.ui.web.service.LocalStorage
import zio.*

final class LocalService(
    userTokenCache: LocalStorage.StorageCache[UserToken],
) {

  object userToken {

    def set(token: UserToken): UIO[Unit] =
      userTokenCache.set(token)

    def clear: UIO[Unit] =
      userTokenCache.remove

    def getOption: IO[UIError.ClientSide.InternalDefect, Option[UserToken]] =
      userTokenCache.find

    def get: IO[UIError.ClientSide.InternalDefect | UIError.Redirect, UserToken] =
      getOption.someOrElseZIO { P.login.LoginPage.navigate.replace(P.login.LoginPage.PageParams(None)) }

  }

  object user {

    def getOption: IO[UIError.ClientSide.InternalDefect, Option[User]] =
      userToken.getOption.map(_.map(_.user))

    def get: IO[UIError.ClientSide.InternalDefect | UIError.Redirect, User] =
      userToken.get.map(_.user)

  }

}
object LocalService {

  val layer: ZLayer[LocalStorage, UIError.ClientSide.InternalDefect, LocalService] =
    ZLayer {
      for {
        localStorage <- ZIO.service[LocalStorage]
        userTokenCache <- localStorage.storageKey.string[UserToken]("user-token").cached
      } yield LocalService(userTokenCache)
    }

}
