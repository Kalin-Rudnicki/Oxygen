package oxygen.example.webServer.api

import oxygen.example.api.ConnectionApi
import oxygen.example.api.model.error.ApiError
import oxygen.example.api.model.user.*
import oxygen.example.api.service.TokenService
import oxygen.example.conversion.domainToApi.*
import oxygen.example.core.model.user.*
import oxygen.example.domain.model as DM
import oxygen.example.domain.service.ConnectionService
import oxygen.http.server.CurrentRequest
import zio.*

final case class ConnectionApiImpl(
    connectionService: ConnectionService,
    tokenService: TokenService,
) extends ConnectionApi {

  override def requestConnection(userId: UserId, authorization: UserToken): IO[ApiError, Unit] =
    CurrentRequest.handle[DM.error.ConnectionError, ApiError] {
      for {
        user <- tokenService.validateToken(authorization)
        _ <- connectionService.requestConnection(user, userId)
      } yield ()
    }

  override def decisionConnectionRequest(userId: UserId, accept: Boolean, authorization: UserToken): IO[ApiError, Unit] =
    CurrentRequest.handle[DM.error.ConnectionError, ApiError] {
      for {
        user <- tokenService.validateToken(authorization)
        _ <-
          if accept then connectionService.acceptConnection(user, userId)
          else connectionService.rejectConnection(user, userId)
      } yield ()
    }

  override def getConnections(userId: UserId, authorization: UserToken): IO[ApiError, Seq[User]] =
    CurrentRequest.handle[DM.error.DomainError, ApiError] {
      for {
        user <- tokenService.validateToken(authorization)
        _ <- connectionService.ensureAccess(user, userId)
        connectedUsers <- connectionService.getConnectedUsers(userId)
      } yield connectedUsers.map(_.toApi)
    }

}
object ConnectionApiImpl {

  val layer: URLayer[ConnectionService & TokenService, ConnectionApi] =
    ZLayer.fromFunction { ConnectionApiImpl.apply }

}
