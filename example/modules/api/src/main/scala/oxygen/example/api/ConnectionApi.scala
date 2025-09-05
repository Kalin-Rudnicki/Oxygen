package oxygen.example.api

import oxygen.example.api.model.error.*
import oxygen.example.api.model.user.*
import oxygen.example.core.model.user.{*, given}
import oxygen.http.client.DeriveClient
import oxygen.http.core.*
import zio.*

trait ConnectionApi derives DeriveClient {

  @route.post("/api/connection/%/request")
  def requestConnection(
      @param.path userId: UserId,
      @param.header authorization: UserToken,
  ): IO[ApiError, Unit]

  @route.post("/api/connection/%/request/decision")
  def decisionConnectionRequest(
      @param.path userId: UserId,
      @param.query accept: Boolean,
      @param.header authorization: UserToken,
  ): IO[ApiError, Unit]

  @route.get("/api/user/%/connection")
  def getConnections(
      @param.path userId: UserId,
      @param.header authorization: UserToken,
  ): IO[ApiError, Seq[User]]

}
