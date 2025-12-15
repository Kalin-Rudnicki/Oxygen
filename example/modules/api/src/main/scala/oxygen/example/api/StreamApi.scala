package oxygen.example.api

import java.time.Instant
import java.util.UUID
import oxygen.http.client.DeriveClient
import oxygen.http.core.*
import oxygen.schema.JsonSchema
import zio.*
import zio.stream.*

trait StreamApi derives DeriveClient {

  @route.get("/api/stream/random-uuid")
  def randomUUIDs(): ServerSentEvents[Nothing, StreamApi.UUIDEvent]

}
object StreamApi {

  final case class UUIDEvent(
      id: UUID,
      timestamp: Instant,
  ) derives JsonSchema

  def randomUUIDs: ZStream[StreamApi, Nothing, StreamApi.UUIDEvent] =
    ZStream.serviceWithStream[StreamApi](_.randomUUIDs().toStream)

}
