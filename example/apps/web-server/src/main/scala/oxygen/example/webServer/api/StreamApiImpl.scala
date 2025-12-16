package oxygen.example.webServer.api

import oxygen.example.api.StreamApi
import oxygen.http.core.ServerSentEvents
import oxygen.zio.syntax.log.*
import zio.*
import zio.stream.*

final case class StreamApiImpl() extends StreamApi {

  override def randomUUIDs(): ServerSentEvents[Nothing, StreamApi.UUIDEvent] =
    ServerSentEvents.succeed {
      ZStream.logInfo("Starting UUID stream") *>
        ZStream.scoped { ZIO.addFinalizer { ZIO.logWarning("Server finalized") } } *>
        ZStream.repeatZIO {
          for {
            sleepMillis <- Random.nextIntBetween(100, 5000)
            _ <- Clock.sleep(sleepMillis.millis)
            uuid <- Random.nextUUID
            now <- Clock.instant
            _ <- ZIO.logDetailedAnnotated("Emitting UUID", "uuid-value" -> uuid.toString)
          } yield StreamApi.UUIDEvent(uuid, now)
        }
    }

}
object StreamApiImpl {

  val layer: ULayer[StreamApi] =
    ZLayer.succeed { StreamApiImpl() }

}
