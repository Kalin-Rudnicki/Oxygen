package oxygen.socket

import oxygen.predef.zio.*
import zio.ZIOAppDefault
import zio.stream.*

object TmpMain extends ZIOAppDefault {

  /*

  sealed trait GameEvent derives JsonCodec
  object GameEvent {
    final case class Start(gameId: UUID, home: String, away: String) extends GameEvent
    final case class Score(gameId: UUID, home: Int, away: Int) extends GameEvent
    final case class End(gameId: UUID) extends GameEvent
  }

  private def doWait: UIO[Unit] = Random.nextIntBetween(500, 5000).flatMap(i => Clock.sleep(i.millis))

  private def doGame(pub: PubSub.Publisher[GameEvent], home: String, away: String)(scores: (Int, Int)*): Task[Unit] =
    Random.nextUUID
      .flatMap { gameId =>
        doWait *>
          pub.publish(GameEvent.Start(gameId, home, away)) *>
          ZIO.foreachDiscard(scores) { case (home, away) => doWait *> pub.publish(GameEvent.Score(gameId, home, away)) } *>
          doWait *>
          pub.publish(GameEvent.End(gameId))
      }
      .tapErrorCause(Logger.log.error(_))
      .forkDaemon
      .unit


  override def run: RIO[Scope, Unit] =
    for {
      _ <- Logger.defaultToOxygen.set
      _ <- Logger.level.trace.set
      _ <- Logger.log.info("Socket Fun!")

      host = "localhost"
      port = 4765
      topic1 <- PubSub.topic[GameEvent]("game-event")(using StringCodec.usingJsonCodec)

      _ <- PubSub.startServer(port).fork

      pub1 <- topic1.publisher(host, port)

      _ <- topic1.consume(host, port) { update => Logger.log.info(s"Game Event: \n$update") }.forkDaemon
      _ <- topic1
        .consume(host, port) {
          case GameEvent.Start(_, home, away) => Logger.log.important(s"New game! ${home.unesc} vs ${away.unesc}")
          case _                              => ZIO.unit
        }
        .forkDaemon

      _ <- Clock.sleep(100.millis)

      _ <- doGame(pub1, "Bears", "Packers")(
        (6, 0),
        (7, 0),
        (10, 0),
        (10, 3),
        (16, 3),
        (18, 3),
      )
      _ <- doGame(pub1, "Cubs", "Rockies")(
        (1, 0),
        (2, 0),
        (2, 1),
        (2, 2),
        (3, 3),
      )
      _ <- ZIO.never
    } yield ()
   */

  override def run: RIO[Scope, Unit] =
    for {
      _ <- Logger.defaultToOxygen.set
      _ <- Logger.level.trace.set
      _ <- Logger.log.info("Stream Fun!")

      stream =
        for {
          _ <- ZStream.fromZIO { Logger.log.info("Creating stream...") }
          _ <- ZStream.scoped { ZIO.addFinalizer { Logger.log.info("finalizer") } }
          i <- ZStream.fromChunk { Chunk.fromIterable(0.to(10)) }
          _ <- ZStream.fromZIO { Logger.log.info(s"element: $i") }
        } yield ()

      _ <- stream.runDrain
    } yield ()

}
