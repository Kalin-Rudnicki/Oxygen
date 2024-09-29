package oxygen.socket

import java.util.UUID
import oxygen.predef.core.*
import oxygen.predef.zio.*

object PubSub {

  enum ClientType extends Enum[ClientType] { case Publisher, Consumer }
  object ClientType extends Enum.Companion[ClientType]

  def startServer(port: Int): RIO[Scope, Nothing] =
    Logger.log.info(s"Starting server on port: $port") *>
      Ref.make(Map.empty[UUID, String]).flatMap { clientIdToTopicsRef =>
        Ref.make(Map.empty[String, Set[Socket]]).flatMap { topicToSocketsRef =>
          val server = Server(clientIdToTopicsRef, topicToSocketsRef)
          ServerSocket.open(port) { socket =>
            (for {
              clientType <- socket.readMessage[ClientType]
              topic <- socket.readMessage[String]
              _ <- clientType match {
                case ClientType.Publisher =>
                  def loop: Task[Unit] =
                    socket.readRawMessage.flatMap {
                      case Message.Str(message) => server.publish(topic, message) *> loop
                      case Message.HeartBeat    => Logger.log.trace("server received heart-beat") *> loop
                      case Message.Close        => socket.close
                    }

                  loop
                case ClientType.Consumer =>
                  server.add(socket, topic)
              }
            } yield ()).tapErrorCause(Logger.log.error(_))
          }
        }
      }

  def topic[A: StringCodec](topic: String): UIO[Topic[A]] =
    ZIO.dieMessage(s"'\\n' not allowed in topic name ${topic.unesc}").whenDiscard(topic.contains('\n')).as(Topic(topic, StringCodec[A]))

  final class Topic[A](topic: String, stringCodec: StringCodec[A]) {

    def publisher(host: String, port: Int): RIO[Scope, Publisher[A]] =
      for {
        socket <- Socket.connect(host, port)
        _ <- socket.writeMessage(ClientType.Publisher)(using ClientType.stringCodec.encoder)
        _ <- socket.writeMessage(topic)(using StringCodec.string.encoder)
      } yield Publisher(socket, stringCodec.encoder)

    def consume[R](host: String, port: Int)(handle: A => RIO[R, Unit]): RIO[R & Scope, Unit] =
      Socket
        .connect(host, port)
        .flatMap { socket =>
          def loop: RIO[R, Unit] =
            socket.readRawMessage.flatMap {
              case Message.Str(message) =>
                stringCodec.decoder.decode(message) match {
                  case Right(value) => handle(value) *> loop
                  case Left(error)  => ZIO.dieMessage(error)
                }
              case Message.Close     => Logger.log.debug(s"closing client-socket") *> socket.close
              case Message.HeartBeat => Logger.log.trace("client received heart-beat") *> loop
            }

          (
            Logger.log.info(s"Starting consumer: $host/$port") *>
              socket.writeMessage(ClientType.Consumer)(using ClientType.stringCodec.encoder) *>
              socket.writeMessage(topic)(using StringCodec.string.encoder) *>
              loop
          ) @@ Logger.addContext("client-id" -> socket.id)
        }
        .tapErrorCause(Logger.log.error(_))

  }

  final class Server(
      clientIdToTopicsRef: Ref[Map[UUID, String]],
      topicToSocketsRef: Ref[Map[String, Set[Socket]]],
  ) {

    def add(client: Socket, topic: String): UIO[Unit] =
      ZIO.dieMessage("client already added").whenZIODiscard(clientIdToTopicsRef.get.map(_.contains(client.id))) *>
        clientIdToTopicsRef.update(_.updated(client.id, topic)) *>
        topicToSocketsRef.update {
          _.updatedWith(topic) { existing => (existing.getOrElse(Set.empty) + client).some }
        }

    def remove(client: Socket): UIO[Unit] =
      for {
        topic <- clientIdToTopicsRef.get.map(_.get(client.id)).someOrElseZIO { ZIO.dieMessage("client does not exist") }
        _ <- topicToSocketsRef.update {
          _.updatedWith(topic) { existing => (existing.getOrElse(Set.empty) - client).someWhen(_.nonEmpty) }
        }
        _ <- clientIdToTopicsRef.update(_.removed(client.id))
      } yield ()

    def publish(topic: String, message: String): Task[Unit] =
      topicToSocketsRef.get.flatMap { map =>
        ZIO.foreachDiscard(map.getOrElse(topic, Set.empty)) { _.writeRawMessage(Message.Str(message)) }
      }

    def close: UIO[Unit] =
      topicToSocketsRef.get.flatMap { map => ZIO.foreachDiscard(map.values.flatten.toSet) { _.close } } *>
        clientIdToTopicsRef.set(Map.empty) *>
        topicToSocketsRef.set(Map.empty)

  }

  final class Publisher[A](socket: Socket, stringEncoder: StringEncoder[A]) {

    private given StringEncoder[A] = stringEncoder

    def publish(message: A): Task[Unit] =
      // Logger.log.debug(s"Publishing event: $message") *>
      socket.writeMessage(message)

  }

}
