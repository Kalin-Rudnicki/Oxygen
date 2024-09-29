package oxygen.socket

import java.util.UUID
import oxygen.predef.core.*
import oxygen.predef.zio.*
import zio.Semaphore

final class Socket private (val id: UUID, socket: java.net.Socket, readMutex: Semaphore, writeMutex: Semaphore) {

  val in = java.io.DataInputStream(socket.getInputStream)
  val out = java.io.DataOutputStream(socket.getOutputStream)

  private[socket] def readRawMessage: Task[Message] =
    readMutex.withPermit {
      ZIO.attempt { in.readByte() }.flatMap {
        case 0 => ZIO.succeed(Message.Close)
        case 1 => ZIO.succeed(Message.HeartBeat)
        case 2 =>
          for {
            length <- ZIO.attempt { in.readInt() }
            bytes <- ZIO.attempt { in.readNBytes(length) }
            string <- ZIO.attempt { String(bytes) }
          } yield Message.Str(string)
        case b => close *> ZIO.dieMessage(s"Invalid byte: $b")
      }
    }

  private[socket] def writeRawMessage(message: Message): Task[Unit] =
    writeMutex.withPermit {
      message match {
        case Message.Close     => ZIO.attempt { out.writeByte(0) }
        case Message.HeartBeat => ZIO.attempt { out.writeByte(1) }
        case Message.Str(message) =>
          for {
            _ <- ZIO.attempt { out.writeByte(2) }
            bytes = message.getBytes
            _ <- ZIO.attempt { out.writeInt(bytes.length) }
            _ <- ZIO.attempt { out.write(bytes) }
          } yield ()
      }
    }

  def readMessage[A: StringDecoder]: Task[A] =
    readRawMessage.flatMap {
      case Message.Str(message) =>
        StringDecoder[A].decode(message) match {
          case Right(value) => ZIO.succeed(value)
          case Left(value)  => ZIO.dieMessage(value)
        }
      case Message.HeartBeat => readMessage[A]
      case Message.Close     => ZIO.dieMessage("closed")
    }

  def writeMessage[A: StringEncoder](message: A): Task[Unit] =
    writeRawMessage(Message.Str(StringEncoder[A].encode(message)))

  def close: UIO[Unit] =
    writeRawMessage(Message.Close).timeout(1.second).ignore *>
      ZIO.attempt { socket.close() }.ignore

  override def hashCode: Int = id.hashCode

  override def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: Socket => this.id == that.id
    case _            => false

  override def toString: String = s"Socket($id)"

}
object Socket {

  private[socket] def serverClient(socket: java.net.Socket): UIO[Socket] =
    for {
      id <- Random.nextUUID
      readMutex <- Semaphore.make(1)
      writeMutex <- Semaphore.make(1)
    } yield Socket(id, socket, readMutex, writeMutex)

  def connect(host: String, port: Int): RIO[Scope, Socket] =
    for {
      id <- Random.nextUUID
      readMutex <- Semaphore.make(1)
      writeMutex <- Semaphore.make(1)
      socket <- ZIO.attempt { Socket(id, java.net.Socket(host, port), readMutex, writeMutex) }.withFinalizer(_.close)
    } yield socket

}
