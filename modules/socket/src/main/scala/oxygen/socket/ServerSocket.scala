package oxygen.socket

import oxygen.predef.zio.*

final class ServerSocket private (socket: java.net.ServerSocket) {

  private def accept: Task[Socket] =
    ZIO.attempt { socket.accept() }.flatMap(Socket.serverClient)

  private def close: UIO[Unit] =
    ZIO.attempt { socket.close() }.ignore

}
object ServerSocket {

  def open[R](port: Int)(handle: Socket => RIO[R, Unit]): RIO[R, Nothing] =
    ZIO.scoped {
      ZIO.attempt { ServerSocket(java.net.ServerSocket(port)) }.withFinalizer(_.close).flatMap { server =>
        server.accept.flatMap(handle(_).ignore.fork).forever
      }
    }

}
