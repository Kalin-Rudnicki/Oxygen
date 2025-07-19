package oxygen.http.core.internal

import java.io.*
import java.nio.charset.StandardCharsets
import oxygen.predef.core.*
import scala.util.Try
import zio.{System as _, *}

final class HttpBufferedReader(stream: InputStream) {

  private var current: Int = -2

  private var currentBufferIndex: Int = 0
  private var buffer: Array[Byte] = new Array[Byte](256)

  private def bufferChar(char: Int): Unit = {
    if (char == -1)
      throw new EOFException(s"Unexpected EOF, current buffer: ${bufferResult().unesc}")

    if (currentBufferIndex >= buffer.length) {
      if (buffer.length == Int.MaxValue)
        throw new RuntimeException("buffer size exceeded")

      val newLength: Long = buffer.length.toLong * 4
      val newBuffer: Array[Byte] = new Array[Byte](if (newLength > Int.MaxValue) Int.MaxValue else newLength.toInt)
      System.arraycopy(buffer, 0, newBuffer, 0, buffer.length)
      buffer = newBuffer
    }

    buffer(currentBufferIndex) = char.toByte
    currentBufferIndex = currentBufferIndex + 1
  }

  private def peekBuffer(): String = {
    val byteArray: Array[Byte] = new Array[Byte](currentBufferIndex)
    System.arraycopy(buffer, 0, byteArray, 0, currentBufferIndex)
    new String(byteArray, StandardCharsets.UTF_8)
  }
  private def bufferResult(): String = {
    val res = peekBuffer()
    currentBufferIndex = 0
    res
  }

  def isEOF(): Boolean = {
    if (current == -2)
      current = stream.read()

    current == -1
  }

  def readUntilSpaceAndSkipSpaces(): String = {
    if (current == -2)
      current = stream.read()
    while (current != ' ') {
      bufferChar(current)
      current = stream.read()
    }
    while (current == ' ')
      current = stream.read()

    bufferResult()
  }

  def readUntilColonAndSkipSpaces(): String = {
    if (current == -2)
      current = stream.read()
    while (current != ':') {
      bufferChar(current)
      current = stream.read()
    }
    current = stream.read()
    while (current == ' ')
      current = stream.read()

    bufferResult()
  }

  def readUntilEOL(readNextChar: Boolean = true): String = {
    if (current == -2)
      current = stream.read()
    while (current != '\r') {
      bufferChar(current)
      current = stream.read()
    }
    current = stream.read()
    if (current != '\n')
      throw new RuntimeException("Expected '\\n' to follow '\\r'")
    current = if (readNextChar) stream.read() else -2

    bufferResult()
  }

  def isEOL(): Boolean =
    current == '\r'

  def consumeEOL(readNextChar: Boolean): Unit = {
    if (current == -2)
      current = stream.read()
    if (current != '\r')
      throw new RuntimeException("Expected EOL")
    current = stream.read()
    if (current != '\n')
      throw new RuntimeException("Expected '\\n' to follow '\\r'")
    current = if (readNextChar) stream.read() else -2

    ()
  }

  def readNBytes(n: Int): Array[Byte] =
    if (n != 0) {
      if (current != -2)
        throw new RuntimeException("<readNBytes> Not set up for byte read")

      val byteArray: Array[Byte] = new Array[Byte](n)
      val numRead: Int = stream.read(byteArray, 0, n)
      if (numRead != n)
        throw new RuntimeException(s"Received unexpected number of bytes (non-stream) ($numRead != $n)")

      byteArray
    } else
      new Array[Byte](0)

  def rawStream(): InputStream = {
    if (current != -2)
      throw new RuntimeException("<rawStream> Not set up for byte read")

    stream
  }

  private def readChunkLine(): Int = {
    val str: String = readUntilEOL(readNextChar = false)
    Try { Integer.parseUnsignedInt(str, 16) }.toOption match
      case Some(value) => value
      case None        => throw new RuntimeException(s"Not a valid # of hex bytes: $str")
  }

  def chunkedStream(): URIO[Scope, InputStream] = {
    if (current != -2)
      throw new RuntimeException("<chunkedStream> Not set up for byte read")

    val in = PipedInputStream()
    val out = PipedOutputStream(in)

    current = stream.read()

    val asyncEffect: Task[Unit] =
      ZIO.asyncZIO[Any, Throwable, Unit] { register =>
        var chunkSize: Int = 0
        var bytes: Array[Byte] = null

        val thread: Thread = Thread { () =>
          try {
            while ({ chunkSize = readChunkLine(); chunkSize > 0 }) {

              if (bytes == null || chunkSize > bytes.length)
                bytes = new Array[Byte](chunkSize)

              var numRead: Int = 0

              while (chunkSize > 0) {
                numRead = stream.read(bytes, 0, chunkSize)
                if (numRead > 0) {
                  out.write(bytes, 0, numRead)
                  chunkSize = chunkSize - numRead
                }
              }

              if (chunkSize < 0)
                throw new RuntimeException("over-read stream?")

              consumeEOL(readNextChar = true)
            }

            register(ZIO.unit)
          } catch {
            case e: Throwable =>
              register(ZIO.fail(e))
          }
        }

        ZIO.attempt { thread.start() }
      }

    for {
      fiber <- asyncEffect.orDie.forkScoped
      _ <- ZIO.addFinalizer { fiber.join }
      _ <- ZIO.addFinalizer { ZIO.attempt { out.flush() }.orDie }
      _ <- ZIO.addFinalizer { ZIO.attempt { out.close() }.orDie }
    } yield in
  }

}
