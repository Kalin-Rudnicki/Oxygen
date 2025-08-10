package oxygen.http.model

import java.io.InputStream
import java.nio.charset.Charset
import zio.*

sealed trait HttpBody {

  def readString: Task[String] = this match {
    case HttpBody.Empty                                                                => ZIO.dieMessage("empty body")
    case HttpBody.Text(body, _)                                                        => ZIO.succeed(body)
    case HttpBody.Bytes(body, _, charset)                                              => ZIO.succeed(new String(body, charset))
    case HttpBody.Stream(_, _, _, Some(contentLength)) if contentLength > Int.MaxValue => ZIO.dieMessage(s"too big to fit into a string: $contentLength")

    case HttpBody.Stream(stream, _, charset, Some(contentLength)) =>
      ZIO.scoped {
        ZIO
          .asyncZIO[Scope, Throwable, String] { register =>
            ZIO.succeed {
              register(stream.flatMap { stream => ZIO.attempt { new String(stream.readNBytes(contentLength.toInt), charset) } })
            }
          }
      }
    case HttpBody.Stream(stream, _, charset, None) =>
      ZIO.scoped {
        ZIO
          .asyncZIO[Scope, Throwable, String] { register =>
            ZIO.succeed {
              register(stream.flatMap { stream => ZIO.attempt { new String(stream.readAllBytes(), charset) } })
            }
          }
      }
  }

}
object HttpBody {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  val emptyPlain: HttpBody.Text = plain("")
  def plain(body: String): HttpBody.Text = HttpBody.Text(body, ContentType.PlainText)
  def json(body: String): HttpBody.Text = HttpBody.Text(body, ContentType.Json)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Bodies
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  case object Empty extends HttpBody

  sealed trait NonEmpty extends HttpBody

  final case class Text(body: String, contentType: ContentType) extends HttpBody.NonEmpty

  final case class Bytes(body: Array[Byte], contentType: ContentType, charset: Charset) extends HttpBody.NonEmpty

  final case class Stream(stream: URIO[Scope, InputStream], contentType: ContentType, charset: Charset, contentLength: Option[Long]) extends HttpBody.NonEmpty

}
