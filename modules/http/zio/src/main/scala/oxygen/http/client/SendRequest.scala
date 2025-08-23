package oxygen.http.client

import oxygen.http.core.{RequestNonPathCodec, RequestPathCodec}
import oxygen.predef.core.*
import scala.annotation.tailrec
import zio.http.*

final case class SendRequest(
    method: Method,
    path: Path,
    queryParams: QueryParams,
    headers: Headers,
    body: Body,
)
object SendRequest {

  def simple(
      method: Method,
      path: String,
      body: String,
  ): SendRequest =
    SendRequest(
      method,
      Path(path),
      QueryParams.empty,
      Headers.empty,
      Body.fromString(body),
    )

  enum AppliedValue[A] {
    case Path(codec: RequestPathCodec[A], value: A)
    case NonPath(codec: RequestNonPathCodec[A], value: A)
  }

  def make(values: AppliedValue[?]*): (Path, QueryParams, Headers, Body) = {
    @tailrec
    def loop(
        queue: List[AppliedValue[?]],
        pathAcc: Growable[String],
        nonPathAcc: RequestNonPathCodec.Builder,
    ): (Path, QueryParams, Headers, Body) =
      queue match {
        case (head: AppliedValue.Path[?]) :: tail =>
          loop(tail, pathAcc ++ head.codec.encode(head.value), nonPathAcc)
        case (head: AppliedValue.NonPath[?]) :: tail =>
          loop(tail, pathAcc, head.codec.encodeInternal(head.value, nonPathAcc))
        case Nil =>
          val path = pathAcc.toArraySeq.foldLeft(Path.empty) { _ / _ }
          val (queryParams, headers, body) = nonPathAcc.build
          (path, queryParams, headers, body)
      }

    loop(values.toList, Growable.empty, RequestNonPathCodec.Builder.empty)
  }

}
