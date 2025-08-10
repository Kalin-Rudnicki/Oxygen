package oxygen.http.core

import oxygen.http.model.*
import oxygen.meta.*
import scala.annotation.Annotation

sealed abstract class route(final val method: HttpMethod.Standard) extends Annotation derives FromExprT {
  val url: String
}
object route {

  final case class get(url: String) extends route(HttpMethod.GET)
  final case class post(url: String) extends route(HttpMethod.POST)
  final case class put(url: String) extends route(HttpMethod.PUT)
  final case class patch(url: String) extends route(HttpMethod.PATCH)
  final case class delete(url: String) extends route(HttpMethod.DELETE)
  final case class head(url: String) extends route(HttpMethod.HEAD)
  final case class options(url: String) extends route(HttpMethod.OPTIONS)
  final case class connect(url: String) extends route(HttpMethod.CONNECT)
  final case class trace(url: String) extends route(HttpMethod.TRACE)

}

sealed abstract class param extends Annotation derives FromExprT
object param {

  final case class path() extends param

  sealed abstract class query extends param
  object query {
    final case class plain() extends query
    final case class json() extends query
  }

  sealed abstract class header extends param
  object header {
    final case class plain() extends header
    final case class json() extends header
  }

  sealed abstract class body extends param
  object body {
    final case class plain() extends body
    final case class json() extends body
  }

}

// TODO (KR) : explore how to support ' @httpCode.`200` '
final case class httpCode(code: HttpCode.Standard) extends Annotation derives FromExprT
object httpCode {

  given FromExprT[HttpCode.Standard] = FromExprT.derived

}
