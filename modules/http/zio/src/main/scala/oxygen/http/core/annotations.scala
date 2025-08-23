package oxygen.http.core

import oxygen.meta.*
import scala.annotation.Annotation
import zio.http.{Method, Status}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      route
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed abstract class route(final val method: Method) extends Annotation derives FromExprT {
  val url: String
}
object route {

  final case class get(url: String) extends route(Method.GET)
  final case class post(url: String) extends route(Method.POST)
  final case class put(url: String) extends route(Method.PUT)
  final case class patch(url: String) extends route(Method.PATCH)
  final case class delete(url: String) extends route(Method.DELETE)
  final case class head(url: String) extends route(Method.HEAD)
  final case class options(url: String) extends route(Method.OPTIONS)
  final case class connect(url: String) extends route(Method.CONNECT)
  final case class trace(url: String) extends route(Method.TRACE)

  final case class custom(name: String, url: String) extends route(Method.CUSTOM(name))

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      param
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed abstract class param extends Annotation derives FromExprT
object param {

  sealed abstract class PathLike extends param
  sealed abstract class NonPathLike extends param
  sealed abstract class QueryLike extends NonPathLike
  sealed abstract class HeaderLike extends NonPathLike
  sealed abstract class BodyLike extends NonPathLike

  // =====|  |=====

  final case class path() extends PathLike // TODO (KR) : optional name
  object path {
    final case class custom() extends PathLike
    final case class plain() extends PathLike // TODO (KR) : optional name
    final case class json() extends PathLike // TODO (KR) : optional name
  }

  object nonPath {
    final case class custom() extends NonPathLike
  }

  final case class query() extends QueryLike // TODO (KR) : optional name
  object query {
    final case class plain() extends QueryLike // TODO (KR) : optional name
    final case class json() extends QueryLike // TODO (KR) : optional name
  }

  final case class header() extends HeaderLike // TODO (KR) : optional name
  object header {
    final case class plain() extends HeaderLike // TODO (KR) : optional name
    final case class json() extends HeaderLike // TODO (KR) : optional name
  }

  final case class body() extends BodyLike // TODO (KR) : optional name
  object body {
    final case class plain() extends BodyLike // TODO (KR) : optional name
    final case class json() extends BodyLike // TODO (KR) : optional name
  }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      statusCode
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed abstract class statusCode(final val status: Status) extends Annotation derives FromExprT
object statusCode {

  // =====| by name |=====

  final case class Continue() extends statusCode(Status.Continue)
  final case class SwitchingProtocols() extends statusCode(Status.SwitchingProtocols)
  final case class Processing() extends statusCode(Status.Processing)
  final case class Ok() extends statusCode(Status.Ok)
  final case class Created() extends statusCode(Status.Created)
  final case class Accepted() extends statusCode(Status.Accepted)
  final case class NonAuthoritativeInformation() extends statusCode(Status.NonAuthoritativeInformation)
  final case class NoContent() extends statusCode(Status.NoContent)
  final case class ResetContent() extends statusCode(Status.ResetContent)
  final case class PartialContent() extends statusCode(Status.PartialContent)
  final case class MultiStatus() extends statusCode(Status.MultiStatus)
  final case class MultipleChoices() extends statusCode(Status.MultipleChoices)
  final case class MovedPermanently() extends statusCode(Status.MovedPermanently)
  final case class Found() extends statusCode(Status.Found)
  final case class SeeOther() extends statusCode(Status.SeeOther)
  final case class NotModified() extends statusCode(Status.NotModified)
  final case class UseProxy() extends statusCode(Status.UseProxy)
  final case class TemporaryRedirect() extends statusCode(Status.TemporaryRedirect)
  final case class PermanentRedirect() extends statusCode(Status.PermanentRedirect)
  final case class BadRequest() extends statusCode(Status.BadRequest)
  final case class Unauthorized() extends statusCode(Status.Unauthorized)
  final case class PaymentRequired() extends statusCode(Status.PaymentRequired)
  final case class Forbidden() extends statusCode(Status.Forbidden)
  final case class NotFound() extends statusCode(Status.NotFound)
  final case class MethodNotAllowed() extends statusCode(Status.MethodNotAllowed)
  final case class NotAcceptable() extends statusCode(Status.NotAcceptable)
  final case class ProxyAuthenticationRequired() extends statusCode(Status.ProxyAuthenticationRequired)
  final case class RequestTimeout() extends statusCode(Status.RequestTimeout)
  final case class Conflict() extends statusCode(Status.Conflict)
  final case class Gone() extends statusCode(Status.Gone)
  final case class LengthRequired() extends statusCode(Status.LengthRequired)
  final case class PreconditionFailed() extends statusCode(Status.PreconditionFailed)
  final case class RequestEntityTooLarge() extends statusCode(Status.RequestEntityTooLarge)
  final case class RequestUriTooLong() extends statusCode(Status.RequestUriTooLong)
  final case class UnsupportedMediaType() extends statusCode(Status.UnsupportedMediaType)
  final case class RequestedRangeNotSatisfiable() extends statusCode(Status.RequestedRangeNotSatisfiable)
  final case class ExpectationFailed() extends statusCode(Status.ExpectationFailed)
  final case class MisdirectedRequest() extends statusCode(Status.MisdirectedRequest)
  final case class UnprocessableEntity() extends statusCode(Status.UnprocessableEntity)
  final case class Locked() extends statusCode(Status.Locked)
  final case class FailedDependency() extends statusCode(Status.FailedDependency)
  final case class UnorderedCollection() extends statusCode(Status.UnorderedCollection)
  final case class UpgradeRequired() extends statusCode(Status.UpgradeRequired)
  final case class PreconditionRequired() extends statusCode(Status.PreconditionRequired)
  final case class TooManyRequests() extends statusCode(Status.TooManyRequests)
  final case class RequestHeaderFieldsTooLarge() extends statusCode(Status.RequestHeaderFieldsTooLarge)
  final case class InternalServerError() extends statusCode(Status.InternalServerError)
  final case class NotImplemented() extends statusCode(Status.NotImplemented)
  final case class BadGateway() extends statusCode(Status.BadGateway)
  final case class ServiceUnavailable() extends statusCode(Status.ServiceUnavailable)
  final case class GatewayTimeout() extends statusCode(Status.GatewayTimeout)
  final case class HttpVersionNotSupported() extends statusCode(Status.HttpVersionNotSupported)
  final case class VariantAlsoNegotiates() extends statusCode(Status.VariantAlsoNegotiates)
  final case class InsufficientStorage() extends statusCode(Status.InsufficientStorage)
  final case class NotExtended() extends statusCode(Status.NotExtended)
  final case class NetworkAuthenticationRequired() extends statusCode(Status.NetworkAuthenticationRequired)

  // =====| by code |=====

  final case class `100`() extends statusCode(Status.Continue)
  final case class `101`() extends statusCode(Status.SwitchingProtocols)
  final case class `102`() extends statusCode(Status.Processing)
  final case class `200`() extends statusCode(Status.Ok)
  final case class `201`() extends statusCode(Status.Created)
  final case class `202`() extends statusCode(Status.Accepted)
  final case class `203`() extends statusCode(Status.NonAuthoritativeInformation)
  final case class `204`() extends statusCode(Status.NoContent)
  final case class `205`() extends statusCode(Status.ResetContent)
  final case class `206`() extends statusCode(Status.PartialContent)
  final case class `207`() extends statusCode(Status.MultiStatus)
  final case class `300`() extends statusCode(Status.MultipleChoices)
  final case class `301`() extends statusCode(Status.MovedPermanently)
  final case class `302`() extends statusCode(Status.Found)
  final case class `303`() extends statusCode(Status.SeeOther)
  final case class `304`() extends statusCode(Status.NotModified)
  final case class `305`() extends statusCode(Status.UseProxy)
  final case class `307`() extends statusCode(Status.TemporaryRedirect)
  final case class `308`() extends statusCode(Status.PermanentRedirect)
  final case class `400`() extends statusCode(Status.BadRequest)
  final case class `401`() extends statusCode(Status.Unauthorized)
  final case class `402`() extends statusCode(Status.PaymentRequired)
  final case class `403`() extends statusCode(Status.Forbidden)
  final case class `404`() extends statusCode(Status.NotFound)
  final case class `405`() extends statusCode(Status.MethodNotAllowed)
  final case class `406`() extends statusCode(Status.NotAcceptable)
  final case class `407`() extends statusCode(Status.ProxyAuthenticationRequired)
  final case class `408`() extends statusCode(Status.RequestTimeout)
  final case class `409`() extends statusCode(Status.Conflict)
  final case class `410`() extends statusCode(Status.Gone)
  final case class `411`() extends statusCode(Status.LengthRequired)
  final case class `412`() extends statusCode(Status.PreconditionFailed)
  final case class `413`() extends statusCode(Status.RequestEntityTooLarge)
  final case class `414`() extends statusCode(Status.RequestUriTooLong)
  final case class `415`() extends statusCode(Status.UnsupportedMediaType)
  final case class `416`() extends statusCode(Status.RequestedRangeNotSatisfiable)
  final case class `417`() extends statusCode(Status.ExpectationFailed)
  final case class `421`() extends statusCode(Status.MisdirectedRequest)
  final case class `422`() extends statusCode(Status.UnprocessableEntity)
  final case class `423`() extends statusCode(Status.Locked)
  final case class `424`() extends statusCode(Status.FailedDependency)
  final case class `425`() extends statusCode(Status.UnorderedCollection)
  final case class `426`() extends statusCode(Status.UpgradeRequired)
  final case class `428`() extends statusCode(Status.PreconditionRequired)
  final case class `429`() extends statusCode(Status.TooManyRequests)
  final case class `431`() extends statusCode(Status.RequestHeaderFieldsTooLarge)
  final case class `500`() extends statusCode(Status.InternalServerError)
  final case class `501`() extends statusCode(Status.NotImplemented)
  final case class `502`() extends statusCode(Status.BadGateway)
  final case class `503`() extends statusCode(Status.ServiceUnavailable)
  final case class `504`() extends statusCode(Status.GatewayTimeout)
  final case class `505`() extends statusCode(Status.HttpVersionNotSupported)
  final case class `506`() extends statusCode(Status.VariantAlsoNegotiates)
  final case class `507`() extends statusCode(Status.InsufficientStorage)
  final case class `510`() extends statusCode(Status.NotExtended)
  final case class `511`() extends statusCode(Status.NetworkAuthenticationRequired)

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Doc
//////////////////////////////////////////////////////////////////////////////////////////////////////

final case class httpDoc(doc: String) extends Annotation derives FromExprT
final case class apiName(name: String) extends Annotation derives FromExprT
final case class endpointName(name: String) extends Annotation derives FromExprT
