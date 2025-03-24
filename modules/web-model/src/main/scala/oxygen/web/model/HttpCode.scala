package oxygen.web.model

import oxygen.core.{Enum, NonEmptyList}

sealed abstract class HttpCode(val code: Int, val name: String) {
  private final val hundred = code / 100

  final def is1xx: Boolean = hundred == 1
  final def is2xx: Boolean = hundred == 2
  final def is3xx: Boolean = hundred == 3
  final def is4xx: Boolean = hundred == 4
  final def is5xx: Boolean = hundred == 5

  final def is4xxOr5xx: Boolean = is4xx || is5xx

  override def toString: String = s"$code - $name"

}
object HttpCode {

  def apply(code: Int): HttpCode = HttpCode.Standard.codeMap.decode(code).getOrElse(HttpCode.NonStandard(code))

  sealed abstract class Standard(code: Int, name: String) extends HttpCode(code, name), Enum[HttpCode.Standard]
  object Standard extends Enum.Companion[HttpCode.Standard] {

    // format: off
    override val values: Array[Standard] = Array(HttpCode.Continue, HttpCode.SwitchingProtocols, HttpCode.Processing, HttpCode.Ok, HttpCode.Created, HttpCode.Accepted, HttpCode.NonAuthoritativeInformation, HttpCode.NoContent, HttpCode.ResetContent, HttpCode.PartialContent, HttpCode.MultiStatus, HttpCode.AlreadyReported, HttpCode.ImUsed, HttpCode.MultipleChoices, HttpCode.MovedPermanently, HttpCode.Found, HttpCode.SeeOther, HttpCode.NotModified, HttpCode.UseProxy, HttpCode.TemporaryRedirect, HttpCode.PermanentRedirect, HttpCode.BadRequest, HttpCode.Unauthorized, HttpCode.PaymentRequired, HttpCode.Forbidden, HttpCode.NotFound, HttpCode.MethodNotAllowed, HttpCode.NotAcceptable, HttpCode.ProxyAuthenticationRequired, HttpCode.RequestTimeout, HttpCode.Conflict, HttpCode.Gone, HttpCode.LengthRequired, HttpCode.PreconditionFailed, HttpCode.PayloadTooLarge, HttpCode.UriTooLong, HttpCode.UnsupportedMediaType, HttpCode.RangeNotSatisfiable, HttpCode.ExpectationFailed, HttpCode.ImATeapot, HttpCode.MisdirectedRequest, HttpCode.UnprocessableEntity, HttpCode.Locked, HttpCode.FailedDependency, HttpCode.TooEarly, HttpCode.UpgradeRequired, HttpCode.PreconditionRequired, HttpCode.TooManyRequests, HttpCode.RequestHeaderFieldsTooLarge, HttpCode.UnavailableForLegalReasons, HttpCode.InternalServerError, HttpCode.NotImplemented, HttpCode.BadGateway, HttpCode.ServiceUnavailable, HttpCode.GatewayTimeout, HttpCode.HttpVersionNotSupported, HttpCode.VariantAlsoNegotiates, HttpCode.InsufficientStorage, HttpCode.LoopDetected, HttpCode.NotExtended, HttpCode.NetworkAuthenticationRequired)
    // format: on

    implicit object codeMap extends EnumMap[Int](c => NonEmptyList.one(c.code))

  }

  final case class NonStandard private[HttpCode] (_code: Int) extends HttpCode(_code, s"Non-Standard HTTP Code (${_code})")

  // =====| By Name |=====

  case object Continue extends HttpCode.Standard(100, "Continue")
  case object SwitchingProtocols extends HttpCode.Standard(101, "Switching Protocols")
  case object Processing extends HttpCode.Standard(102, "Processing")

  case object Ok extends HttpCode.Standard(200, "OK")
  case object Created extends HttpCode.Standard(201, "Created")
  case object Accepted extends HttpCode.Standard(202, "Accepted")
  case object NonAuthoritativeInformation extends HttpCode.Standard(203, "Non-Authoritative Information")
  case object NoContent extends HttpCode.Standard(204, "No Content")
  case object ResetContent extends HttpCode.Standard(205, "Reset Content")
  case object PartialContent extends HttpCode.Standard(206, "Partial Content")
  case object MultiStatus extends HttpCode.Standard(207, "Multi-Status")
  case object AlreadyReported extends HttpCode.Standard(208, "Already Reported")
  case object ImUsed extends HttpCode.Standard(226, "IM Used")

  case object MultipleChoices extends HttpCode.Standard(300, "Multiple Choices")
  case object MovedPermanently extends HttpCode.Standard(301, "Moved Permanently")
  case object Found extends HttpCode.Standard(302, "Found")
  case object SeeOther extends HttpCode.Standard(303, "See Other")
  case object NotModified extends HttpCode.Standard(304, "Not Modified")
  case object UseProxy extends HttpCode.Standard(305, "Use Proxy")
  case object TemporaryRedirect extends HttpCode.Standard(307, "Temporary Redirect")
  case object PermanentRedirect extends HttpCode.Standard(308, "Permanent Redirect")

  case object BadRequest extends HttpCode.Standard(400, "Bad Request")
  case object Unauthorized extends HttpCode.Standard(401, "Unauthorized")
  case object PaymentRequired extends HttpCode.Standard(402, "Payment Required")
  case object Forbidden extends HttpCode.Standard(403, "Forbidden")
  case object NotFound extends HttpCode.Standard(404, "Not Found")
  case object MethodNotAllowed extends HttpCode.Standard(405, "Method Not Allowed")
  case object NotAcceptable extends HttpCode.Standard(406, "Not Acceptable")
  case object ProxyAuthenticationRequired extends HttpCode.Standard(407, "Proxy Authentication Required")
  case object RequestTimeout extends HttpCode.Standard(408, "Request Timeout")
  case object Conflict extends HttpCode.Standard(409, "Conflict")
  case object Gone extends HttpCode.Standard(410, "Gone")
  case object LengthRequired extends HttpCode.Standard(411, "Length Required")
  case object PreconditionFailed extends HttpCode.Standard(412, "Precondition Failed")
  case object PayloadTooLarge extends HttpCode.Standard(413, "Payload Too Large")
  case object UriTooLong extends HttpCode.Standard(414, "URI Too Long")
  case object UnsupportedMediaType extends HttpCode.Standard(415, "Unsupported Media Type")
  case object RangeNotSatisfiable extends HttpCode.Standard(416, "Range Not Satisfiable")
  case object ExpectationFailed extends HttpCode.Standard(417, "Expectation Failed")
  case object ImATeapot extends HttpCode.Standard(418, "I'm a teapot")
  case object MisdirectedRequest extends HttpCode.Standard(421, "Misdirected Request")
  case object UnprocessableEntity extends HttpCode.Standard(422, "Unprocessable Entity")
  case object Locked extends HttpCode.Standard(423, "Locked")
  case object FailedDependency extends HttpCode.Standard(424, "Failed Dependency")
  case object TooEarly extends HttpCode.Standard(425, "Too Early")
  case object UpgradeRequired extends HttpCode.Standard(426, "Upgrade Required")
  case object PreconditionRequired extends HttpCode.Standard(428, "Precondition Required")
  case object TooManyRequests extends HttpCode.Standard(429, "Too Many Requests")
  case object RequestHeaderFieldsTooLarge extends HttpCode.Standard(431, "Request Header Fields Too Large")
  case object UnavailableForLegalReasons extends HttpCode.Standard(451, "Unavailable For Legal Reasons")

  case object InternalServerError extends HttpCode.Standard(500, "Internal Server Error")
  case object NotImplemented extends HttpCode.Standard(501, "Not Implemented")
  case object BadGateway extends HttpCode.Standard(502, "Bad Gateway")
  case object ServiceUnavailable extends HttpCode.Standard(503, "Service Unavailable")
  case object GatewayTimeout extends HttpCode.Standard(504, "Gateway Timeout")
  case object HttpVersionNotSupported extends HttpCode.Standard(505, "HTTP Version Not Supported")
  case object VariantAlsoNegotiates extends HttpCode.Standard(506, "Variant Also Negotiates")
  case object InsufficientStorage extends HttpCode.Standard(507, "Insufficient Storage")
  case object LoopDetected extends HttpCode.Standard(508, "Loop Detected")
  case object NotExtended extends HttpCode.Standard(510, "Not Extended")
  case object NetworkAuthenticationRequired extends HttpCode.Standard(511, "Network Authentication Required")

  // =====| By Code |=====

  inline def `100`: Continue.type = Continue
  inline def `101`: SwitchingProtocols.type = SwitchingProtocols
  inline def `102`: Processing.type = Processing

  inline def `200`: Ok.type = Ok
  inline def `201`: Created.type = Created
  inline def `202`: Accepted.type = Accepted
  inline def `203`: NonAuthoritativeInformation.type = NonAuthoritativeInformation
  inline def `204`: NoContent.type = NoContent
  inline def `205`: ResetContent.type = ResetContent
  inline def `206`: PartialContent.type = PartialContent
  inline def `207`: MultiStatus.type = MultiStatus
  inline def `208`: AlreadyReported.type = AlreadyReported
  inline def `226`: ImUsed.type = ImUsed

  inline def `300`: MultipleChoices.type = MultipleChoices
  inline def `301`: MovedPermanently.type = MovedPermanently
  inline def `302`: Found.type = Found
  inline def `303`: SeeOther.type = SeeOther
  inline def `304`: NotModified.type = NotModified
  inline def `305`: UseProxy.type = UseProxy
  inline def `307`: TemporaryRedirect.type = TemporaryRedirect
  inline def `308`: PermanentRedirect.type = PermanentRedirect

  inline def `400`: BadRequest.type = BadRequest
  inline def `401`: Unauthorized.type = Unauthorized
  inline def `402`: PaymentRequired.type = PaymentRequired
  inline def `403`: Forbidden.type = Forbidden
  inline def `404`: NotFound.type = NotFound
  inline def `405`: MethodNotAllowed.type = MethodNotAllowed
  inline def `406`: NotAcceptable.type = NotAcceptable
  inline def `407`: ProxyAuthenticationRequired.type = ProxyAuthenticationRequired
  inline def `408`: RequestTimeout.type = RequestTimeout
  inline def `409`: Conflict.type = Conflict
  inline def `410`: Gone.type = Gone
  inline def `411`: LengthRequired.type = LengthRequired
  inline def `412`: PreconditionFailed.type = PreconditionFailed
  inline def `413`: PayloadTooLarge.type = PayloadTooLarge
  inline def `414`: UriTooLong.type = UriTooLong
  inline def `415`: UnsupportedMediaType.type = UnsupportedMediaType
  inline def `416`: RangeNotSatisfiable.type = RangeNotSatisfiable
  inline def `417`: ExpectationFailed.type = ExpectationFailed
  inline def `418`: ImATeapot.type = ImATeapot
  inline def `421`: MisdirectedRequest.type = MisdirectedRequest
  inline def `422`: UnprocessableEntity.type = UnprocessableEntity
  inline def `423`: Locked.type = Locked
  inline def `424`: FailedDependency.type = FailedDependency
  inline def `425`: TooEarly.type = TooEarly
  inline def `426`: UpgradeRequired.type = UpgradeRequired
  inline def `428`: PreconditionRequired.type = PreconditionRequired
  inline def `429`: TooManyRequests.type = TooManyRequests
  inline def `431`: RequestHeaderFieldsTooLarge.type = RequestHeaderFieldsTooLarge
  inline def `451`: UnavailableForLegalReasons.type = UnavailableForLegalReasons

  inline def `500`: InternalServerError.type = InternalServerError
  inline def `501`: NotImplemented.type = NotImplemented
  inline def `502`: BadGateway.type = BadGateway
  inline def `503`: ServiceUnavailable.type = ServiceUnavailable
  inline def `504`: GatewayTimeout.type = GatewayTimeout
  inline def `505`: HttpVersionNotSupported.type = HttpVersionNotSupported
  inline def `506`: VariantAlsoNegotiates.type = VariantAlsoNegotiates
  inline def `507`: InsufficientStorage.type = InsufficientStorage
  inline def `508`: LoopDetected.type = LoopDetected
  inline def `510`: NotExtended.type = NotExtended
  inline def `511`: NetworkAuthenticationRequired.type = NetworkAuthenticationRequired

  // =====| Other / Helpers |=====

  given ordering: Ordering[HttpCode] = Ordering.by(_.code)

}
