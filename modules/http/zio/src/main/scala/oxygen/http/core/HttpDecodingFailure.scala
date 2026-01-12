package oxygen.http.core

import oxygen.json.*
import oxygen.predef.core.*
import oxygen.schema.JsonSchema
import oxygen.schema.instances.given

// TODO (KR) : have the ability to include the original attempted value
//           : it can be very hard to trace down errors with that missing, especially in the body
//           : @see [[ReadOnlyCachedHttpBody.optionalCachedBody]]
//           : would this go in the cause? source? separate field on the top level ___DecodingFailure?

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      DecodingFailureCause
//////////////////////////////////////////////////////////////////////////////////////////////////////

enum DecodingFailureCause derives JsonSchema {

  case MissingRequired
  case ManyNotSupported(received: Int)
  case DecodeError(error: String, input: DecodingFailureCause.DecodeInput)
  case ExecutionFailure(cause: Throwable)

  override final def toString: String = this match
    case DecodingFailureCause.MissingRequired            => "Missing required parameter"
    case DecodingFailureCause.ManyNotSupported(received) => s"Providing many parameters is not allowed (received $received)"
    case DecodingFailureCause.DecodeError(error, input)  => s"[failed to decode] $error${input.showSuffix}"
    case DecodingFailureCause.ExecutionFailure(cause)    => s"[failed to execute] ${cause.safeGetMessage}"

}
object DecodingFailureCause {

  enum DecodeInput derives JsonSchema {
    case InputValues(values: List[String])
    case Body(body: String)
    case BodySSE(eventData: String)
    case BodyNoValue
    case PreviousValue(valueToString: String)
    case NotApplicable

    final def showSuffix: String = this match
      case DecodeInput.InputValues(Nil)    => "\n\n< no inputs >"
      case DecodeInput.InputValues(values) => values.zipWithIndex.map { case (v, i) => s"  - [$i] : $v" }.mkString("\n\n", "\n", "")
      case DecodeInput.Body("")            => "\n\n< empty body >"
      case DecodeInput.Body(body)          => "\n\n< body >\n" + body
      case DecodeInput.BodySSE(body)       => "\n\n< body : SSE event data >\n" + body
      case DecodeInput.BodyNoValue         => ""
      case DecodeInput.PreviousValue(_)    => ""
      case DecodeInput.NotApplicable       => ""

  }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      HttpDecodingFailure
//////////////////////////////////////////////////////////////////////////////////////////////////////

@jsonDiscriminator("errorType")
sealed trait HttpDecodingFailure extends Throwable derives JsonSchema {

  val cause: DecodingFailureCause

  def show: String

  override final def getMessage: String = show
  override final def toString: String = show

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      RequestDecodingFailure
//////////////////////////////////////////////////////////////////////////////////////////////////////

final case class RequestDecodingFailure(
    sources: List[RequestDecodingFailure.Source],
    cause: DecodingFailureCause,
) extends HttpDecodingFailure derives JsonSchema {

  private def showSources: String = sources match
    case Nil           => "(no sources)"
    case source :: Nil => source.toString
    case _             => sources.mkString("[ ", " , ", " ]")

  def containsAuthHeader: Boolean = sources.exists(_.isAuthHeader)
  def isMissingAuth: Boolean = cause == DecodingFailureCause.MissingRequired && containsAuthHeader

  override def show: String = s"Error decoding http-request $showSources : $cause"

}
object RequestDecodingFailure {

  @jsonDiscriminator("sourceType")
  enum Source derives JsonSchema {

    @jsonType("RequestPath") case Path(name: String)
    @jsonType("RequestQueryParam") case QueryParam(name: String)
    @jsonType("RequestHeader") case Header(name: String)
    @jsonType("RequestBody") case Body

    final def isAuthHeader: Boolean =
      this match {
        case Source.Header(name) =>
          val lowerName: String = name
          authNames.exists(lowerName.contains)
        case _ => false
      }

    override final def toString: String = this match
      case Source.Path(name)       => s"path '$name'"
      case Source.QueryParam(name) => s"query-param '$name'"
      case Source.Header(name)     => s"header '$name'"
      case Source.Body             => "body"

  }

  private val authNames: ArraySeq[String] = ArraySeq("auth", "key", "token", "secret", "password")

  object missingAuth {
    def unapply(error: RequestDecodingFailure): Boolean = error.isMissingAuth
  }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      ResponseDecodingFailure
//////////////////////////////////////////////////////////////////////////////////////////////////////

final case class ResponseDecodingFailure(
    sources: List[ResponseDecodingFailure.Source],
    cause: DecodingFailureCause,
) extends HttpDecodingFailure derives JsonSchema {

  private def showSources: String = sources match
    case Nil           => "(no sources)"
    case source :: Nil => source.toString
    case _             => sources.mkString("[ ", " , ", " ]")

  override def show: String = s"Error decoding http-response $showSources : $cause"

}
object ResponseDecodingFailure {

  @jsonDiscriminator("sourceType")
  enum Source derives JsonSchema {

    @jsonType("ResponseHeader") case Header(name: String)
    @jsonType("ResponseBody") case Body

    override final def toString: String = this match
      case Source.Header(name) => s"response header '$name'"
      case Source.Body         => "response body"

  }

}
