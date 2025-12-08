package oxygen.http.server

import oxygen.http.model.internal.*
import oxygen.predef.core.*
import oxygen.zio.syntax.log.*
import scala.reflect.TypeTest
import zio.*

final case class CurrentRequest private[server] (
    request: ReceivedRequest,
    scope: Scope,
)
object CurrentRequest {

  private[server] val ref: FiberRef[Option[CurrentRequest]] =
    Unsafe.unsafely { FiberRef.unsafe.make(None) }

  val get: UIO[CurrentRequest] =
    ref.get.someOrElseZIO { ZIO.dieMessage("No current request set") }

  def handle[DomainError, ApiError]: HandleBuilder[DomainError, ApiError] = new HandleBuilder

  final class HandleBuilder[DomainError, ApiError] {

    // TODO (KR) : attach domain error in header?
    def apply[Out](
        req: ZIO[Scope, ApiError | DomainError, Out],
    )(using
        getApiError: TypeTest[ApiError | DomainError, ApiError],
        getDomainError: TypeTest[ApiError | DomainError, DomainError],
        showDomainError: Show[DomainError],
        errorLevel: ErrorLevel[DomainError],
        converter: ErrorConverter[DomainError, ApiError],
    ): IO[ApiError, Out] =
      CurrentRequest.get.flatMap { current =>
        val extendedScope: IO[ApiError | DomainError, Out] = current.scope.extend[Any](req)

        // TODO (KR) : use ExtractedCauses
        extendedScope.foldCauseZIO(
          _.failureTraceOrCause match {
            case Left((getApiError(apiError), trace))       => ZIO.refailCause(Cause.fail(apiError, trace))
            case Left((getDomainError(domainError), trace)) =>
              ZIO.logAtLevel(errorLevel.level(domainError))("Converting Domain error to API error", Cause.fail(showDomainError.show(domainError), trace)) *>
                (converter.convert(domainError) match {
                  case Right(apiError) => ZIO.refailCause(Cause.fail(apiError, trace))
                  case Left(defect)    => ZIO.refailCause(Cause.die(defect, trace))
                })
            case Right(cause) =>
              ZIO.refailCause(cause)
          },
          ZIO.succeed(_),
        )
      }

  }

}
