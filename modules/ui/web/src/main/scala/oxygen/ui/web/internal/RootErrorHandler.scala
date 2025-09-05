package oxygen.ui.web.internal

import oxygen.predef.core.*
import oxygen.ui.web.{PageMessage, PageMessages, UIError}
import oxygen.zio.ExtractedCauses
import zio.*

trait RootErrorHandler {

  protected def handleErrorCauseInternal(errorLocation: RootErrorHandler.ErrorLocation, error: Cause[UIError.NonRedirect]): UIO[Unit]

  final def handleError(errorLocation: RootErrorHandler.ErrorLocation, error: UIError.NonRedirect): UIO[Unit] =
    handleErrorCause(errorLocation, Cause.fail(error))

  final def handleDefect(errorLocation: RootErrorHandler.ErrorLocation, error: Throwable): UIO[Unit] =
    handleErrorCause(errorLocation, Cause.die(error))

  final def handleErrorCause(errorLocation: RootErrorHandler.ErrorLocation, error: Cause[UIError.NonRedirect]): UIO[Unit] =
    ZIO.logDebugCause(s"Handling error in $errorLocation", error) *>
      handleErrorCauseInternal(errorLocation, error)

}
object RootErrorHandler {

  final case class Default[Env](defaultPages: DefaultPages[Env]) extends RootErrorHandler {

    private def showErrors(pageInstance: PageInstance.Untyped, errors: NonEmptyList[PageMessage]): UIO[Unit] =
      PageMessages.PageLocal.update(pageInstance)(_ :++ errors.toList)

    private def handleCauses(pageInstance: PageInstance.Untyped, causes: ExtractedCauses[UIError.NonRedirect]): UIO[Unit] =
      causes match {
        case ExtractedCauses.Failures(failures, _) =>
          failures.collectFirst { case Cause.Fail(UIError.Internal.UrlNotFound(url), _) => url } match {
            case Some(url) => Router.route(NavigationEvent.renderPage(defaultPages.notFound)(url))
            case None      => showErrors(pageInstance, failures.flatMap(_.value.toPageMessages))
          }
        case ExtractedCauses.Defects(defects) => showErrors(pageInstance, defects.flatMap(e => UIError.ClientSide.InternalDefect.somethingWentWrong(e.value).toPageMessages))
        case ExtractedCauses.Other(_)         => showErrors(pageInstance, UIError.ClientSide.InternalDefect.somethingWentWrong("ExtractedCauses.Other").toPageMessages)
      }

    override protected def handleErrorCauseInternal(errorLocation: RootErrorHandler.ErrorLocation, error: Cause[UIError.NonRedirect]): UIO[Unit] =
      errorLocation match
        case ErrorLocation.BrowserLoad    => Router.route(NavigationEvent.renderPage(defaultPages.error)(error))
        case loc: ErrorLocation.HasPageId => handleCauses(loc.pageInstance, ExtractedCauses.fromCause(error))

  }

  sealed trait ErrorLocation
  object ErrorLocation {
    case object BrowserLoad extends ErrorLocation
    sealed abstract class HasPageId(final val pageInstance: PageInstance.Untyped) extends ErrorLocation

    final case class NavigationAttempt(prevPageInstance: PageInstance.Untyped) extends ErrorLocation.HasPageId(prevPageInstance)
    final case class NavigationPagePostLoad(currentPageInstance: PageInstance.Untyped) extends ErrorLocation.HasPageId(currentPageInstance)
    final case class PageEvent(currentPageInstance: PageInstance.Untyped) extends ErrorLocation.HasPageId(currentPageInstance)
  }

  final case class ErrorWithLocation(loc: ErrorLocation, error: Cause[UIError.NonRedirect])

  type RootError = ErrorWithLocation | UIError.Redirect

  def rootError[R, A](loc: => ErrorLocation)(effect: ZIO[R, UIError, A]): ZIO[R, RootError, A] =
    effect.catchAllTrace {
      case (nonRedirect: UIError.NonRedirect, trace) => ZIO.fail(ErrorWithLocation(loc, Cause.fail(nonRedirect, trace)))
      case (redirect: UIError.Redirect, trace)       => ZIO.refailCause(Cause.fail(redirect, trace))
    }

}
