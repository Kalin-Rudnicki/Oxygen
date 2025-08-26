package oxygen.ui.web.internal

import oxygen.ui.web.{PageMessage, PageMessages, UIError}
import zio.*

trait RootErrorHandler {

  def handleError(errorLocation: RootErrorHandler.ErrorLocation, error: UIError.NonRedirect): UIO[Unit]
  def handleDefect(errorLocation: RootErrorHandler.ErrorLocation, error: Throwable): UIO[Unit]

  // TODO (KR) : might be worth inverting this, and having this be what is implemented, and the others are helpers
  final def handleErrorCause(errorLocation: RootErrorHandler.ErrorLocation, error: Cause[UIError.NonRedirect]): UIO[Unit] =
    ZIO.logDebugCause(s"Handling error in $errorLocation", error) *>
      (error.failures match {
        case Nil =>
          error.defects match {
            case Nil =>
              if (error.isInterruptedOnly) ZIO.logDebugCause(s"Interrupted: $errorLocation", error)
              else ZIO.logErrorCause(s"Unhandled error in $errorLocation", error)
            case defects =>
              ZIO.foreachDiscard(defects)(handleDefect(errorLocation, _))
          }
        case failures =>
          ZIO.foreachDiscard(failures)(handleError(errorLocation, _))
      })

}
object RootErrorHandler {

  final case class Default[Env](defaultPages: DefaultPages[Env]) extends RootErrorHandler {

    // TODO (KR) : 404 handling
    override def handleError(errorLocation: ErrorLocation, error: UIError.NonRedirect): UIO[Unit] =
      errorLocation match {
        case ErrorLocation.BrowserLoad    => Router.route(NavigationEvent.renderPage(defaultPages.error)(Cause.fail(error)))
        case loc: ErrorLocation.HasPageId => PageMessages.PageLocal.update(loc.pageReference)(_ :+ PageMessage.error(error.toString))
      }

    override def handleDefect(errorLocation: ErrorLocation, error: Throwable): UIO[Unit] =
      errorLocation match {
        case ErrorLocation.BrowserLoad    => Router.route(NavigationEvent.renderPage(defaultPages.error)(Cause.die(error)))
        case loc: ErrorLocation.HasPageId => PageMessages.PageLocal.update(loc.pageReference)(_ :+ PageMessage.error(error.toString))
      }

  }

  sealed trait ErrorLocation
  object ErrorLocation {
    case object BrowserLoad extends ErrorLocation
    sealed abstract class HasPageId(final val pageReference: PageReference) extends ErrorLocation

    final case class NavigationAttempt(prevPageReference: PageReference) extends ErrorLocation.HasPageId(prevPageReference)
    final case class NavigationPagePostLoad(currentPageReference: PageReference) extends ErrorLocation.HasPageId(currentPageReference)
    final case class PageEvent(currentPageReference: PageReference) extends ErrorLocation.HasPageId(currentPageReference)
  }

  final case class ErrorWithLocation(loc: ErrorLocation, error: Cause[UIError.NonRedirect])

  type RootError = ErrorWithLocation | UIError.Redirect

  def rootError[R, A](loc: => ErrorLocation)(effect: ZIO[R, UIError, A]): ZIO[R, RootError, A] =
    effect.catchAllTrace {
      case (nonRedirect: UIError.NonRedirect, trace) => ZIO.fail(ErrorWithLocation(loc, Cause.fail(nonRedirect, trace)))
      case (redirect: UIError.Redirect, trace)       => ZIO.refailCause(Cause.fail(redirect, trace))
    }

}
