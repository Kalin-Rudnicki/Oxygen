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
      PageMessages.PageLocal.toState(using pageInstance).update(_ :++ errors.toList)

    private def handleCauses(pageInstance: PageInstance.Untyped, causes: ExtractedCauses[UIError.NonRedirect]): UIO[Unit] =
      causes match {
        case ExtractedCauses.Failures(failures, _, _) =>
          failures.collectFirst { case Cause.Fail(UIError.Internal.UrlNotFound(url), _) => url } match {
            case Some(url) => Router.route(NavigationEvent.renderPage(defaultPages.notFound)(url))
            case None      => showErrors(pageInstance, failures.flatMap(_.value.toPageMessages))
          }
        case ExtractedCauses.Defects(defects, _) => showErrors(pageInstance, defects.flatMap(e => UIError.ClientSide.InternalDefect.somethingWentWrong(e.value).toPageMessages))
        case _                                   => showErrors(pageInstance, UIError.ClientSide.InternalDefect.somethingWentWrong("ExtractedCauses.Other").toPageMessages)
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

  private val folder: Cause.Folder[ErrorLocation, UIError, RootError] =
    new Cause.Folder[ErrorLocation, UIError, RootError] {

      override def empty(context: ErrorLocation): RootError =
        ErrorWithLocation(context, Cause.empty)

      override def failCase(context: ErrorLocation, error: UIError, stackTrace: StackTrace): RootError =
        error match {
          case nonRedirect: UIError.NonRedirect => ErrorWithLocation(context, Cause.fail(nonRedirect, stackTrace))
          case redirect: UIError.Redirect       => redirect
        }

      override def dieCase(context: ErrorLocation, t: Throwable, stackTrace: StackTrace): RootError =
        ErrorWithLocation(context, Cause.die(t, stackTrace))

      override def interruptCase(context: ErrorLocation, fiberId: FiberId, stackTrace: StackTrace): RootError =
        ErrorWithLocation(context, Cause.interrupt(fiberId, stackTrace))

      override def bothCase(context: ErrorLocation, left: RootError, right: RootError): RootError =
        (left, right) match
          case (left: ErrorWithLocation, right: ErrorWithLocation) => ErrorWithLocation(context, Cause.Both(left.error, right.error))
          case (left: UIError.Redirect, _)                         => left
          case (_: ErrorWithLocation, right: UIError.Redirect)     => right

      override def thenCase(context: ErrorLocation, left: RootError, right: RootError): RootError =
        (left, right) match
          case (left: ErrorWithLocation, right: ErrorWithLocation) => ErrorWithLocation(context, Cause.Then(left.error, right.error))
          case (left: UIError.Redirect, _)                         => left
          case (_: ErrorWithLocation, right: UIError.Redirect)     => right

      override def stacklessCase(context: ErrorLocation, value: RootError, stackless: Boolean): RootError =
        value match
          case value: UIError.Redirect  => value
          case value: ErrorWithLocation => ErrorWithLocation(value.loc, Cause.Stackless(value.error, stackless))

    }

  def rootError[R, A](loc: => ErrorLocation)(effect: ZIO[R, UIError, A]): ZIO[R, RootError, A] =
    effect.catchAllCause { cause => ZIO.fail(cause.foldContext(loc)(folder)) }

}
