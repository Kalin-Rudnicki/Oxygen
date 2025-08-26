package oxygen.ui.web

import oxygen.http.core.RequestDecodingFailure
import oxygen.ui.web.internal.NavigationEvent

// TODO (KR) : add better error cases, and sensible `toString`
sealed trait UIError
object UIError {

  final case class Redirect(navEvent: NavigationEvent) extends UIError

  sealed trait NonRedirect extends UIError
  sealed trait Standard extends UIError.NonRedirect

  final case class ValidationError(message: String) extends UIError.Standard
  final case class ExternalError(message: String) extends UIError.Standard // TODO (KR) : include some sort of cause/internal message

  /////// Internal ///////////////////////////////////////////////////////////////

  // TODO (KR) : figure out what the scoping should be here...
  private[web] sealed trait Internal extends UIError.NonRedirect
  private[web] object Internal {
    final case class DirectPageNavigationToPageNotInRouter(page: RoutablePage[?]) extends Internal
    final case class UrlNotFound(url: PageURL) extends Internal
    final case class PageParamDecodingFailure(error: RequestDecodingFailure) extends Internal
  }
}
