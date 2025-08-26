package oxygen.ui.web.internal

import oxygen.ui.web.{NonRoutablePage, Page, PageURL, RoutablePage}

final case class NavigationEvent private (
    target: NavigationEvent.Target,
    navType: NavigationEvent.NavType,
    // TODO (KR) : page messages upon loading
    // TODO (KR) : does it make sense be able to have a sort of "message ids that should remain on the new page?"
    //           : this way when you know you are going to navigate you could send these ids, the messages will be displayed BEFORE page transitions,
    //           : and then will all be available after the page switches.
)
object NavigationEvent {

  /////// Builders ///////////////////////////////////////////////////////////////

  def push(url: PageURL): NavigationEvent = NavigationEvent(Target.Url(url), NavType.Push)
  def replace(url: PageURL): NavigationEvent = NavigationEvent(Target.Url(url), NavType.Replace)

  private[web] def browserLoad(url: PageURL): NavigationEvent = NavigationEvent(Target.Url(url), NavType.None)
  private[web] def pushPage(page: RoutablePage[?])(params: page.Params): NavigationEvent = NavigationEvent(Target.RoutablePageWithParams(page)(params), NavType.Push)
  private[web] def replacePage(page: RoutablePage[?])(params: page.Params): NavigationEvent = NavigationEvent(Target.RoutablePageWithParams(page)(params), NavType.Replace)
  private[web] def renderPage(page: NonRoutablePage[?])(params: page.Params): NavigationEvent = NavigationEvent(Target.NonRoutablePageWithParams(page)(params), NavType.None)

  /////// Types ///////////////////////////////////////////////////////////////

  sealed trait Target
  object Target {
    final case class Url(url: PageURL) extends Target

    // format: off
    sealed abstract class PageWithParams[Env](final val page: Page[Env])(final val params: page.Params) extends Target
    final class RoutablePageWithParams[Env](val routablePage: RoutablePage[Env])(val routableParams: routablePage.Params) extends Target.PageWithParams[Env](routablePage)(routableParams)
    final class NonRoutablePageWithParams[Env](val nonRoutablePage: NonRoutablePage[Env])(val nonRoutableParams: nonRoutablePage.Params) extends Target.PageWithParams[Env](nonRoutablePage)(nonRoutableParams)
    // format: on
  }

  enum NavType { case Push, Replace, None }

}
