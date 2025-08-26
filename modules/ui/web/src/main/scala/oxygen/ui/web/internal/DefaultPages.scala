package oxygen.ui.web.internal

import oxygen.ui.web.{NonRoutablePage, PageURL, UIError}
import oxygen.ui.web.defaults.*
import zio.Cause

final case class DefaultPages[-Env](
    initial: NonRoutablePage.AuxEP[Env, Unit],
    error: NonRoutablePage.AuxEP[Env, Cause[UIError.NonRedirect]],
    notFound: NonRoutablePage.AuxEP[Env, PageURL],
)
object DefaultPages {

  val default: DefaultPages[Any] =
    DefaultPages(
      initial = DefaultInitialPage,
      error = DefaultErrorPage,
      notFound = DefaultNotFoundPage,
    )

}
