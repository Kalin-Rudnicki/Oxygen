package oxygen.ui.web.internal

import oxygen.predef.core.*
import oxygen.ui.web.create.MediaQuery
import zio.internal.stacktracer.SourceLocation

sealed trait StyleSheetElement {

  def leafs(parent: StyleSheetSelector, media: Option[MediaQuery]): Growable[StyleSheetElement.Leaf]

}
object StyleSheetElement {

  /** Combine an outer media query with an inner one (nested `media` blocks AND together). */
  private[web] def combineMedia(outer: Option[MediaQuery], inner: Option[MediaQuery]): Option[MediaQuery] =
    (outer, inner) match {
      case (Some(o), Some(i)) => Some(o && i)
      case (Some(o), None)    => Some(o)
      case (None, i)          => i
    }

  final case class Leaf(
      selector: StyleSheetSelector,
      loc: SourceLocation,
      key: String,
      value: String,
      media: Option[MediaQuery],
  ) {
    lazy val selectorString: String = selector.show
    def mediaQueryString: Option[String] = media.map(_.query)
  }

  final case class AppliedStyleSheet(
      selector: StyleSheetSelector,
      loc: SourceLocation,
      elems: Seq[StyleSheetElement],
      media: Option[MediaQuery] = None,
  ) extends StyleSheetElement {

    /** Wrap this applied sheet (and everything it contains) in an additional (outer) media query. */
    def withMedia(mq: MediaQuery): AppliedStyleSheet =
      copy(media = Some(media.fold(mq)(mq && _)))

    def leafs: Growable[Leaf] =
      Growable.many(elems).flatMap(_.leafs(selector, media))

    override def leafs(parent: StyleSheetSelector, parentMedia: Option[MediaQuery]): Growable[Leaf] =
      AppliedStyleSheet(parent >> selector, loc, elems, combineMedia(parentMedia, media)).leafs

  }

  final case class CSS(key: String, value: Lazy[String], loc: SourceLocation) extends StyleSheetElement {

    override def leafs(parent: StyleSheetSelector, media: Option[MediaQuery]): Growable[Leaf] =
      Growable.single(Leaf(parent, loc, key, value.value, media))

  }

}
