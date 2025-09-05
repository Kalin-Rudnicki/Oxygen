package oxygen.ui.web.internal

import oxygen.predef.core.*
import zio.internal.stacktracer.SourceLocation

sealed trait StyleSheetElement {

  def leafs(parent: StyleSheetSelector): Growable[StyleSheetElement.Leaf]

}
object StyleSheetElement {

  final case class Leaf(
      selector: StyleSheetSelector,
      loc: SourceLocation,
      key: String,
      value: String,
  ) {
    lazy val selectorString: String = selector.show
  }

  final case class AppliedStyleSheet(selector: StyleSheetSelector, loc: SourceLocation, elems: Seq[StyleSheetElement]) extends StyleSheetElement {

    def leafs: Growable[Leaf] =
      Growable.many(elems).flatMap(_.leafs(selector))

    override def leafs(parent: StyleSheetSelector): Growable[Leaf] =
      AppliedStyleSheet(parent >> selector, loc, elems).leafs

  }

  final case class CSS(key: String, value: String, loc: SourceLocation) extends StyleSheetElement {

    override def leafs(parent: StyleSheetSelector): Growable[Leaf] =
      Growable.single(Leaf(parent, loc, key, value))

  }

}
