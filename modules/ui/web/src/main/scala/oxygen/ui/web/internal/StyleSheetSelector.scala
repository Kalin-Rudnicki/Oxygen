package oxygen.ui.web.internal

import oxygen.ui.web.create.StyleSheetBuilder
import zio.internal.stacktracer.SourceLocation

sealed trait StyleSheetSelector {

  final def |(that: StyleSheetSelector): StyleSheetSelector = StyleSheetSelector.Or(this, that)
  final def >(that: StyleSheetSelector): StyleSheetSelector = StyleSheetSelector.DirectChild(this, that)
  final def directChild(that: StyleSheetSelector): StyleSheetSelector = StyleSheetSelector.DirectChild(this, that)
  final def >>(that: StyleSheetSelector): StyleSheetSelector = StyleSheetSelector.Child(this, that)
  final def child(that: StyleSheetSelector): StyleSheetSelector = StyleSheetSelector.Child(this, that)

  final def apply(using
      loc: SourceLocation,
      add: StyleSheetBuilder.MutableAdder,
  )(
      elems: (StyleSheetBuilder.MutableAdder ?=> StyleSheetElement)*,
  ): StyleSheetElement.AppliedStyleSheet = {
    val res = StyleSheetElement.AppliedStyleSheet(this, loc, elems.map(_(using StyleSheetBuilder.MutableAdder.noop)))
    add.add(res)
    res
  }

  final def show: String = FinalizedStyleSheetSelector.selector(this).show

}
object StyleSheetSelector {

  def tag(tag: String): StyleSheetSelector.RootWithTag = StyleSheetSelector.RootWithTag.Tag(tag)
  def `class`(className: String): StyleSheetSelector.RootWithoutTag = StyleSheetSelector.RootWithoutTag.ClassName(className)
  def pseudoClass(pseudoClassName: String): StyleSheetSelector.NonRoot = StyleSheetSelector.NonRoot.PseudoClassName(pseudoClassName)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Tree
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class DirectChild(parent: StyleSheetSelector, child: StyleSheetSelector) extends StyleSheetSelector
  final case class Child(parent: StyleSheetSelector, child: StyleSheetSelector) extends StyleSheetSelector
  final case class Or(a: StyleSheetSelector, b: StyleSheetSelector) extends StyleSheetSelector

  sealed trait SingleRoot extends StyleSheetSelector

  sealed trait RootWithTag extends SingleRoot {

    final def &(that: RootWithoutTag): RootWithTag = RootWithTag.And1(this, that)
    final def &(that: NonRoot): RootWithTag = RootWithTag.And2(this, that)
    final def |(that: RootWithTag): RootWithTag = RootWithTag.Or1(this, that)
    final def |(that: RootWithoutTag): RootWithTag = RootWithTag.Or2(this, that)

    final def c(className: String): RootWithTag = this & `class`(className)
    final def pc(pseudoClassName: String): RootWithTag = this & pseudoClass(pseudoClassName)

    final def hover: RootWithTag = this.pc("hover")
    final def active: RootWithTag = this.pc("active")
    final def invalid: RootWithTag = this.pc("invalid")
    final def has(sel: StyleSheetSelector): RootWithTag = this & NonRoot.Has(sel)

  }
  object RootWithTag {

    final case class Tag(tag: String) extends RootWithTag
    final case class And1(a: RootWithTag, b: RootWithoutTag) extends RootWithTag
    final case class And2(a: RootWithTag, b: NonRoot) extends RootWithTag
    final case class Or1(a: RootWithTag, b: RootWithTag) extends RootWithTag
    final case class Or2(a: RootWithTag, b: RootWithoutTag) extends RootWithTag

  }

  sealed trait RootWithoutTag extends SingleRoot {

    final def &(that: RootWithTag): RootWithTag = RootWithTag.And1(that, this)
    final def &(that: RootWithoutTag): RootWithoutTag = RootWithoutTag.And1(this, that)
    final def &(that: NonRoot): RootWithoutTag = RootWithoutTag.And2(this, that)
    final def |(that: RootWithTag): RootWithTag = RootWithTag.Or2(that, this)
    final def |(that: RootWithoutTag): RootWithoutTag = RootWithoutTag.Or1(this, that)
    final def |(that: NonRoot): NonRoot = NonRoot.Or1(this, that)

    final def c(className: String): RootWithoutTag = this & `class`(className)
    final def pc(pseudoClassName: String): RootWithoutTag = this & pseudoClass(pseudoClassName)

    final def hover: RootWithoutTag = this.pc("hover")
    final def active: RootWithoutTag = this.pc("active")
    final def invalid: RootWithoutTag = this.pc("invalid")
    final def has(sel: StyleSheetSelector): RootWithoutTag = this & NonRoot.Has(sel)

  }
  object RootWithoutTag {

    final case class ClassName(className: String) extends RootWithoutTag
    final case class And1(a: RootWithoutTag, b: RootWithoutTag) extends RootWithoutTag
    final case class And2(a: RootWithoutTag, b: NonRoot) extends RootWithoutTag
    final case class Or1(a: RootWithoutTag, b: RootWithoutTag) extends RootWithoutTag

  }

  sealed trait NonRoot {

    final def &(that: RootWithoutTag): RootWithoutTag = RootWithoutTag.And2(that, this)
    final def &(that: NonRoot): NonRoot = NonRoot.And1(this, that)
    final def |(that: RootWithoutTag): NonRoot = NonRoot.Or1(that, this)
    final def |(that: NonRoot): NonRoot = NonRoot.Or2(this, that)

  }
  object NonRoot {

    final case class PseudoClassName(pseudoClassName: String) extends NonRoot
    final case class Has(selector: StyleSheetSelector) extends NonRoot // TODO (KR) : remove? has(selector.build.toString)
    final case class And1(a: NonRoot, b: NonRoot) extends NonRoot
    final case class Or1(a: RootWithoutTag, b: NonRoot) extends NonRoot
    final case class Or2(a: NonRoot, b: NonRoot) extends NonRoot

  }

}
