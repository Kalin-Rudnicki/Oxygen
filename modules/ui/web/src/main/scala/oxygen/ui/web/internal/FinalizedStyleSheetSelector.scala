package oxygen.ui.web.internal

import oxygen.predef.core.*

object FinalizedStyleSheetSelector {

  object Single {

    sealed trait Referencable {

      def &(that: Single.NonReferencable): Single.Referencable

      def show: String

    }

    sealed trait NonReferencable {
      val classes: List[String]
      val pseudoClasses: List[String]
      def &(that: Single.NonReferencable): Single.NonReferencable

    }

    final case class TagRoot(tag: String, classes: List[String], pseudoClasses: List[String]) extends Single.Referencable {

      override def &(that: Single.NonReferencable): Single.Referencable = Single.TagRoot(tag, this.classes ++ that.classes, this.pseudoClasses ++ that.pseudoClasses)

      override def show: String = s"$tag${classes.sorted.map(c => s".$c").mkString}${pseudoClasses.sorted.map(c => s":$c").mkString}"

    }

    final case class ClassRoot(nelClasses: NonEmptyList[String], pseudoClasses: List[String]) extends Single.Referencable, Single.NonReferencable {
      override val classes: List[String] = nelClasses.toList
      override def &(that: Single.NonReferencable): Single.ClassRoot = Single.ClassRoot(this.nelClasses ++ that.classes, this.pseudoClasses ++ that.pseudoClasses)

      override def show: String = s"${classes.sorted.map(c => s".$c").mkString}${pseudoClasses.sorted.map(c => s":$c").mkString}"

    }

    final case class PseudoClassOnly(nelPseudoClasses: NonEmptyList[String]) extends Single.NonReferencable {
      override val classes: List[String] = Nil
      override val pseudoClasses: List[String] = nelPseudoClasses.toList
      override def &(that: Single.NonReferencable): Single.NonReferencable = that match
        case that: Single.ClassRoot       => Single.ClassRoot(that.nelClasses ++ this.classes, that.pseudoClasses ++ this.pseudoClasses)
        case that: Single.PseudoClassOnly => Single.PseudoClassOnly(this.nelPseudoClasses ++ that.nelPseudoClasses)

    }

  }

  object Many {

    sealed trait Referencable {

      def options: NonEmptyList[Single.Referencable]

      def &(that: Many.NonReferencable): Many.Referencable

      final def |(that: Many.Referencable): Many.Referencable =
        (this, that) match
          case (a: Many.AllClasses, b: Many.AllClasses) => Many.AllClasses(a.options ++ b.options)
          case (a, b)                                   => Many.HasTag(a.options ++ b.options)

      final def showAll: NonEmptyList[String] = options.map(_.show)

    }

    sealed trait NonReferencable {

      def options: NonEmptyList[Single.NonReferencable]

      def &(that: Many.NonReferencable): Many.NonReferencable

      final def |(that: Many.NonReferencable): Many.NonReferencable =
        (this, that) match
          case (a: Many.AllClasses, b: Many.AllClasses) => Many.AllClasses(a.options ++ b.options)
          case (a, b)                                   => Many.HasPseudoClass(a.options ++ b.options)

    }

    final case class HasTag(options: NonEmptyList[Single.Referencable]) extends Many.Referencable {

      override def &(that: Many.NonReferencable): Many.HasTag =
        HasTag {
          for {
            a <- this.options
            b <- that.options
          } yield a & b
        }

    }

    final case class AllClasses(options: NonEmptyList[Single.ClassRoot]) extends Many.Referencable, Many.NonReferencable {

      override def &(that: Many.NonReferencable): Many.AllClasses =
        AllClasses {
          for {
            a <- this.options
            b <- that.options
          } yield a & b
        }

      def |(that: Many.AllClasses): Many.AllClasses =
        Many.AllClasses(this.options ++ that.options)

    }

    final case class HasPseudoClass(options: NonEmptyList[Single.NonReferencable]) extends Many.NonReferencable {

      override def &(that: Many.NonReferencable): Many.NonReferencable = {
        val res: NonEmptyList[Single.NonReferencable] =
          for {
            a <- this.options
            b <- that.options
          } yield a & b

        res.traverse {
          case single: Single.ClassRoot  => single.some
          case _: Single.PseudoClassOnly => None
        } match {
          case Some(classRoots) => Many.AllClasses(classRoots)
          case None             => Many.HasPseudoClass(res)
        }
      }

    }

  }

  sealed trait ManyMany {

    final def showAll: NonEmptyList[String] =
      this match {
        case ManyMany.Root(many) =>
          many.showAll
        case ManyMany.Join(a, join, b) =>
          for {
            a <- a.showAll
            b <- b.showAll
          } yield s"$a$join$b"
        case ManyMany.Or(a, b) =>
          a.showAll ++ b.showAll
      }

    final def show: String = showAll.mkString(", ")

  }
  object ManyMany {

    final case class Root(many: Many.Referencable) extends ManyMany
    final case class Join(a: ManyMany, join: String, b: ManyMany) extends ManyMany
    final case class Or(a: ManyMany, b: ManyMany) extends ManyMany

  }

  def selector(v: StyleSheetSelector): ManyMany =
    v match {
      case root: StyleSheetSelector.SingleRoot           => ManyMany.Root(singleRoot(root))
      case StyleSheetSelector.DirectChild(parent, child) => ManyMany.Join(selector(parent), " > ", selector(child))
      case StyleSheetSelector.Child(parent, child)       => ManyMany.Join(selector(parent), " ", selector(child))
      case StyleSheetSelector.Or(a, b)                   => ManyMany.Or(selector(a), selector(b))
    }

  def singleRoot(v: StyleSheetSelector.SingleRoot): Many.Referencable =
    v match {
      case v: StyleSheetSelector.RootWithoutTag      => rootWithoutTag(v)
      case StyleSheetSelector.RootWithTag.Tag(tag)   => Many.HasTag(NonEmptyList.one(Single.TagRoot(tag, Nil, Nil)))
      case StyleSheetSelector.RootWithTag.And1(a, b) => singleRoot(a) & rootWithoutTag(b)
      case StyleSheetSelector.RootWithTag.And2(a, b) => singleRoot(a) & nonRoot(b)
      case StyleSheetSelector.RootWithTag.Or1(a, b)  => singleRoot(a) | singleRoot(b)
      case StyleSheetSelector.RootWithTag.Or2(a, b)  => singleRoot(a) | rootWithoutTag(b)
    }

  def rootWithoutTag(v: StyleSheetSelector.RootWithoutTag): Many.AllClasses =
    v match {
      case StyleSheetSelector.RootWithoutTag.ClassName(className) => Many.AllClasses(NonEmptyList.one(Single.ClassRoot(NonEmptyList.one(className), Nil)))
      case StyleSheetSelector.RootWithoutTag.And1(a, b)           => rootWithoutTag(a) & rootWithoutTag(b)
      case StyleSheetSelector.RootWithoutTag.And2(a, b)           => rootWithoutTag(a) & nonRoot(b)
      case StyleSheetSelector.RootWithoutTag.Or1(a, b)            => rootWithoutTag(a) | rootWithoutTag(b)
    }

  def nonRoot(v: StyleSheetSelector.NonRoot): Many.NonReferencable =
    v match {
      case StyleSheetSelector.NonRoot.PseudoClassName(pseudoClassName) => Many.HasPseudoClass(NonEmptyList.one(Single.PseudoClassOnly(NonEmptyList.one(pseudoClassName))))
      case StyleSheetSelector.NonRoot.Has(has)                         => Many.HasPseudoClass(NonEmptyList.one(Single.PseudoClassOnly(NonEmptyList.one(s"has(${selector(has).show})"))))
      case StyleSheetSelector.NonRoot.And1(a, b)                       => nonRoot(a) & nonRoot(b)
      case StyleSheetSelector.NonRoot.Or1(a, b)                        => rootWithoutTag(a) | nonRoot(b)
      case StyleSheetSelector.NonRoot.Or2(a, b)                        => nonRoot(a) | nonRoot(b)
    }

}
