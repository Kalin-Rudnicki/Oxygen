package oxygen.slyce.core.generic

import oxygen.meta.K0.*
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.slyce.core.*
import scala.quoted.*
import scala.reflect.ClassTag

private[generic] sealed trait ElementRepr {

  def typeRepr: TypeRepr

  final def subtypeOrThrow[A <: ElementRepr: ClassTag as ct] =
    this match {
      case ct(self) => self
      case _        =>
        given Quotes = typeRepr.quotes
        report.errorAndAbort(s"Expected ${ct.runtimeClass.getName}, but got ${this.getClass.getName}\n$this")
    }

  def showHeader: String
  def showBody: IndentedString

  final def toIndentedString: IndentedString = IndentedString.section(showHeader)(showBody)
  override final def toString: String = toIndentedString.toString

}
private[generic] object ElementRepr {

  sealed trait TokenOrNodeRepr extends ElementRepr {

    val gen: ProductOrSumGeneric[?]

    override final lazy val typeRepr: TypeRepr = gen.typeRepr

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Token
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait TokenRepr extends TokenOrNodeRepr

  final case class ProductTokenRepr(
      gen: ProductGeneric[?],
      regString: String,
      reg: ParsedRegex,
      builder: Expr[Token.Builder[?]],
  ) extends TokenRepr {

    override def showHeader: String = s"Token.Product(${typeRepr.showAnsiCode})"

    override def showBody: IndentedString =
      IndentedString.inline(
        s"regex-str: ${regString.unesc("`")}",
        s"regex: `$reg`",
        s"builder: ${builder.showAnsiCode(using gen.typeRepr.quotes)}",
      )

  }

  final case class SumTokenRepr(
      gen: SumGeneric[?],
      children: List[SumTokenRepr.Child],
  ) extends TokenRepr {

    override def showHeader: String = s"Token.Sum(${typeRepr.showAnsiCode})"

    override def showBody: IndentedString =
      children.map(_.toIndentedString)

  }
  object SumTokenRepr {

    final class Child(val parentGen: SumGeneric[?])(
        val kase: parentGen.Case[?],
        val child: ElementReprRef[TokenRepr],
    ) {

      def toIndentedString: IndentedString =
        IndentedString.section(s"${kase.name}:")(child.value.toIndentedString)

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Node
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait NodeRepr extends TokenOrNodeRepr

  final case class ProductNodeRepr(
      gen: ProductGeneric[?],
      children: List[ProductNodeRepr.Child],
  ) extends NodeRepr {

    override def showHeader: String = s"Node.Product(${typeRepr.showAnsiCode})"

    override def showBody: IndentedString =
      children.map(_.toIndentedString)

  }
  object ProductNodeRepr {

    final class Child(val parentGen: ProductGeneric[?])(
        val field: parentGen.Field[?],
        val child: ElementReprRef[ElementRepr],
    ) {

      def toIndentedString: IndentedString =
        IndentedString.section(s"${field.name}:")(child.value.showHeader)

    }

  }

  final case class SumNodeRepr(
      gen: SumGeneric[?],
      children: List[SumNodeRepr.Child],
  ) extends NodeRepr {

    override def showHeader: String = s"Node.Sum(${typeRepr.showAnsiCode})"

    override def showBody: IndentedString =
      children.map(_.toIndentedString)

  }
  object SumNodeRepr {

    final class Child(val parentGen: SumGeneric[?])(
        val kase: parentGen.Case[?],
        val child: ElementReprRef[NodeRepr],
    ) {

      def toIndentedString: IndentedString =
        IndentedString.section(s"${kase.name}:")(child.value.toIndentedString)

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Special
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait SpecialRepr extends ElementRepr

  sealed trait SingleTypeParam extends SpecialRepr {
    val inner: ElementReprRef[ElementRepr.TokenOrNodeRepr]
  }
  sealed trait OtherSpecial extends SpecialRepr

  final case class OptionRepr(typeRepr: TypeRepr, inner: ElementReprRef[ElementRepr.TokenOrNodeRepr]) extends SingleTypeParam {

    override def showHeader: String = s"Option(${inner.value.typeRepr.showAnsiCode})"

    override def showBody: IndentedString =
      inner.value.toIndentedString

  }

  // FIX-PRE-MERGE (KR) : Custom, uses node and then maps it
  // FIX-PRE-MERGE (KR) : - Either
  // FIX-PRE-MERGE (KR) : - Tuple
  // FIX-PRE-MERGE (KR) : - Unit
  // FIX-PRE-MERGE (KR) : Raw Nodes
  // FIX-PRE-MERGE (KR) : - OptionNode
  // FIX-PRE-MERGE (KR) : - EitherNode
  // FIX-PRE-MERGE (KR) : - ListNode
  // FIX-PRE-MERGE (KR) : - NonEmptyListNode
  // FIX-PRE-MERGE (KR) : - TupleNode?   - need to create 22 of these..?
  // FIX-PRE-MERGE (KR) : - UnitNode
  // FIX-PRE-MERGE (KR) :
  // FIX-PRE-MERGE (KR) : Expressions with associativity and hierarchy

  //

  // FIX-PRE-MERGE (KR) : don't limit these to `TokenOrNodeRepr` - allow SpecialRepr too
  //                    : in a case of something like `Option[List[MyToken]]`, there is ambiguity and the algorithm wont know which one to pick ( None or Some(Nil) )
  //                    : but, this is handled by the algorithm, so this repr should allow all of it, and let the algorithm decide what it can actually handle.
  //                    : otherwise, it stops you from doing things like `Option[NonEmptyList[MyToken]]`, which is perfectly valid, or `Option[(A, B, C)]` or `List[(A, B, C)]`

  final case class ListRepr(typeRepr: TypeRepr, inner: ElementReprRef[ElementRepr.TokenOrNodeRepr]) extends SingleTypeParam {

    override def showHeader: String = s"List(${inner.value.typeRepr.showAnsiCode})"

    override def showBody: IndentedString =
      inner.value.toIndentedString

  }

  final case class NonEmptyListRepr(typeRepr: TypeRepr, inner: ElementReprRef[ElementRepr.TokenOrNodeRepr]) extends SingleTypeParam {

    override def showHeader: String = s"NonEmptyList(${inner.value.typeRepr.showAnsiCode})"

    override def showBody: IndentedString =
      inner.value.toIndentedString

  }

  final case class UnionRepr(typeRepr: TypeRepr, inner: NonEmptyList[ElementReprRef[ElementRepr.TokenOrNodeRepr]]) extends OtherSpecial {

    override def showHeader: String = s"Union( ${inner.map(_.value.typeRepr.showAnsiCode).mkString(" | ")} )"

    override def showBody: IndentedString =
      inner.toList.map(_.value.toIndentedString)

  }

}
