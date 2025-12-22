package oxygen.meta.k0

import oxygen.core.*
import oxygen.core.syntax.common.*
import oxygen.quoted.*
import scala.quoted.*

trait Entity[SelfBound, A <: SelfBound] private[k0] () {

  type SelfType[A2 <: SelfBound] <: Entity[SelfBound, A2]

  val label: String
  final def name: String = label

  val sym: Symbol
  val typeRepr: TypeRepr

  final given tpe: Type[A] = typeRepr.asTypeOf[A]

  def pos: Position

  def annotations(using Quotes): AnnotationsTyped[A]

  /**
    * Meant to be used in the following manner:
    *
    * Entity in this entity could be any of Generic, ProductGeneric, SumGeneric, Field, Case
    * ```scala
    * val untyped: Entity[?] = ???
    *
    * type A
    * val typed: Entity[A] = untyped.typedAs[A]
    * ```
    *
    * Entity in this case could be any of Generic, ProductGeneric, SumGeneric, Field, Case
    */
  final def typedAs[TypeName <: SelfBound]: SelfType[TypeName] = this.asInstanceOf[SelfType[TypeName]]

  def summonTypeClass[TC[_]: Type](using quotes: Quotes): Expr[TC[A]] =
    Implicits.search(TypeRepr.of[TC[A]]) match {
      case ImplicitSearchSuccess(tree)        => tree.asExprOf[TC[A]]
      case ImplicitSearchFailure(explanation) =>
        val tcBase = TypeRepr.of[TC]
        val tcTpe = tcBase.narrowOpt[LambdaType].fold(tcBase)(_.resType)

        report.errorAndAbort(
          s"""Error summoning typeclass: ${TypeRepr.of[TC[A]].showAnsiCode}
               |  type:      ${typeRepr.showAnsiCode}
               |  typeclass: ${tcTpe.showAnsiCode}
               |
               |  explanation (note that the compiler often gives somewhat useless explanations here, it seems to not be passing along some additional context):
               |    ${explanation.replaceAll("\n", "\n    ")}""".stripMargin,
          pos,
        )
    }

  final def summonTypeClassOrDerive[TC[_]: Type](f: => Type[A] ?=> Expr[TC[A]])(using quotes: Quotes): Expr[TC[A]] =
    Implicits.search(TypeRepr.of[TC[A]]) match
      case ImplicitSearchSuccess(tree) => tree.asExprOf[TC[A]]
      case ImplicitSearchFailure(_)    => f(using tpe)

  def toIndentedString: IndentedString
  override final def toString: String = toIndentedString.toString

}
object Entity {

  trait Child[ParentBound, B <: ParentBound, A] private[k0] () extends Entity[ParentBound, B] {

    override type SelfType[A2 <: ParentBound] <: Child[ParentBound, A2, A]

    val idx: Int
    val childType: String

    def parentGeneric: Generic[A]

    override def summonTypeClass[TC[_]: Type](using quotes: Quotes): Expr[TC[B]] =
      Implicits.search(TypeRepr.of[TC[B]]) match {
        case ImplicitSearchSuccess(tree)        => tree.asExprOf[TC[B]]
        case ImplicitSearchFailure(explanation) =>
          val tcBase = TypeRepr.of[TC]
          val tcTpe = tcBase.narrowOpt[LambdaType].fold(tcBase)(_.resType)

          report.errorAndAbort(
            s"""Error summoning typeclass: ${TypeRepr.of[TC[B]].showAnsiCode}
                 |  ${(childType + ":").alignLeft(12)} $name
                 |  ${(childType + "-type:").alignLeft(12)} ${typeRepr.showAnsiCode}
                 |  typeclass:   ${tcTpe.showAnsiCode}
                 |  parent-type: ${parentGeneric.typeRepr.showAnsiCode}
                 |
                 |  explanation (note that the compiler often gives somewhat useless explanations here, it seems to not be passing along some additional context):
                 |    ${explanation.replaceAll("\n", "\n    ")}""".stripMargin,
            pos,
          )
      }

    final def getExpr[F[_]](expressions: Expressions[F, A])(using Quotes): Expr[F[B]] =
      expressions.at[B](idx)

  }
  object Child {

    abstract class Deferred[ParentBound, B <: ParentBound, A](entity: Entity[Any, B]) extends Child[ParentBound, B, A] {
      override final val label: String = entity.label
      override final val sym: Symbol = entity.sym
      override final val typeRepr: TypeRepr = entity.typeRepr
      override final def pos: Position = entity.pos
      override final def annotations(using Quotes): AnnotationsTyped[B] = entity.annotations
      override final def toIndentedString: IndentedString = entity.toIndentedString
    }

  }

}
