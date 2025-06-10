package oxygen.meta2

import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Generic
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait Generic[A] {

  val label: String
  val sym: Symbol
  val typeRepr: TypeRepr
  val typeType: TypeType

  final given tpe: Type[A] = typeRepr.asTypeOf
  final given quotes: Quotes = sym.quotes

  final def annotations: Annotations = typeRepr.annotations

}
object Generic {

  // TODO (KR) :

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      ProductGeneric
//////////////////////////////////////////////////////////////////////////////////////////////////////

trait ProductGeneric[A] private extends Generic[A] { generic =>

  override val typeType: TypeType.Case

  val fields: Contiguous[Field[?]]

  def fieldsToInstance(fields: Contiguous[Expr[?]]): Expr[A]

  final case class Field[B](
      idx: Int,
      sym: Symbol,
      constructorSym: Symbol,
      typeRepr: TypeRepr,
      valDef: ValDef,
      get: Expr[A] => Expr[B],
  ) {

    def name: String = valDef.name

    given tpe: Type[B] = typeRepr.asTypeOf

    def summonTypeClass[TC[_]: Type]: Expr[TC[B]] =
      Implicits.search(typeRepr) match
        case ImplicitSearchSuccess(tree)        => tree.asExprOf
        case ImplicitSearchFailure(explanation) => report.errorAndAbort(s"Error summoning ${TypeRepr.of[TC[B]].show}\n\n$explanation")

    def annotations: AnnotationsTyped[B] = AnnotationsTyped(constructorSym.annotations.all, valDef.show)

    object util {

      // FIX-PRE-MERGE (KR) :

    }

  }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      SumGeneric
//////////////////////////////////////////////////////////////////////////////////////////////////////

trait SumGeneric[A] private extends Generic[A] { generic =>

  override val typeType: TypeType.Sealed

  val cases: Contiguous[Case[? <: A]]

  final case class Case[B <: A](
      idx: Int,
      productGeneric: ProductGeneric[B],
  )

  object util {

    // FIX-PRE-MERGE (KR) :

  }

}
