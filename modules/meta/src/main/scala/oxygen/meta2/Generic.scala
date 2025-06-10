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

  }

  object util {

    def map[Out](f: [b] => Type[b] ?=> Field[b] => Out): Contiguous[Out] =
      fields.map { _field =>
        type _b
        val field: Field[_b] = _field.asInstanceOf[Field[_b]]

        f[_b](using field.tpe)(field)
      }

    def mapExpr[Out](f: [b] => Type[b] ?=> Field[b] => Expr[Out]): Contiguous[Expr[Out]] =
      map[Expr[Out]](f)

    // FIX-PRE-MERGE (KR) :

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
  ) {

    given tpe: Type[B] = productGeneric.tpe
    given quotes: Quotes = productGeneric.quotes

    def label: String = productGeneric.label

    def annotations: Annotations = productGeneric.annotations

  }

  object util {

    def map[Out](f: [b <: A] => Type[b] ?=> Case[b] => Out): Contiguous[Out] =
      cases.map { _case =>
        type _b <: A
        val `case`: Case[_b] = _case.asInstanceOf[Case[_b]]

        f[_b](using `case`.tpe)(`case`)
      }

    def mapExpr[Out](f: [b <: A] => Type[b] ?=> Case[b] => Expr[Out]): Contiguous[Expr[Out]] =
      map[Expr[Out]](f)

    // FIX-PRE-MERGE (KR) :

  }

}
