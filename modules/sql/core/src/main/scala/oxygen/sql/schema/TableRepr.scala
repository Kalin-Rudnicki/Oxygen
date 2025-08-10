package oxygen.sql.schema

import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.typeclass.*
import scala.quoted.*

trait TableRepr[A] {

  final type RowT = A
  type PrimaryKeyT
  type NonPrimaryKeyT

  val schemaName: String
  val tableName: String
  val rowRepr: RowRepr.ProductRepr[A]
  val pk: TableRepr.Partial[A, PrimaryKeyT]
  val npk: TableRepr.Partial[A, NonPrimaryKeyT]

  val ref: String

  final def toIndentedString: IndentedString =
    rowRepr.toIndentedString

}
object TableRepr {

  inline def of[A](using ev: TableRepr[A]): ev.type = ev

  trait Typed[A, PK, NPK] extends TableRepr[A] {
    override final type PrimaryKeyT = PK
    override final type NonPrimaryKeyT = NPK
  }

  final case class TypedImpl[A, PK, NPK](
      schemaName: String,
      tableName: String,
      rowRepr: RowRepr.ProductRepr[A],
      pk: TableRepr.Partial[A, PK],
      npk: TableRepr.Partial[A, NPK],
  ) extends Typed[A, PK, NPK] {

    override val ref: String = s"$schemaName.$tableName"

  }

  type AuxPK[A, PK] = TableRepr[A] { type PrimaryKeyT = PK }
  type Aux[A, PK, NPK] = TableRepr[A] { type PrimaryKeyT = PK; type NonPrimaryKeyT = NPK }

  final case class Partial[A, B](
      _get: A => B,
      rowRepr: RowRepr[B],
  ) {

    def get(a: A): B = _get(a)

    def aEncoder: InputEncoder[A] =
      rowRepr.encoder.contramap(_get)

  }

  private def derivedImpl[A: Type](using quotes: Quotes): Expr[TableRepr.Typed[A, ?, ?]] = {
    given g: K0.ProductGeneric[A] =
      K0.ProductGeneric.of[A](K0.Derivable.Config())

    val expr: Expr[TableRepr.Typed[A, ?, ?]] =
      DeriveProductRowRepr.populateFields[A].defineAndUse {
        DeriveTableRepr[A](_).derive
      }

    MacroUtil.macroShowExprWhen(expr, g.annotations.optionalOf[K0.annotation.showDerivation[TableRepr]])
  }

  transparent inline def derived[A]: TableRepr.Typed[A, ?, ?] = ${ derivedImpl[A] }

}
