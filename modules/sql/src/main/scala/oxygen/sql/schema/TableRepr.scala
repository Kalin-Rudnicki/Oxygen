package oxygen.sql.schema

import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.sql.generic.{DeriveProductRowRepr, DeriveTableRepr}
import scala.quoted.*

final case class TableRepr[A, K](
    schemaName: String,
    tableName: String,
    rowRepr: RowRepr.ProductRepr[A],
    pk: TableRepr.Partial[A, K],
    nonPK: TableRepr.Partial[A, ?],
) {
  type RowT = A
  type KeyT = K

  val ref: String = s"$schemaName.$tableName"

  def toIndentedString: IndentedString =
    rowRepr.toIndentedString

}
object TableRepr {

  inline def of[A](using ev: TableRepr[A, ?]): ev.type = ev

  final case class Partial[A, B](
      get: A => B,
      rowRepr: RowRepr[B],
  ) {

    def aEncoder: InputEncoder[A] =
      rowRepr.encoder.contramap(get)

  }

  private def derivedImpl[A: Type, K: Type](using quotes: Quotes): Expr[TableRepr[A, K]] = {
    given g: K0.ProductGeneric[A] =
      K0.ProductGeneric.of[A](K0.Derivable.Config())

    val expr: Expr[TableRepr[A, K]] =
      DeriveProductRowRepr.populateFields[A].defineAndUse {
        DeriveTableRepr[A, K](_).derive
      }

    // TODO (KR) : Support showing derivation
    /*
    g.annotations.optionalOf[K0.annotation.showDerivation[TableRepr[A, Any]]].foreach { ann =>
      report.info(expr.showAnsi, ann)
    }
     */

    expr
  }

  inline def derived[A, K] = ${ derivedImpl[A, K] }

}
