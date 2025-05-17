package oxygen.sql.schema

import oxygen.predef.core.*
import oxygen.predef.meta.*
import oxygen.sql.generic.{DeriveProductRowRepr, DeriveTableRepr}

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
    val k0: K0[quotes.type] = K0(Meta(quotes))
    import k0.meta.*

    val g: k0.ProductGeneric[A] =
      k0.ProductGeneric.of[A]

    val expr =
      DeriveProductRowRepr.populateFields[quotes.type, A, TableRepr[A, K]](k0)(g) { i =>
        DeriveTableRepr[quotes.type, A, K](k0)(g, i).makeTableSchema
      }

    g.optionalAnnotation[oxygen.meta.annotation.showDerivation].foreach { ann =>
      k0.meta.report.info(expr.showAnsi, ann)
    }

    expr
  }

  inline def derived[A, K] = ${ derivedImpl[A, K] }

}
