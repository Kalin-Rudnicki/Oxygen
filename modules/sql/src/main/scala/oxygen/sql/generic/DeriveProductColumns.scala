package oxygen.sql.generic

import oxygen.predef.core.*
import oxygen.predef.meta.*
import oxygen.sql.*
import oxygen.sql.schema.*

@scala.annotation.nowarn("msg=unused import")
final class DeriveProductColumns[Q <: Quotes, A](val k0: K0[Q])(generic: k0.ProductGeneric[A], instances: k0.ValExpressions[Columns]) {
  import generic.given
  import k0.given

  private def makeColumnsInner: Seq[Expr[Contiguous[Column]]] =
    generic.builders.mapToSeq {
      [i] =>
        (field: generic.Field[i]) =>
          import field.given

          '{ ${ field.getExpr(instances) }.columns }
    }

  private def makeColumnsExpr: Expr[Contiguous[Column]] =
    '{ ${ makeColumnsInner.toContiguous.flattenExprs }.flatten }

  def makeColumns: Expr[Columns[A]] =
    '{ Columns($makeColumnsExpr) }

}
