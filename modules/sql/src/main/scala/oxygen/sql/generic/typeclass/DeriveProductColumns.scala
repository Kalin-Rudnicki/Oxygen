package oxygen.sql.generic.typeclass

import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.sql.*
import oxygen.sql.schema.*
import scala.quoted.*

final class DeriveProductColumns[A](
    instances: K0.Expressions[Columns, A],
)(using Quotes, Type[Columns], Type[A], K0.ProductGeneric[A])
    extends K0.Derivable.ProductDeriver[Columns, A] {

  private def makeColumnsInner: Growable[Expr[Contiguous[Column]]] =
    generic.mapChildren.mapExpr[Contiguous[Column]] { [i] => (_, _) ?=> (field: generic.Field[i]) =>
      import field.given

      '{ ${ field.getExpr(instances) }.columns }
    }

  private def makeColumnsExpr: Expr[Contiguous[Column]] =
    '{ ${ makeColumnsInner.toContiguous.seqToExpr }.flatten }

  override def derive: Expr[Columns[A]] =
    '{ Columns($makeColumnsExpr) }

}
