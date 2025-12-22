package oxygen.sql.generic.typeclass

import oxygen.meta.*
import oxygen.meta.k0.*
import oxygen.predef.core.*
import oxygen.sql.*
import oxygen.sql.schema.*
import scala.quoted.*

final class DeriveProductColumns[A](
    instances: Expressions[Columns, A],
)(using Quotes, Type[Columns], Type[A], ProductGeneric[A])
    extends Derivable.ProductDeriver[Columns, A] {

  private def makeColumnsInner: Growable[Expr[ArraySeq[Column]]] =
    generic.mapChildren.mapExpr[ArraySeq[Column]] { [i] => (_, _) ?=> (field: generic.Field[i]) =>
      '{ ${ field.getExpr(instances) }.columns }
    }

  private def makeColumnsExpr: Expr[ArraySeq[Column]] =
    '{ ${ makeColumnsInner.seqToArraySeqExpr }.flatten }

  override def derive: Expr[Columns[A]] =
    '{ Columns($makeColumnsExpr) }

}
