package oxygen.sql.generic.typeclass

import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.*
import oxygen.sql.schema.*
import scala.quoted.*

final class DeriveTableRepr[A: Type](
    instances: K0.Expressions[RowRepr, A],
)(using quotes: Quotes, fTpe: Type[RowRepr], generic: K0.ProductGeneric[A]) {

  private def schemaNameExpr: Expr[String] =
    Expr { generic.annotations.optionalOfValue[schemaName].fold("public")(_.name) }

  private def tableNameExpr: Expr[String] =
    Expr { generic.annotations.optionalOfValue[tableName].fold(generic.label.camelToSnake)(_.name) }

  private def rowReprExpr: Expr[RowRepr.ProductRepr[A]] =
    DeriveProductRowRepr[A](instances).derive

  private def makePartial(isPK: Boolean): (TypeRepr, Expr[TableRepr.Partial[A, ?]]) = {
    type B
    val subset: K0.ProductGeneric.Subset[A, B] =
      generic.filtered[B] { [i] => (_, _) ?=> (field: generic.Field[i]) =>
        field.annotations.optionalOfValue[primaryKey].nonEmpty == isPK
      }
    given Type[B] = subset.bTpe

    val rowReprExpr: Expr[RowRepr[B]] =
      subset.subInstance.fromDeriver[RowRepr](
        DeriveProductRowRepr(_),
        instances,
        '{ RowRepr.Empty },
      )

    subset.bGeneric.typeRepr ->
      '{
        TableRepr.Partial[A, B](
          _get = ${ subset.convertExpr },
          rowRepr = $rowReprExpr,
        )
      }
  }

  def derive: Expr[TableRepr.Typed[A, ?, ?]] = {
    val (pkTpe, pkPartial) = makePartial(true)
    val (npkTpe, npkPartial) = makePartial(false)

    type PK
    type NPK

    given Type[PK] = pkTpe.asTypeOf
    given Type[NPK] = npkTpe.asTypeOf

    '{
      TableRepr.TypedImpl[A, PK, NPK](
        schemaName = $schemaNameExpr,
        tableName = $tableNameExpr,
        rowRepr = $rowReprExpr,
        pk = ${ pkPartial.asExprOf[TableRepr.Partial[A, PK]] },
        npk = ${ npkPartial.asExprOf[TableRepr.Partial[A, NPK]] },
      )
    }
  }

}
