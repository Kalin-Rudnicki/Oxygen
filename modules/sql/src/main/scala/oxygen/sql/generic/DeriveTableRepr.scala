package oxygen.sql.generic

import oxygen.predef.core.*
import oxygen.predef.meta.*
import oxygen.sql.*
import oxygen.sql.schema.*
import scala.annotation.nowarn

@scala.annotation.nowarn("msg=unused import")
final class DeriveTableRepr[Q <: Quotes, A, K: Type](val k0: K0[Q])(generic: k0.ProductGeneric[A], instances: k0.ValExpressions[RowRepr]) {
  import generic.given
  import k0.given
  import k0.meta.*

  private val kRepr: TypeRepr = TypeRepr.of[K]

  private def schemaNameExpr: Expr[String] =
    Expr {
      generic.optionalAnnotationValue[schemaName].fold("public")(_.name)
    }

  private def tableNameExpr: Expr[String] =
    Expr {
      generic.optionalAnnotationValue[tableName].fold(generic.label.camelToSnake)(_.name)
    }

  private def rowReprExpr: Expr[RowRepr.ProductRepr[A]] =
    DeriveProductRowRepr[Q, A](k0)(generic, instances).makeRowRepr

  private lazy val fieldsWithIsPK: Contiguous[(generic.Field[?], Boolean)] =
    Contiguous.from(generic.fields).map { f => (f, f.optionalAnnotationValue[primaryKey].nonEmpty) }

  private lazy val pkFields: Contiguous[generic.Field[?]] =
    fieldsWithIsPK.collect { case (f, true) => f }

  private lazy val nonPKFields: Contiguous[generic.Field[?]] =
    fieldsWithIsPK.collect { case (f, false) => f }

  @nowarn("msg=unused local definition")
  private def makePartial(columns: Contiguous[generic.Field[?]]): (TypeRepr, Expr[TableRepr.Partial[A, ?]]) =
    columns match {
      case Contiguous() =>
        TypeRepr.of[Unit] ->
          '{
            TableRepr.Partial[A, Unit](
              get = _ => (),
              rowRepr = RowRepr.Empty,
            )
          }
      case Contiguous(single) =>
        type T
        given Type[T] = single.typeRepr.asTyped
        val field: generic.Field[T] = single.asInstanceOf[generic.Field[T]]

        single.typeRepr ->
          '{
            TableRepr.Partial[A, T](
              get = (a: A) => ${ field.get('a) },
              rowRepr = ${ field.getExpr(instances) },
            )
          }
      case many =>
        val repr: TypeRepr = TypeRepr.tuplePreferTupleN(many.map(_.typeRepr).toSeq*)
        type T
        given Type[T] = repr.asTyped

        val tupGen: k0.ProductGeneric[T] =
          k0.ProductGeneric.of[T]
        val tupInstances: k0.ValExpressions[RowRepr] =
          k0.ValExpressions.unsafeMake[RowRepr](many.map { f => k0.ValExpressions.Elem.unsafeMake(f.getExpr(instances), f.tpe) })

        val tupRowRepr: Expr[RowRepr[T]] = DeriveProductRowRepr[Q, T](k0)(tupGen, tupInstances).makeRowRepr

        def get(a: Expr[A]): Expr[T] =
          tupGen.builders.mapToInstance {
            [i] =>
              (field: tupGen.Field[i]) =>
                import field.given

                many.at(field.idx).get(a).asExprOf[i]
          }

        repr ->
          '{
            TableRepr.Partial[A, T](
              get = (a: A) => ${ get('a) },
              rowRepr = $tupRowRepr,
            )
          }
    }

  def makeTableSchema: Expr[TableRepr[A, K]] = {
    val (pkTpe, pkPartial) = makePartial(pkFields)
    val (_, nonPKPartial) = makePartial(nonPKFields)

    if (!(pkTpe =:= kRepr))
      report.errorAndAbort(
        s"""Derived PK does not match specification:
           |  Expected: ${kRepr.show}
           |  Actual: ${pkTpe.show}""".stripMargin,
      )

    '{
      TableRepr[A, K](
        schemaName = $schemaNameExpr,
        tableName = $tableNameExpr,
        rowRepr = $rowReprExpr,
        pk = ${ pkPartial.asExprOf[TableRepr.Partial[A, K]] },
        nonPK = $nonPKPartial,
      )
    }
  }

}
