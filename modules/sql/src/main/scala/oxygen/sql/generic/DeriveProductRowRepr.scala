package oxygen.sql.generic

import oxygen.predef.core.*
import oxygen.predef.meta.*
import oxygen.sql.*
import oxygen.sql.schema.*

@scala.annotation.nowarn("msg=unused import")
final class DeriveProductRowRepr[Q <: Quotes, A](val k0: K0[Q])(generic: k0.ProductGeneric[A], instances: k0.ValExpressions[RowRepr]) {
  import generic.given
  import k0.given

  private def decoderInstances: k0.ValExpressions[ResultDecoder] =
    instances.mapK[ResultDecoder] {
      [i] =>
        (inst: k0.ValExpressions.Elem[RowRepr, i]) =>
          import inst.given

          '{ ${ inst.expr }.decoder }
    }

  private def encoderInstances: k0.ValExpressions[InputEncoder] =
    instances.mapK[InputEncoder] {
      [i] =>
        (inst: k0.ValExpressions.Elem[RowRepr, i]) =>
          import inst.given

          '{ ${ inst.expr }.encoder }
    }

  private def makeProductSchemas: Expr[Contiguous[(String, RowRepr[?])]] =
    generic.builders.mapToContiguousExpr {
      [i] =>
        (field: generic.Field[i]) =>
          import field.given

          '{ (${ Expr(field.name) }, ${ field.getExpr(instances) }) }
    }

  def makeRowRepr: Expr[RowRepr.ProductRepr[A]] = {
    if (generic.fields.isEmpty)
      k0.meta.report.errorAndAbort("Not allowed: product schema with no fields")

    '{
      new RowRepr.ProductRepr[A] {

        override val productFields: Contiguous[(String, RowRepr[?])] = $makeProductSchemas

        override val columns: Columns[A] = Columns(productFields.flatMap(_._2.columns.columns))

        override val decoder: ResultDecoder[A] = ${ DeriveProductResultDecoder[Q, A](k0)(generic, decoderInstances).makeResultDecoder }

        override val encoder: InputEncoder[A] = ${ DeriveProductInputEncoder[Q, A](k0)(generic, encoderInstances).makeInputEncoder }

      }
    }
  }

}
object DeriveProductRowRepr {

  @scala.annotation.nowarn("msg=unused import")
  def populateFields[Q <: Quotes, A, B: Type](k0: K0[Q])(generic: k0.ProductGeneric[A])(f: k0.ValExpressions[RowRepr] => Expr[B]): Expr[B] = {
    import k0.meta.given

    generic.builders.withLazyValExpressions[RowRepr, B] {
      [i] =>
        (field: generic.Field[i]) =>
          import field.given

          val isInline: Boolean =
            field.optionalAnnotationValue[inlineColumnNames].nonEmpty

          val columnNameExpr: Expr[String] =
            Expr { field.optionalAnnotationValue[columnName].fold(field.name.camelToSnake)(_.name) }

          isInline match {
            case true  => '{ ${ field.summonTypeClass[RowRepr] }.prefixedInline($columnNameExpr) }
            case false => '{ ${ field.summonTypeClass[RowRepr] }.prefixed($columnNameExpr) }
        }
    } { f }
  }

}
