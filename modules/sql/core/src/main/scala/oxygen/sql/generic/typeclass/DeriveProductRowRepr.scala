package oxygen.sql.generic.typeclass

import oxygen.meta.{*, given}
import oxygen.meta.k0.*
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.*
import oxygen.sql.schema.*
import scala.quoted.*

final class DeriveProductRowRepr[A](
    instances: Expressions[RowRepr, A],
)(using Quotes, Type[RowRepr], Type[A], ProductGeneric[A])
    extends Derivable.ProductDeriver[RowRepr, A] {

  private def decoderInstances: Expressions[ResultDecoder, A] =
    instances.mapK[ResultDecoder] { [i] => _ ?=> (v: Expr[RowRepr[i]]) =>
      '{
        $v.decoder
      }
    }

  private def encoderInstances: Expressions[InputEncoder, A] =
    instances.mapK[InputEncoder] { [i] => _ ?=> (v: Expr[RowRepr[i]]) =>
      '{
        $v.encoder
      }
    }

  private def makeProductSchemas: Expr[ArraySeq[(String, RowRepr[?])]] =
    generic.mapChildren
      .mapExpr[(String, RowRepr[?])] { [i] => (_, _) ?=> (field: generic.Field[i]) =>
        '{
          (${ Expr(field.name) }, ${ field.getExpr(instances) })
        }
      }
      .seqToArraySeqExpr

  override def derive: Expr[RowRepr.ProductRepr[A]] = {
    if generic.fields.isEmpty then report.errorAndAbort("Not allowed: product schema with no fields")

    '{
      new RowRepr.ProductRepr[A] {

        override val productFields: ArraySeq[(String, RowRepr[?])] = $makeProductSchemas

        override val columns: Columns[A] = Columns(productFields.flatMap(_._2.columns.columns))

        override val decoder: ResultDecoder[A] = ${ DeriveProductResultDecoder(decoderInstances).derive }

        override val encoder: InputEncoder[A] = ${ DeriveProductInputEncoder(encoderInstances).derive }

      }
    }
  }

}
object DeriveProductRowRepr {

  def populateFields[A: Type](using quotes: Quotes, generic: ProductGeneric[A]): ValDefinitions[RowRepr, A] =
    generic.cacheVals[RowRepr]() { [i] => (_, _) ?=> (field: generic.Field[i]) =>
      val isInline: Boolean =
        field.annotations.optionalOfValue[inlineColumnNames].nonEmpty

      val columnNameExpr: Expr[String] =
        Expr { field.annotations.optionalOfValue[columnName].fold(field.name.camelToSnake)(_.name) }

      isInline match {
        case true  => '{ ${ field.summonTypeClass[RowRepr] }.prefixedInline($columnNameExpr) }
        case false => '{ ${ field.summonTypeClass[RowRepr] }.prefixed($columnNameExpr) }
      }
    }

}
