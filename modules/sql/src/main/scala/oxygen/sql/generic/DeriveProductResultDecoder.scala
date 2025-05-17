package oxygen.sql.generic

import oxygen.predef.core.*
import oxygen.predef.meta.*
import oxygen.sql.*
import oxygen.sql.error.*
import oxygen.sql.schema.*

@scala.annotation.nowarn("msg=unused import")
final class DeriveProductResultDecoder[Q <: Quotes, A](val k0: K0[Q])(generic: k0.ProductGeneric[A], instances: k0.ValExpressions[ResultDecoder]) {
  import generic.given
  import k0.given

  private def makeSize: Expr[Int] =
    generic.builders.foldLeft(Expr(0)) {
      [i] =>
        (acc: Expr[Int], field: generic.Field[i]) =>
          import field.given

          '{ $acc + ${ field.getExpr(instances) }.size }
    }

  private def makeDecode(offset: Expr[Int], values: Expr[Contiguous[Matchable]]): Expr[Either[QueryError.UnableToDecodeRow, A]] =
    generic.builders.withValExpressionsFold('{ ($offset, $offset) }) { // TODO (KR) : consider caching these outside the schema, or within a val
      [i] =>
        (acc: Expr[(Int, Int)], field: generic.Field[i]) =>
          import field.given

          '{ ($acc._2, $acc._2 + ${ field.getExpr(instances) }.size) }
    } { offsets =>
      generic.builders.eitherMapToInstance {
        [i] =>
          (field: generic.Field[i]) =>
            import field.given

            '{ ${ field.getExpr(instances) }.__decodeInternal(${ field.getExpr(offsets) }._1, $values) }
      }
    }

  private def makeToStringHeader(size: Expr[Int]): Contiguous[Expr[String]] =
    Contiguous[Expr[String]](
      Expr(s"ResultDecoder.CustomDecoder(type = ${generic.typeRepr.show}, size = "),
      '{ $size.toString },
      Expr(")"),
    )

  private def makeToStringBody: Contiguous[Expr[String]] =
    generic.builders.mapToContiguous {
      [i] =>
        (field: generic.Field[i]) =>
          import field.given

          '{ s"\n    ${${ field.getExpr(instances) }}" }
    }

  def makeResultDecoder: Expr[ResultDecoder.CustomDecoder[A]] =
    '{
      new ResultDecoder.CustomDecoder[A] {

        override val size: Int = $makeSize

        override def __decodeInternal(offset: Int, values: Contiguous[Matchable]): Either[QueryError.UnableToDecodeRow, A] =
          ${ makeDecode('offset, 'values) }

        override def toString: String = ${ (makeToStringHeader('size) ++ makeToStringBody).flattenExprs }.mkString

      }
    }

}
