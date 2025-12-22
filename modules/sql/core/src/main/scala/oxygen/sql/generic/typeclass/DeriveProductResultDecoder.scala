package oxygen.sql.generic.typeclass

import oxygen.meta.{*, given}
import oxygen.meta.k0.*
import oxygen.predef.core.*
import oxygen.sql.*
import oxygen.sql.error.*
import oxygen.sql.schema.*
import scala.quoted.*

final class DeriveProductResultDecoder[A](
    instances: Expressions[ResultDecoder, A],
)(using Quotes, Type[ResultDecoder], Type[A], ProductGeneric[A])
    extends Derivable.ProductDeriver[ResultDecoder, A] {

  private def makeSize(using Quotes): Expr[Int] =
    generic.mapChildren.foldLeftExpr[Int](Expr(0)) { [i] => (_, _) ?=> (field: generic.Field[i], acc: Expr[Int]) =>
      '{
        $acc + ${ field.getExpr(instances) }.size
      }
    }

  private def makeDecode(offset: Expr[Int], values: Expr[ArraySeq[Matchable]])(using Quotes): Expr[Either[QueryError.UnableToDecodeRow, A]] =
    generic.cacheVals
      .foldLeftDelayed[Int](
        valName = name => s"offset_$name",
      )(offset) { // TODO (KR) : consider caching these outside the schema, or within a val
        [i] => (_, _) ?=> (field: generic.Field[i], acc: Expr[Int]) =>
          '{
            $acc + ${ field.getExpr(instances) }.size
          }
      }
      .defineAndUse { (offsets, _) =>
        generic.instantiate.either { [i] => (_, _) ?=> (field: generic.Field[i]) =>
          '{
            ${ field.getExpr(instances) }.__decodeInternal(${ field.getExpr(offsets) }, $values)
          }
        }
      }

  private def makeToStringHeader(size: Expr[Int])(using Quotes): Growable[Expr[String]] =
    Growable[Expr[String]](
      Expr(s"ResultDecoder.CustomDecoder(type = ${generic.typeRepr.show}, size = "),
      '{ $size.toString },
      Expr(")"),
    )

  private def makeToStringBody(using Quotes): Growable[Expr[String]] =
    generic.mapChildren.flatMapExpr[Growable, String] { [i] => (_, _) ?=> (field: generic.Field[i]) =>
      Growable(
        Expr("\n    "),
        '{ ${ field.getExpr(instances) }.toString },
      )
    }

  override def derive: Expr[ResultDecoder.CustomDecoder[A]] =
    '{
      new ResultDecoder.CustomDecoder[A] {

        override val size: Int = $makeSize

        override def __decodeInternal(offset: Int, values: ArraySeq[Matchable]): Either[QueryError.UnableToDecodeRow, A] =
          ${ makeDecode('offset, 'values) }

        override def toString: String = ${ (makeToStringHeader('size) ++ makeToStringBody).to[Seq].exprMkString }

      }
    }

}
