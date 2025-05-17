package oxygen.sql.generic

import oxygen.predef.meta.*
import oxygen.sql.*
import oxygen.sql.query.InputWriter
import oxygen.sql.schema.*

@scala.annotation.nowarn("msg=unused import")
final class DeriveProductInputEncoder[Q <: Quotes, A](val k0: K0[Q])(generic: k0.ProductGeneric[A], instances: k0.ValExpressions[InputEncoder]) {
  import generic.given
  import k0.given

  private def makeSize: Expr[Int] =
    generic.builders.foldLeft(Expr(0)) {
      [i] =>
        (acc: Expr[Int], field: generic.Field[i]) =>
          import field.given

          '{ $acc + ${ field.getExpr(instances) }.size }
    }

  private def makeEncodeInner(writer: Expr[InputWriter], value: Expr[A]): Seq[Expr[Unit]] =
    generic.builders.mapToSeq {
      [i] =>
        (field: generic.Field[i]) =>
          import field.given

          '{ ${ field.getExpr(instances) }.unsafeEncode($writer, ${ field.get(value) }) }
    }

  private def makeEncode(writer: Expr[InputWriter], value: Expr[A]): Expr[Unit] =
    Expr.block(
      makeEncodeInner(writer, value).toList,
      '{ () },
    )

  def makeInputEncoder: Expr[InputEncoder.CustomEncoder[A]] =
    '{
      new InputEncoder.CustomEncoder[A] {

        override val size: Int = $makeSize

        override def unsafeEncode(writer: InputWriter, value: A): Unit = ${ makeEncode('writer, 'value) }

      }
    }

}
