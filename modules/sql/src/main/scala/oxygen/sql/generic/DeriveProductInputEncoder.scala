package oxygen.sql.generic

import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.sql.*
import oxygen.sql.query.InputWriter
import oxygen.sql.schema.*
import scala.quoted.*

final class DeriveProductInputEncoder[A](
    instances: K0.Expressions[InputEncoder, A],
)(using Quotes, Type[InputEncoder], Type[A], K0.ProductGeneric[A])
    extends K0.Derivable.ProductDeriver[InputEncoder, A] {

  private def makeSize: Expr[Int] =
    generic.mapChildren.foldLeftExpr[Int](Expr(0)) { [i] => (_, _) ?=> (field: generic.Field[i], acc: Expr[Int]) =>
      '{
        $acc + ${ field.getExpr(instances) }.size
      }
    }

  private def makeEncodeInner(writer: Expr[InputWriter], value: Expr[A]): Growable[Expr[Unit]] =
    generic.mapChildren.mapExpr[Unit] { [i] => (_, _) ?=> (field: generic.Field[i]) =>
      '{
        ${ field.getExpr(instances) }.unsafeEncode($writer, ${ field.fromParent(value) })
      }
    }

  private def makeEncode(writer: Expr[InputWriter], value: Expr[A]): Expr[Unit] =
    Expr.block(
      makeEncodeInner(writer, value).to[List],
      '{ () },
    )

  override def derive: Expr[InputEncoder.CustomEncoder[A]] =
    '{
      new InputEncoder.CustomEncoder[A] {

        override val size: Int = $makeSize

        override def unsafeEncode(writer: InputWriter, value: A): Unit = ${ makeEncode('writer, 'value) }

      }
    }

}
