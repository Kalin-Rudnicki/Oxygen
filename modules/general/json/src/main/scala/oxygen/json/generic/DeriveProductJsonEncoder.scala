package oxygen.json.generic

import oxygen.json.*
import oxygen.meta.{*, given}
import oxygen.meta.k0.*
import oxygen.predef.core.*
import scala.quoted.*

final class DeriveProductJsonEncoder[A](
    instances: Expressions[JsonEncoder, A],
)(using Quotes, Type[JsonEncoder.ObjectEncoder], Type[A], ProductGeneric[A])
    extends Derivable.ProductDeriver[JsonEncoder.ObjectEncoder, A] {

  private def makeEncodeJsonAST(value: Expr[A]): Expr[Json.Obj] = {
    val fields: Growable[Expr[Option[(String, Json)]]] =
      generic.mapChildren.mapExpr[Option[(String, Json)]] { [a] => (_, _) ?=> (field: generic.Field[a]) =>

        val fieldNameExpr = Expr(field.annotations.optionalOfValue[jsonField].fold(field.name)(_.name))

        '{
          Option.when(${ field.getExpr(instances) }.addToObject(${ field.fromParent(value) })) {
            $fieldNameExpr ->
              ${ field.getExpr(instances) }.encodeJsonAST(${ field.fromParent(value) })
          }
        }
      }

    '{ Json.Obj(${ fields.seqToArraySeqExpr }.flatten) }
  }

  override def derive: Expr[JsonEncoder.ObjectEncoder[A]] =
    '{
      new JsonEncoder.ObjectEncoder[A] {
        override def encodeJsonAST(value: A): Json.Obj = ${ makeEncodeJsonAST('value) }
      }
    }

}
