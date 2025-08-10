package oxygen.json.generic

import oxygen.json.*
import oxygen.meta.{*, given}
import oxygen.predef.core.*
import scala.quoted.*

final class DeriveProductJsonEncoder[A](
    instances: K0.Expressions[JsonEncoder, A],
)(using Quotes, Type[JsonEncoder.ObjectEncoder], Type[A], K0.ProductGeneric[A])
    extends K0.Derivable.ProductDeriver[JsonEncoder.ObjectEncoder, A] {

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

    '{ Json.Obj(${ fields.to[Contiguous].seqToExpr }.flattenIterable) }
  }

  override def derive: Expr[JsonEncoder.ObjectEncoder[A]] =
    '{
      new JsonEncoder.ObjectEncoder[A] {
        override def encodeJsonAST(value: A): Json.Obj = ${ makeEncodeJsonAST('value) }
      }
    }

}
