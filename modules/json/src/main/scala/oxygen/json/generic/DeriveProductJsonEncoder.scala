package oxygen.json.generic

import oxygen.json.*
import oxygen.meta.*
import oxygen.predef.core.*
import scala.quoted.*

final class DeriveProductJsonEncoder[A](
    instances: K0.Expressions[JsonEncoder, A],
)(using Quotes, Type[JsonEncoder], Type[A], K0.ProductGeneric[A])
    extends K0.Derivable.ProductDeriver[JsonEncoder, A] {

  private def makeEncodeJsonAST(value: Expr[A]): Expr[Json] = {
    val fields: Growable[Expr[Option[(String, Json)]]] =
      generic.mapChildren.mapExpr[Option[(String, Json)]] {
        [a] =>
          (_, _) ?=>
            (field: generic.Field[a]) =>

              val fieldName = Expr(field.name)

              '{
                Option.when(${ field.getExpr(instances) }.addToObject(${ field.fromParent(value) })) {
                  $fieldName ->
                    ${ field.getExpr(instances) }.encodeJsonAST(${ field.fromParent(value) })
                }
            }
      }

    '{ Json.Obj(${ fields.to[Contiguous].seqToExpr }.flattenIterable) }
  }

  override def derive: Expr[JsonEncoder[A]] =
    '{
      new JsonEncoder[A] {
        override def encodeJsonAST(value: A): Json = ${ makeEncodeJsonAST('value) }
      }
    }

}
