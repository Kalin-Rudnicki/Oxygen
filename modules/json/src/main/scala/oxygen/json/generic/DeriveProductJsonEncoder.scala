package oxygen.json.generic

import oxygen.json.*
import oxygen.meta.*
import scala.quoted.*

final class DeriveProductJsonEncoder[Q <: Quotes, A](val k0: K0[Q])(generic: k0.ProductGeneric[A], instances: k0.ValExpressions[JsonEncoder]) {
  import generic.tpe
  private given quotes: Q = k0.meta.quotes

  private def makeEncodeJsonAST(value: Expr[A]): Expr[Json] = {
    val fields: Seq[Expr[Option[(String, Json)]]] =
      generic.builders.mapToSeq[Expr[Option[(String, Json)]]] {
        [a] =>
          (field: generic.Field[a]) =>
            import field.given

            val fieldName = Expr(field.name)

            '{
              Option.when(${ field.getExpr(instances) }.addToObject(${ field.get(value) })) {
                $fieldName ->
                  ${ field.getExpr(instances) }.encodeJsonAST(${ field.get(value) })
              }
          }
      }

    '{ Json.obj(${ Expr.ofSeq(fields) }.flatten*) }
  }

  // NOTE : this needs to be called where the provided [[instances]] have been spliced into the expr.
  def makeJsonEncoder: Expr[JsonEncoder[A]] =
    '{
      new JsonEncoder[A] {
        override def encodeJsonAST(value: A): Json = ${ makeEncodeJsonAST('value) }
      }
    }

}
