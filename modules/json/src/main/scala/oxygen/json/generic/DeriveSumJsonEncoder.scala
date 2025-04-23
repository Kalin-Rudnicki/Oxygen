package oxygen.json.generic

import oxygen.json.*
import oxygen.meta.*
import scala.quoted.*

@scala.annotation.nowarn("msg=unused import")
final class DeriveSumJsonEncoder[Q <: Quotes, A](val k0: K0[Q])(generic: k0.SumGeneric[A], instances: k0.ValExpressions[JsonEncoder]) {
  import generic.tpe
  private given quotes: Q = k0.meta.quotes

  private def makeEncodeJsonAST(value: Expr[A]): Expr[Json] =
    generic.builders.matchOnInstance[Json](value) {
      [i <: A] =>
        (kase: generic.Case[i], value: Expr[i]) =>
          import kase.given

          val caseName = Expr(kase.name)

          '{
            Json.obj(
              $caseName ->
                ${ kase.getExpr(instances) }.encodeJsonAST($value),
            )
        }
    }

  // NOTE : this needs to be called where the provided [[instances]] have been spliced into the expr.
  def makeJsonEncoder: Expr[JsonEncoder[A]] =
    '{
      new JsonEncoder[A] {
        override def encodeJsonAST(value: A): Json = ${ makeEncodeJsonAST('value) }
      }
    }

}
