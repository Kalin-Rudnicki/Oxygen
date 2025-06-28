package oxygen.json.generic

import oxygen.json.*
import oxygen.meta.*
import scala.quoted.*

final class DeriveSumJsonEncoder[A](
    instances: K0.Expressions[JsonEncoder, A],
)(using Quotes, Type[JsonEncoder], Type[A], K0.SumGeneric[A])
    extends K0.Derivable.SumDeriver[JsonEncoder, A] {

  private def makeEncodeJsonAST(value: Expr[A]): Expr[Json] =
    generic.matcher.instance[Json](value) { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) =>

      val caseName = Expr(kase.name)

      kase.caseExtractor.withRHS { value =>
        '{
          Json.obj(
            $caseName ->
              ${ kase.getExpr(instances) }.encodeJsonAST($value),
          )
        }
      }
    }

  override def derive: Expr[JsonEncoder[A]] =
    '{
      new JsonEncoder[A] {
        override def encodeJsonAST(value: A): Json = ${ makeEncodeJsonAST('value) }
      }
    }

}
