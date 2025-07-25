package oxygen.json.generic

import oxygen.json.*
import oxygen.meta.{*, given}
import scala.quoted.*

final class DeriveSumJsonEncoder[A](
    instances: K0.Expressions[JsonEncoder.ObjectEncoder, A],
)(using Quotes, Type[JsonEncoder.ObjectEncoder], Type[A], K0.SumGeneric[A])
    extends K0.Derivable.SumDeriver[JsonEncoder.ObjectEncoder, A] {

  private def addDiscriminator(discrimKey: Expr[String], discrimValue: Expr[String], json: Expr[Json.Obj]): Expr[Json.Obj] =
    '{
      Json.Obj(($discrimKey, Json.Str($discrimValue)) +: $json.value)
    }

  private def makeEncodeJsonAST(value: Expr[A]): Expr[Json.Obj] =
    generic.matcher.instance[Json.Obj](value) { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) =>

      val caseNameExpr = Expr(kase.annotations.optionalOfValue[jsonType].fold(kase.name)(_.name))

      kase.caseExtractor.withRHS { value =>
        '{
          Json.obj(
            $caseNameExpr ->
              ${ kase.getExpr(instances) }.encodeJsonAST($value),
          )
        }
      }
    }

  private def makeEncodeJsonAST(value: Expr[A], discrimKey: Expr[String]): Expr[Json.Obj] =
    generic.matcher.instance[Json.Obj](value) { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) =>

      val caseNameExpr = Expr(kase.annotations.optionalOfValue[jsonType].fold(kase.name)(_.name))

      kase.caseExtractor.withRHS { value =>
        addDiscriminator(
          discrimKey,
          caseNameExpr,
          '{ ${ kase.getExpr(instances) }.encodeJsonAST($value) },
        )
      }
    }

  override def derive: Expr[JsonEncoder.ObjectEncoder[A]] =
    generic.annotations.optionalOfValue[jsonDiscriminator] match {
      case Some(discrim) =>
        '{
          new JsonEncoder.ObjectEncoder[A] {
            override def encodeJsonAST(value: A): Json.Obj = ${ makeEncodeJsonAST('value, Expr(discrim.name)) }
          }
        }
      case None =>
        '{
          new JsonEncoder.ObjectEncoder[A] {
            override def encodeJsonAST(value: A): Json.Obj = ${ makeEncodeJsonAST('value) }
          }
        }
    }

}
