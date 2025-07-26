package oxygen.json.generic

import oxygen.json.*
import oxygen.meta.*
import scala.quoted.*

final class DeriveSumJsonEncoder[A](
    instances: K0.Expressions[JsonEncoder, A],
)(using Quotes, Type[JsonEncoder], Type[A], K0.SumGeneric[A])
    extends K0.Derivable.SumDeriver[JsonEncoder, A] {

  private def unsafeAddDiscriminator(discrimKey: Expr[String], discrimValue: Expr[String], json: Expr[Json]): Expr[Json] =
    '{
      val tmp: Json = $json
      tmp match {
        case Json.Obj(value) => Json.Obj(($discrimKey, Json.Str($discrimValue)) +: value)
        case _               => throw new RuntimeException(s"Unable to add discriminator, child is not an object: $tmp")
      }
    }

  private def makeEncodeJsonAST(value: Expr[A]): Expr[Json] =
    generic.matcher.instance[Json](value) { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) =>

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

  private def makeEncodeJsonAST(value: Expr[A], discrimKey: Expr[String]): Expr[Json] =
    generic.matcher.instance[Json](value) { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) =>

      val caseNameExpr = Expr(kase.annotations.optionalOfValue[jsonType].fold(kase.name)(_.name))

      kase.caseExtractor.withRHS { value =>
        unsafeAddDiscriminator(
          discrimKey,
          caseNameExpr,
          '{ ${ kase.getExpr(instances) }.encodeJsonAST($value) },
        )
      }
    }

  override def derive: Expr[JsonEncoder[A]] =
    generic.annotations.optionalOfValue[jsonDiscriminator] match {
      case Some(discrim) =>
        '{
          new JsonEncoder[A] {
            override def encodeJsonAST(value: A): Json = ${ makeEncodeJsonAST('value, Expr(discrim.name)) }
          }
        }
      case None =>
        '{
          new JsonEncoder[A] {
            override def encodeJsonAST(value: A): Json = ${ makeEncodeJsonAST('value) }
          }
        }
    }

}
