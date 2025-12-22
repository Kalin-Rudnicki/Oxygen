package oxygen.json.generic

import oxygen.json.*
import oxygen.meta.{*, given}
import oxygen.meta.k0.*
import oxygen.predef.core.*
import scala.quoted.*

final class DeriveSumJsonEncoder[A](
    instances: Expressions[JsonEncoder.ObjectEncoder, A],
)(using Quotes, Type[JsonEncoder.ObjectEncoder], Type[A], SumGeneric[A])
    extends Derivable.SumDeriver[JsonEncoder.ObjectEncoder, A] {

  private def addDiscriminator(discrimKey: Expr[String], discrimValue: Expr[String], inner: Expr[Growable[(String, Json)]]): Expr[Growable[(String, Json)]] =
    '{
      ($discrimKey, Json.Str($discrimValue)) +: $inner
    }

  private def makeEncodeJsonObjectFields(value: Expr[A]): Expr[Growable[(String, Json)]] =
    generic.matcher.instance[Growable[(String, Json)]](value) { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) =>

      val caseNameExpr = Expr(kase.annotations.optionalOfValue[jsonType].fold(kase.name)(_.name))

      kase.caseExtractor.withRHS { value =>
        '{
          Growable.single(
            $caseNameExpr ->
              ${ kase.getExpr(instances) }.encodeJsonAST($value),
          )
        }
      }
    }

  private def makeEncodeJsonObjectFields(value: Expr[A], discrimKey: Expr[String]): Expr[Growable[(String, Json)]] =
    generic.matcher.instance[Growable[(String, Json)]](value) { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) =>

      val caseNameExpr = Expr(kase.annotations.optionalOfValue[jsonType].fold(kase.name)(_.name))

      kase.caseExtractor.withRHS { value =>
        addDiscriminator(
          discrimKey,
          caseNameExpr,
          '{ ${ kase.getExpr(instances) }.encodeJsonObjectFields($value) },
        )
      }
    }

  override def derive: Expr[JsonEncoder.ObjectEncoder[A]] =
    generic.annotations.optionalOfValue[jsonDiscriminator] match {
      case Some(discrim) =>
        '{
          new JsonEncoder.ObjectEncoder[A] {
            override def encodeJsonObjectFields(value: A): Growable[(String, Json)] = ${ makeEncodeJsonObjectFields('value, Expr(discrim.name)) }
          }
        }
      case None =>
        '{
          new JsonEncoder.ObjectEncoder[A] {
            override def encodeJsonObjectFields(value: A): Growable[(String, Json)] = ${ makeEncodeJsonObjectFields('value) }
          }
        }
    }

}
