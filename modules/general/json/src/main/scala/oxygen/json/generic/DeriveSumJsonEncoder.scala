package oxygen.json.generic

import oxygen.json.*
import oxygen.json.SecretUtil.*
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

  private def addSplitDiscriminator(
      discrimKey: Expr[String],
      discrimValue: Expr[String],
      inner: Expr[Growable[(String, Ior[PlainTextJson, SecretJson])]],
  ): Expr[Growable[(String, Ior[PlainTextJson, SecretJson])]] =
    '{
      val childRes: ArraySeq[(String, Ior[PlainTextJson, SecretJson])] = $inner.toArraySeq
      val hasPlain: Boolean = childRes.exists(_._2.leftOption.nonEmpty)
      val discrimStringValue: Ior[PlainTextJson, SecretJson] =
        if hasPlain then Ior.Left(PlainTextJson.wrap(Json.Str($discrimValue)))
        else Ior.Right(SecretJson.wrap(Json.Str($discrimValue)))

      ($discrimKey, discrimStringValue) +: Growable.WrappedArraySeq(childRes)
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

  private def makeEncodeSplitJsonObjectFields(value: Expr[A]): Expr[Growable[(String, Ior[PlainTextJson, SecretJson])]] =
    generic.matcher.instance[Growable[(String, Ior[PlainTextJson, SecretJson])]](value) { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) =>

      val caseNameExpr = Expr(kase.annotations.optionalOfValue[jsonType].fold(kase.name)(_.name))

      kase.caseExtractor.withRHS { value =>
        '{
          Growable.single(
            $caseNameExpr ->
              ${ kase.getExpr(instances) }.encodeSplitJsonAST($value),
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

  private def makeEncodeSplitJsonObjectFields(value: Expr[A], discrimKey: Expr[String]): Expr[Growable[(String, Ior[PlainTextJson, SecretJson])]] =
    generic.matcher.instance[Growable[(String, Ior[PlainTextJson, SecretJson])]](value) { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) =>

      val caseNameExpr = Expr(kase.annotations.optionalOfValue[jsonType].fold(kase.name)(_.name))

      kase.caseExtractor.withRHS { value =>
        addSplitDiscriminator(
          discrimKey,
          caseNameExpr,
          '{ ${ kase.getExpr(instances) }.encodeSplitJsonObjectFields($value) },
        )
      }
    }

  override def derive: Expr[JsonEncoder.ObjectEncoder[A]] = {
    val baseExpr: Expr[JsonEncoder.ObjectEncoder[A]] =
      generic.annotations.optionalOfValue[jsonDiscriminator] match {
        case Some(discrim) =>
          '{
            new JsonEncoder.ObjectEncoder[A] {
              override def encodeJsonObjectFields(value: A): Growable[(String, Json)] = ${ makeEncodeJsonObjectFields('value, Expr(discrim.name)) }
              override def encodeSplitJsonObjectFields(value: A): Growable[(String, Ior[PlainTextJson, SecretJson])] = ${ makeEncodeSplitJsonObjectFields('value, Expr(discrim.name)) }
            }
          }
        case None =>
          '{
            new JsonEncoder.ObjectEncoder[A] {
              override def encodeJsonObjectFields(value: A): Growable[(String, Json)] = ${ makeEncodeJsonObjectFields('value) }
              override def encodeSplitJsonObjectFields(value: A): Growable[(String, Ior[PlainTextJson, SecretJson])] = ${ makeEncodeSplitJsonObjectFields('value) }
            }
          }
      }
    val isPlain: Boolean = generic.annotations.optionalOf[jsonSecret].isEmpty
    if isPlain then baseExpr
    else '{ $baseExpr.secret }
  }

}
