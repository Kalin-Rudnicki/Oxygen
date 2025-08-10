package oxygen.json.generic

import oxygen.json.*
import oxygen.meta.{*, given}
import oxygen.predef.core.*
import scala.quoted.*

final class DeriveSumJsonDecoder[A](
    instances: K0.Expressions[JsonDecoder.ObjectDecoder, A],
)(using Quotes, Type[JsonDecoder.ObjectDecoder], Type[A], K0.SumGeneric[A])
    extends K0.Derivable.SumDeriver[JsonDecoder.ObjectDecoder, A] {

  private val validKeysExpr: Expr[String] =
    Expr(
      generic.cases
        .map { kase =>
          val name = kase.annotations.optionalOfValue[jsonType].fold(kase.name)(_.name)
          s"'$name'"
        }
        .mkString(", "),
    )

  private def makeDecodeJsonAST(key: Expr[String], value: Expr[Json]): Expr[Either[JsonError, A]] =
    generic.matcher.value[String, Either[JsonError, A]](key) { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) =>

      val caseNameExpr = Expr(kase.annotations.optionalOfValue[jsonType].fold(kase.name)(_.name))

      CaseExtractor.const[String](caseNameExpr).withRHS { _ =>
        '{
          ${ kase.getExpr(instances) }.decodeJsonAST($value).leftMap(_.inField($key))
        }
      }
    } {
      '{
        JsonError(JsonError.Path.Field($key) :: Nil, JsonError.Cause.DecodingFailed("Invalid key, expected one of: " + $validKeysExpr)).asLeft
      }
    }

  private def deriveNoDiscriminator: Expr[JsonDecoder.ObjectDecoder[A]] =
    '{
      new JsonDecoder.ObjectDecoder[A] {
        override def decodeJsonObjectAST(obj: Json.Obj): Either[JsonError, A] =
          obj.value.toList match {
            case (key, value) :: Nil => ${ makeDecodeJsonAST('key, 'value) }
            case _                   => JsonError(Nil, JsonError.Cause.DecodingFailed("Expected single object key, expected one of: " + $validKeysExpr)).asLeft // TODO (KR) : include keys?
          }
      }
    }

  private def matchOnStr(key: Expr[String], obj: Expr[Json.Obj]): Expr[Either[JsonError, A]] =
    generic.matcher.value[String, Either[JsonError, A]](key) { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) =>

      val caseNameExpr = Expr(kase.annotations.optionalOfValue[jsonType].fold(kase.name)(_.name))

      CaseExtractor.const[String](caseNameExpr).withRHS { _ =>
        '{
          ${ kase.getExpr(instances) }.decodeJsonAST($obj).leftMap(_.inField($key))
        }
      }
    } {
      '{
        JsonError(JsonError.Path.Field($key) :: Nil, JsonError.Cause.DecodingFailed("Invalid key, expected one of: " + $validKeysExpr)).asLeft
      }
    }

  private def deriveWithDiscriminator(discrim: String): Expr[JsonDecoder.ObjectDecoder[A]] = {
    val discrimExpr = Expr(discrim)
    '{
      new JsonDecoder.ObjectDecoder[A] {
        override def decodeJsonObjectAST(obj: Json.Obj): Either[JsonError, A] =
          obj.valueMap.get($discrimExpr) match {
            case Some(Json.Str(parsedDiscrim)) => ${ matchOnStr('parsedDiscrim, 'obj) }
            case Some(json)                    => JsonError(JsonError.Path.Field($discrimExpr) :: Nil, JsonError.Cause.InvalidType(Json.Type.String, json.tpe)).asLeft
            case None                          => JsonError(JsonError.Path.Field($discrimExpr) :: Nil, JsonError.Cause.MissingRequired).asLeft
          }
      }
    }
  }

  override def derive: Expr[JsonDecoder.ObjectDecoder[A]] =
    generic.annotations.optionalOfValue[jsonDiscriminator] match {
      case Some(discrim) => deriveWithDiscriminator(discrim.name)
      case None          => deriveNoDiscriminator
    }

}
