package oxygen.json.generic

import oxygen.json.*
import oxygen.meta.*
import oxygen.predef.core.*
import scala.quoted.*

final class DeriveSumJsonDecoder[Q <: Quotes, A](val k0: K0[Q])(generic: k0.SumGeneric[A], instances: k0.ValExpressions[JsonDecoder]) {
  import generic.tpe
  private given quotes: Q = k0.meta.quotes

  private val validKeysExpr: Expr[String] =
    Expr(generic.cases.map { kase => s"'${kase.name}'" }.mkString(", "))

  private def makeDecodeJsonAST(key: Expr[String], value: Expr[Json]): Expr[Either[JsonError, A]] =
    generic.builders.matchOnInput[String, Either[JsonError, A]](key) {
      [i <: A] =>
        (kase: generic.Case[i]) =>
          import kase.given

          val caseName = Expr(kase.name)

          (
            caseName,
            '{
              ${ kase.getExpr(instances) }.decodeJsonAST($value).leftMap(_.inField($key))
            },
        )
    } {
      '{
        JsonError(JsonError.Path.Field($key) :: Nil, JsonError.Cause.DecodingFailed("Invalid key, expected one of: " + $validKeysExpr)).asLeft
      }
    }

  // NOTE : this needs to be called where the provided [[instances]] have been spliced into the expr.
  def makeJsonDecoder: Expr[JsonDecoder[A]] =
    '{
      new JsonDecoder[A] {
        override def decodeJsonAST(ast: Json): Either[JsonError, A] = ast match {
          case obj: Json.Obj =>
            obj.value.toList match {
              case (key, value) :: Nil => ${ makeDecodeJsonAST('key, 'value) }
              case _                   => JsonError(Nil, JsonError.Cause.DecodingFailed("Expected single object key, expected one of: " + $validKeysExpr)).asLeft // TODO (KR) : include keys?
            }
          case _ => JsonError(Nil, JsonError.Cause.InvalidType(Json.Type.Object, ast.tpe)).asLeft
        }
      }
    }

}
