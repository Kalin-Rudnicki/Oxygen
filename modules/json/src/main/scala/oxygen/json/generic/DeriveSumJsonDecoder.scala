package oxygen.json.generic

import oxygen.json.*
import oxygen.meta.*
import oxygen.predef.core.*
import scala.quoted.*

final class DeriveSumJsonDecoder[A](
    instances: K0.Expressions[JsonDecoder, A],
)(using Quotes, Type[JsonDecoder], Type[A], K0.SumGeneric[A])
    extends K0.Derivable.SumDeriver[JsonDecoder, A] {

  private val validKeysExpr: Expr[String] =
    Expr(generic.cases.map { kase => s"'${kase.name}'" }.mkString(", "))

  private def makeDecodeJsonAST(key: Expr[String], value: Expr[Json]): Expr[Either[JsonError, A]] =
    generic.matcher.value[String, Either[JsonError, A]](key) { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) =>

      val caseName = Expr(kase.name)

      CaseExtractor.const[String](caseName).withRHS { _ =>
        '{
          ${ kase.getExpr(instances) }.decodeJsonAST($value).leftMap(_.inField($key))
        }
      }
    } {
      '{
        JsonError(JsonError.Path.Field($key) :: Nil, JsonError.Cause.DecodingFailed("Invalid key, expected one of: " + $validKeysExpr)).asLeft
      }
    }

  override def derive: Expr[JsonDecoder[A]] =
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
