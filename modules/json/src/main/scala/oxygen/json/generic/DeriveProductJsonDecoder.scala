package oxygen.json.generic

import oxygen.json.*
import oxygen.meta.*
import oxygen.predef.core.*
import scala.quoted.*

final class DeriveProductJsonDecoder[A](
    instances: K0.Expressions[JsonDecoder, A],
)(using Quotes, Type[JsonDecoder], Type[A], K0.ProductGeneric[A])
    extends K0.Derivable.ProductDeriver[JsonDecoder, A] {

  private def makeDecodeJsonAST(map: Expr[Map[String, Json]]): Expr[Either[JsonError, A]] =
    generic.instantiate.either[JsonError] {
      [a] =>
        (_, _) ?=>
          (field: generic.Field[a]) =>

            val fieldName = Expr(field.name)

            '{
              $map.get($fieldName) match {
                case Some(value) =>
                  ${ field.getExpr(instances) }
                    .decodeJsonAST(value)
                    .leftMap(_.inField($fieldName))
                case None =>
                  ${ field.getExpr(instances) }.onMissingFromObject
                    .toRight(JsonError(JsonError.Path.Field($fieldName) :: Nil, JsonError.Cause.MissingRequired))
              }
          }
    }

  override def derive: Expr[JsonDecoder[A]] =
    '{
      new JsonDecoder[A] {
        override def decodeJsonAST(ast: Json): Either[JsonError, A] = ast match {
          case obj: Json.Obj => ${ makeDecodeJsonAST('{ obj.valueMap }) }
          case _             => JsonError(Nil, JsonError.Cause.InvalidType(Json.Type.Object, ast.tpe)).asLeft
        }
      }
    }

}
