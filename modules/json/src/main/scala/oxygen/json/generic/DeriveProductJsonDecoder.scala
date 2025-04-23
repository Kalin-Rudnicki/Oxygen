package oxygen.json.generic

import oxygen.json.*
import oxygen.meta.*
import oxygen.predef.core.*
import scala.quoted.*

@scala.annotation.nowarn("msg=unused import")
final class DeriveProductJsonDecoder[Q <: Quotes, A](val k0: K0[Q])(generic: k0.ProductGeneric[A], instances: k0.ValExpressions[JsonDecoder]) {
  import generic.tpe
  private given quotes: Q = k0.meta.quotes

  private def makeDecodeJsonAST(map: Expr[Map[String, Json]]): Expr[Either[JsonError, A]] =
    generic.builders.eitherMapToInstance[JsonError] {
      [a] =>
        (field: generic.Field[a]) =>
          import field.given

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

  // NOTE : this needs to be called where the provided [[instances]] have been spliced into the expr.
  def makeJsonDecoder: Expr[JsonDecoder[A]] =
    '{
      new JsonDecoder[A] {
        override def decodeJsonAST(ast: Json): Either[JsonError, A] = ast match {
          case obj: Json.Obj => ${ makeDecodeJsonAST('{ obj.valueMap }) }
          case _             => JsonError(Nil, JsonError.Cause.InvalidType(Json.Type.Object, ast.tpe)).asLeft
        }
      }
    }

}
