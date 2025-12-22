package oxygen.json.generic

import oxygen.json.*
import oxygen.meta.{*, given}
import oxygen.meta.k0.*
import oxygen.predef.core.*
import scala.quoted.*

final class DeriveProductJsonDecoder[A](
    instances: Expressions[JsonDecoder, A],
)(using Quotes, Type[JsonDecoder.ObjectDecoder], Type[A], ProductGeneric[A])
    extends Derivable.ProductDeriver[JsonDecoder.ObjectDecoder, A] {

  private def makeDecodeJsonAST(obj: Expr[Json.Obj], map: Expr[Map[String, Json]]): Expr[Either[JsonError, A]] =
    generic.instantiate.either[JsonError] { [a] => (_, _) ?=> (field: generic.Field[a]) =>

      val fieldNameExpr: Expr[String] = Expr(field.annotations.optionalOfValue[jsonField].fold(field.name)(_.name))
      val instanceExpr: Expr[JsonDecoder[a]] = field.getExpr(instances)
      val isFlattened: Boolean = field.annotations.optionalOf[jsonFlatten].nonEmpty

      if isFlattened then
        // TODO (KR) : is there a more type-safe & compile-time way to do this?
        '{
          $instanceExpr.toObjectDecoderOrThrow.decodeJsonObjectAST($obj, $map)
        }
      else
        '{
          $map.get($fieldNameExpr) match {
            case Some(value) =>
              $instanceExpr
                .decodeJsonAST(value)
                .leftMap(_.inField($fieldNameExpr))
            case None =>
              $instanceExpr.onMissingFromObject
                .toRight(JsonError(JsonError.Path.Field($fieldNameExpr) :: Nil, JsonError.Cause.MissingRequired))
          }
        }
    }

  override def derive: Expr[JsonDecoder.ObjectDecoder[A]] =
    '{
      new JsonDecoder.ObjectDecoder[A] {
        override def decodeJsonObjectAST(obj: Json.Obj, fieldMap: Map[String, Json]): Either[JsonError, A] = ${ makeDecodeJsonAST('obj, 'fieldMap) }
      }
    }

}
