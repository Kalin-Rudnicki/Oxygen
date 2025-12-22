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

  private def makeDecodeJsonAST(map: Expr[Map[String, Json]]): Expr[Either[JsonError, A]] =
    generic.instantiate.either[JsonError] { [a] => (_, _) ?=> (field: generic.Field[a]) =>

      val fieldNameExpr = Expr(field.annotations.optionalOfValue[jsonField].fold(field.name)(_.name))

      '{
        $map.get($fieldNameExpr) match {
          case Some(value) =>
            ${ field.getExpr(instances) }
              .decodeJsonAST(value)
              .leftMap(_.inField($fieldNameExpr))
          case None =>
            ${ field.getExpr(instances) }.onMissingFromObject
              .toRight(JsonError(JsonError.Path.Field($fieldNameExpr) :: Nil, JsonError.Cause.MissingRequired))
        }
      }
    }

  override def derive: Expr[JsonDecoder.ObjectDecoder[A]] =
    '{
      new JsonDecoder.ObjectDecoder[A] {
        override def decodeJsonObjectAST(obj: Json.Obj): Either[JsonError, A] = ${ makeDecodeJsonAST('{ obj.valueMap }) }
      }
    }

}
