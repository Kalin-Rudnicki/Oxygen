package oxygen.json.generic

import oxygen.json.*
import oxygen.meta.{*, given}
import oxygen.meta.k0.*
import oxygen.predef.core.*
import scala.quoted.*

final class DeriveProductJsonEncoder[A](
    instances: Expressions[JsonEncoder, A],
)(using Quotes, Type[JsonEncoder.ObjectEncoder], Type[A], ProductGeneric[A])
    extends Derivable.ProductDeriver[JsonEncoder.ObjectEncoder, A] {

  private def makeEncodeObjectFields(value: Expr[A]): Expr[Growable[(String, Json)]] = {
    val fields: Growable[Expr[Growable[(String, Json)]]] =
      generic.mapChildren.mapExpr[Growable[(String, Json)]] { [a] => (_, _) ?=> (field: generic.Field[a]) =>

        val fieldNameExpr: Expr[String] = Expr(field.annotations.optionalOfValue[jsonField].fold(field.name)(_.name))
        val instanceExpr: Expr[JsonEncoder[a]] = field.getExpr(instances)
        val fieldExpr: Expr[a] = field.fromParent(value)
        val isFlattened: Boolean = field.annotations.optionalOf[jsonFlatten].nonEmpty

        if isFlattened then
          // TODO (KR) : is there a more type-safe & compile-time way to do this?
          '{
            $instanceExpr.toObjectEncoderOrThrow.encodeJsonObjectFields($fieldExpr)
          }
        else
          '{
            if $instanceExpr.addToObject($fieldExpr) then
              Growable.single {
                $fieldNameExpr ->
                  $instanceExpr.encodeJsonAST($fieldExpr)
              }
            else
              Growable.empty
          }
      }

    '{ ${ fields.seqToExpr }.flatten }
  }

  override def derive: Expr[JsonEncoder.ObjectEncoder[A]] =
    '{
      new JsonEncoder.ObjectEncoder[A] {
        override def encodeJsonObjectFields(value: A): Growable[(String, Json)] = ${ makeEncodeObjectFields('value) }
      }
    }

}
