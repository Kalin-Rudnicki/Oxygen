package oxygen.json

import oxygen.predef.core.*
import scala.annotation.Annotation
import scala.quoted.*

final case class jsonField(name: String) extends Annotation
object jsonField {

  given FromExpr[jsonField] =
    new FromExpr[jsonField] {
      override def unapply(x: Expr[jsonField])(using Quotes): Option[jsonField] = x match
        case '{ new `jsonField`(${ Expr(name) }) }   => jsonField(name).some
        case '{ `jsonField`.apply(${ Expr(name) }) } => jsonField(name).some
        case _                                       => None
    }

}

final case class jsonType(name: String) extends Annotation
object jsonType {

  given FromExpr[jsonType] =
    new FromExpr[jsonType] {
      override def unapply(x: Expr[jsonType])(using Quotes): Option[jsonType] = x match
        case '{ new `jsonType`(${ Expr(name) }) }   => jsonType(name).some
        case '{ `jsonType`.apply(${ Expr(name) }) } => jsonType(name).some
        case _                                      => None
    }

}

final case class jsonDiscriminator(name: String) extends Annotation
object jsonDiscriminator {

  given FromExpr[jsonDiscriminator] =
    new FromExpr[jsonDiscriminator] {
      override def unapply(x: Expr[jsonDiscriminator])(using Quotes): Option[jsonDiscriminator] = x match
        case '{ new `jsonDiscriminator`(${ Expr(name) }) }   => jsonDiscriminator(name).some
        case '{ `jsonDiscriminator`.apply(${ Expr(name) }) } => jsonDiscriminator(name).some
        case _                                               => None
    }

}
