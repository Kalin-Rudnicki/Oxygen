package oxygen.json

import oxygen.meta.*
import oxygen.predef.core.*
import scala.annotation.Annotation
import scala.quoted.*

final case class jsonField(name: String) extends Annotation derives ToExprT, FromExprT

final case class jsonType(name: String) extends Annotation derives ToExprT, FromExprT

final case class jsonDiscriminator(name: String) extends Annotation derives ToExprT, FromExprT
