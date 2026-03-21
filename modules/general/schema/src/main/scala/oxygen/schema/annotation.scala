package oxygen.schema

import oxygen.meta.FromExprT
import scala.annotation.Annotation

final case class doc(value: String) extends Annotation derives FromExprT
