package oxygen.slyce.core

import scala.annotation.Annotation
import scala.util.matching.Regex

final case class regex(r: Regex) extends Annotation
