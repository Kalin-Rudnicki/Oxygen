package oxygen.quoted

import oxygen.quoted.companion.*
import scala.quoted.*

object Implicits {

  def companion(using quotes: Quotes): ImplicitsCompanion = ImplicitsCompanion(using quotes)
  given Quotes => Conversion[Implicits.type, ImplicitsCompanion] = _.companion

}
