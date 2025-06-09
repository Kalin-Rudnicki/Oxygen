package oxygen.quoted

import oxygen.quoted.companion.*
import scala.quoted.*

object report {

  def companion(using quotes: Quotes): reportCompanion = reportCompanion(using quotes)
  given Quotes => Conversion[report.type, reportCompanion] = _.companion

}
