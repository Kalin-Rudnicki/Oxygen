package oxygen.cli

import oxygen.predef.core.*

enum HelpType { case Help, HelpExtra }
object HelpType {
  given strictEnum: StrictEnum[HelpType] = StrictEnum.make(values.toSeq)
}
