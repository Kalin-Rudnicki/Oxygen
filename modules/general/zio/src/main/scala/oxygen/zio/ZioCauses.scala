package oxygen.zio

import oxygen.predef.core.*
import zio.*

opaque type ZioCauses <: ArraySeq[Error] = ArraySeq[Error]
object ZioCauses {

  def fromCauseWithType(cause: Cause[?]): (ZioCauses, CauseType) = {
    val extracted: ExtractedCauses[?] = ExtractedCauses.fromCause(cause)
    (extracted.toErrors.toArraySeq, extracted.causeType)
  }

  def fromCause(cause: Cause[?]): ZioCauses = {
    val extracted: ExtractedCauses[?] = ExtractedCauses.fromCause(cause)
    extracted.toErrors.toArraySeq
  }

}
