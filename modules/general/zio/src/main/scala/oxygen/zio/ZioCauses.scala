package oxygen.zio

import oxygen.predef.core.*
import zio.*

opaque type ZioCauses <: ArraySeq[Error] = ArraySeq[Error]
object ZioCauses {

  def fromCauseWithType(cause: Cause[?]): (ZioCauses, CauseType) = {
    val extracted = ExtractedCauses.fromCause(cause)
    (extracted.toErrors.toArraySeq, extracted.causeType)
  }

  def fromCause(cause: Cause[?]): ZioCauses = {
    val extracted = ExtractedCauses.fromCause(cause)
    extracted.toErrors.toArraySeq
  }

  enum CauseType {

    case Failure
    case Defect
    case Interrupt
    case Empty

    final def isFailure: Boolean = this match
      case CauseType.Failure => true
      case _                 => false

  }

}
