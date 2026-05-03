package oxygen.zio

enum CauseType {

  case Failure
  case Defect
  case Interrupt
  case Empty

  final def isFailure: Boolean = this match
    case CauseType.Failure => true
    case _                 => false

}
