package oxygen.executable

import oxygen.predef.core.*

enum DefaultLoggerType(final val value: String) derives StrictEnum {

  case OxygenAll extends DefaultLoggerType("OXYGEN_ALL")
  case OxygenLean extends DefaultLoggerType("OXYGEN_LEAN")
  case Zio extends DefaultLoggerType("ZIO")

  override final def toString: String = value

}
object DefaultLoggerType {

  val default: DefaultLoggerType = OxygenLean

}
