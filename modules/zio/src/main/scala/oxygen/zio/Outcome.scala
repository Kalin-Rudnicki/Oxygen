package oxygen.zio

import oxygen.core.Enum
import oxygen.predef.json.*
import zio.Exit

enum Outcome extends Enum[Outcome] { case Success, Failure, Defect }
object Outcome extends Enum.Companion[Outcome] {

  def fromExit(exit: Exit[?, ?]): Outcome =
    exit match {
      case Exit.Success(_)                        => Success
      case Exit.Failure(cause) if cause.isFailure => Failure
      case _                                      => Defect
    }

}
