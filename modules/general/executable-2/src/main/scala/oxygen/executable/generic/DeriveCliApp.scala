package oxygen.executable.generic

// import oxygen.quoted.*
import scala.quoted.*

private[executable] object DeriveCliApp {

  @scala.annotation.nowarn
  def derive[A: Type](using Quotes): Expr[Nothing] = {
    val repr: RawCliAppRepr[A] = new RawCliAppRepr[A](isRoot = true)

    // report.errorAndAbort("[todo]\n\n" + repr.toString)

    // FIX-PRE-MERGE (KR) :
    '{ ??? }
  }

}
