package oxygen.cli

import oxygen.predef.core.*
import zio.*

trait AutoComplete[A] {
  def completionOptions: Task[Seq[String]]
}
object AutoComplete extends AutoCompleteLowPriority.LowPriority1 {

  final class Empty[A] extends AutoComplete[A] {
    override def completionOptions: Task[Seq[String]] = ZIO.succeed(Nil)
  }

  final case class FromStrictEnum[A](strictEnum: StrictEnum[A]) extends AutoComplete[A] {
    override def completionOptions: Task[Seq[String]] = ??? // FIX-PRE-MERGE (KR) :
  }

}

object AutoCompleteLowPriority {

  trait LowPriority1 extends LowPriority2 {

    given strictEnum: [A: StrictEnum as se] => AutoComplete[A] = ??? // FIX-PRE-MERGE (KR) :

  }

  trait LowPriority2 {

    given empty: [A] => AutoComplete[A] = new AutoComplete.Empty[A]

  }

}
