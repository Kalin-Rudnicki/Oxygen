package oxygen.cli

import oxygen.predef.core.*
import zio.*

trait CompletionOptions[A] {
  def completionOptions: Task[Seq[String]]
}
object CompletionOptions extends AutoCompleteLowPriority.LowPriority1 {

  final class Empty[A] extends CompletionOptions[A] {
    override def completionOptions: Task[Seq[String]] = ZIO.succeed(Nil)
  }

  final case class FromStrictEnum[A](strictEnum: StrictEnum[A]) extends CompletionOptions[A] {
    override def completionOptions: Task[Seq[String]] = ??? // FIX-PRE-MERGE (KR) :
  }

}

object AutoCompleteLowPriority {

  trait LowPriority1 extends LowPriority2 {

    given strictEnum: [A: StrictEnum as se] => CompletionOptions[A] = ??? // FIX-PRE-MERGE (KR) :

  }

  trait LowPriority2 {

    given empty: [A] => CompletionOptions[A] = new CompletionOptions.Empty[A]

  }

}
