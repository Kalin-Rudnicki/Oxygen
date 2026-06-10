package oxygen.cli

import oxygen.predef.core.*
import zio.*

trait CompletionOptions[A] {
  def completionOptions(in: String): Task[Seq[String]]
}
object CompletionOptions extends CompletionOptionsLowPriority.LowPriority1 {

  def filterPrefix(values: Seq[String], in: String): Seq[String] =
    val low = in.toLowerCase
    val matched = values.filter(_.toLowerCase.startsWith(low))
    if matched.nonEmpty then matched else values

  def completeList[A](completion: CompletionOptions[A], in: String): List[String] =
    Unsafe.unsafely {
      Runtime.default.unsafe.run(completion.completionOptions(in)).getOrThrowFiberFailure().toList
    }

  def helpHints[A](completion: CompletionOptions[A]): List[HelpHint] = completion match
    case FromStrictEnum(se) => HelpHint.EnumValues(se.encodedValues) :: Nil
    case _                  => Nil

  final class Empty[A] extends CompletionOptions[A] {
    override def completionOptions(in: String): Task[Seq[String]] = ZIO.succeed(Nil)
  }

  final case class FromStrictEnum[A](strictEnum: StrictEnum[A]) extends CompletionOptions[A] {
    override def completionOptions(in: String): Task[Seq[String]] =
      ZIO.succeed(CompletionOptions.filterPrefix(strictEnum.encodedValues, in))
  }

}

object CompletionOptionsLowPriority {

  trait LowPriority1 extends LowPriority2 {

    given strictEnum: [A: StrictEnum as se] => CompletionOptions[A] = CompletionOptions.FromStrictEnum(se)

  }

  trait LowPriority2 {

    given empty: [A] => CompletionOptions[A] = new CompletionOptions.Empty[A]

  }

}