package oxygen.zio

import oxygen.predef.core.*
import scala.annotation.tailrec
import zio.*

sealed trait ExtractedCauses[+E]
object ExtractedCauses {

  def fail[E](failure: E, trace: StackTrace = StackTrace.none): ExtractedCauses[E] =
    ExtractedCauses.Failures(NonEmptyList.one(Cause.Fail(failure, trace)), Nil)
  def die(defect: Throwable, trace: StackTrace = StackTrace.none): ExtractedCauses[Nothing] =
    ExtractedCauses.Defects(NonEmptyList.one(Cause.Die(defect, trace)))

  final case class Failures[+E](failures: NonEmptyList[Cause.Fail[E]], defects: List[Cause.Die]) extends ExtractedCauses[E]
  final case class Defects(defects: NonEmptyList[Cause.Die]) extends ExtractedCauses[Nothing]
  final case class Other(cause: Cause[Nothing]) extends ExtractedCauses[Nothing]

  @tailrec
  private def loop[E](
      initialCause: Cause[E],
      queue: List[Cause[E]],
      rFailures: List[Cause.Fail[E]],
      rDefects: List[Cause.Die],
  ): ExtractedCauses[E] =
    queue match {
      case qHead :: qTail =>
        qHead match {
          case fail: Cause.Fail[E]       => loop(initialCause, qTail, fail :: rFailures, rDefects)
          case die: Cause.Die            => loop(initialCause, qTail, rFailures, die :: rDefects)
          case Cause.Then(left, right)   => loop(initialCause, left :: right :: qTail, rFailures, rDefects)
          case Cause.Both(left, right)   => loop(initialCause, left :: right :: qTail, rFailures, rDefects)
          case Cause.Stackless(cause, _) => loop(initialCause, cause :: qTail, rFailures, rDefects)
          case Cause.Empty               => loop(initialCause, qTail, rFailures, rDefects)
          case _: Cause.Interrupt        => loop(initialCause, qTail, rFailures, rDefects)
        }
      case Nil =>
        NonEmptyList.fromList(rFailures.reverse) match {
          case Some(failures) => Failures(failures, rDefects.reverse)
          case None           =>
            NonEmptyList.fromList(rDefects.reverse) match {
              case Some(defects) => Defects(defects)
              case None          => Other(initialCause.asInstanceOf[Cause[Nothing]])
            }
        }
    }

  def fromCause[E](cause: Cause[E]): ExtractedCauses[E] =
    loop(cause, cause :: Nil, Nil, Nil)

}
