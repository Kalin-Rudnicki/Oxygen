package oxygen.zio

import oxygen.predef.core.*
import scala.annotation.tailrec
import zio.*

/**
  * Working with a [[ZIO]] [[Cause]] can sometimes be a bit difficult.
  * The trickiest part is the representation of [[Cause.Then]] and [[Cause.Both]],
  * which gives two valid representations for multiple errors.
  *
  * For the library itself, it's good that it returns with this level of granularity, but when handling these Causes,
  * we generally don't want to have to handle every possible permutation, we more care about a tiered approach of handling the different root Cause types.
  * ([[Cause.Fail]], [[Cause.Die]], [[Cause.Interrupt]]).
  *
  * In general, there is a sort of "priority" to these, in that:
  * - [[Cause.Fail]] is more important than a [[Cause.Die]]
  * - [[Cause.Die]] is more important than a [[Cause.Interrupt]]
  * - [[Cause.Interrupt]] is more important than a [[Cause.Empty]]
  *
  * By converting a [[Cause]] into an [[ExtractedCauses]], we can easily match on the [[ExtractedCauses]] to see what case we are in.
  */
sealed trait ExtractedCauses[+E] {

  final def foldPriority[T](
      fail: NonEmptyList[Cause.Fail[E]] => T,
      die: NonEmptyList[Cause.Die] => T,
      interrupt: NonEmptyList[Cause.Interrupt] => T,
      empty: => T,
  ): T =
    this match {
      case ExtractedCauses.Failures(failures, _, _) => fail(failures)
      case ExtractedCauses.Defects(defects, _)      => die(defects)
      case ExtractedCauses.Interrupts(interrupts)   => interrupt(interrupts)
      case ExtractedCauses.Empty                    => empty
    }

  final def foldPriorityHead[T](
      fail: Cause.Fail[E] => T,
      die: Cause.Die => T,
      interrupt: Cause.Interrupt => T,
      empty: => T,
  ): T =
    foldPriority(
      fail = nel => fail(nel.head),
      die = nel => die(nel.head),
      interrupt = nel => interrupt(nel.head),
      empty = empty,
    )

  def map[E2](f: E => E2): ExtractedCauses[E2]
  def mapEither[E2](f: E => Either[Throwable, E2]): ExtractedCauses[E2]
  def recoverSome[E2 >: E](f: Throwable => Option[E2]): ExtractedCauses[E2]
  final def recoverSomePartial[E2 >: E](f: PartialFunction[Throwable, E2]): ExtractedCauses[E2] = recoverSome(f.lift)
  def recoverSomeTrace[E2 >: E](f: (Throwable, StackTrace) => Option[E2]): ExtractedCauses[E2]
  final def recoverSomeTracePartial[E2 >: E](f: PartialFunction[(Throwable, StackTrace), E2]): ExtractedCauses[E2] = {
    val f2 = f.lift
    recoverSomeTrace { (die, trace) => f2((die, trace)) }
  }
  def mapEitherRecoverSome[E2](f: E => Either[Throwable, E2], g: Throwable => Option[E2]): ExtractedCauses[E2]
  final def mapEitherRecoverSomePartial[E2](f: E => Either[Throwable, E2], g: PartialFunction[Throwable, E2]): ExtractedCauses[E2] = mapEitherRecoverSome(f, g.lift)
  def mapEitherRecoverSomeTrace[E2](f: E => Either[Throwable, E2], g: (Throwable, StackTrace) => Option[E2]): ExtractedCauses[E2]
  final def mapEitherRecoverSomeTracePartial[E2](f: E => Either[Throwable, E2], g: PartialFunction[(Throwable, StackTrace), E2]): ExtractedCauses[E2] = {
    val g2 = g.lift
    mapEitherRecoverSomeTrace(f, (die, trace) => g2((die, trace)))
  }

  def reduceFailures[E2 >: E](f: NonEmptyList[E] => E2): ExtractedCauses[E2]
  final def reduceFailuresLeft[E2 >: E](f: (E2, E) => E2): ExtractedCauses[E2] = reduceFailures(_.reduceLeft(f))

  private def toCauses: List[Cause[E]] =
    this match {
      case ExtractedCauses.Failures(failures, Nil, Nil)            => failures.toList
      case ExtractedCauses.Failures(failures, defects, interrupts) => List(failures.toList, defects, interrupts).flatten
      case ExtractedCauses.Defects(defects, Nil)                   => defects.toList
      case ExtractedCauses.Defects(defects, interrupts)            => defects.toList ++ interrupts
      case ExtractedCauses.Interrupts(interrupts)                  => interrupts.toList
      case ExtractedCauses.Empty                                   => Nil
    }

  final def toCause: Cause[E] =
    this.toCauses match
      case cause :: Nil     => cause
      case causeH :: causeT => causeT.foldLeft(causeH)(Cause.Both(_, _))
      case Nil              => Cause.Empty

  final def eitherNelFailures: Either[ExtractedCauses.NoFailures, NonEmptyList[E]] = this match
    case ExtractedCauses.Failures(failures, _, _) => failures.map(_.value).asRight
    case noFailures: ExtractedCauses.NoFailures   => noFailures.asLeft

  final def eitherFailure: Either[ExtractedCauses.NoFailures, E] = this match
    case ExtractedCauses.Failures(failures, _, _) => failures.head.value.asRight
    case noFailures: ExtractedCauses.NoFailures   => noFailures.asLeft

}
object ExtractedCauses {

  def fail[E](failure: E, trace: StackTrace = StackTrace.none): ExtractedCauses[E] =
    ExtractedCauses.Failures(NonEmptyList.one(Cause.Fail(failure, trace)), Nil, Nil)
  def die(defect: Throwable, trace: StackTrace = StackTrace.none): ExtractedCauses[Nothing] =
    ExtractedCauses.Defects(NonEmptyList.one(Cause.Die(defect, trace)), Nil)

  sealed trait NoFailures extends ExtractedCauses[Nothing] {

    override final def map[E2](f: Nothing => E2): this.type = this
    override final def mapEither[E2](f: Nothing => Either[Throwable, E2]): this.type = this
    override final def reduceFailures[E2 >: Nothing](f: NonEmptyList[Nothing] => E2): this.type = this

  }

  sealed trait NoDefects extends NoFailures {

    override final def recoverSome[E2 >: Nothing](f: Throwable => Option[E2]): this.type = this
    override final def recoverSomeTrace[E2 >: Nothing](f: (Throwable, StackTrace) => Option[E2]): this.type = this
    override final def mapEitherRecoverSome[E2](f: Nothing => Either[Throwable, E2], g: Throwable => Option[E2]): this.type = this
    override final def mapEitherRecoverSomeTrace[E2](f: Nothing => Either[Throwable, E2], g: (Throwable, StackTrace) => Option[E2]): this.type = this

  }

  final case class Failures[+E](failures: NonEmptyList[Cause.Fail[E]], defects: List[Cause.Die], interrupts: List[Cause.Interrupt]) extends ExtractedCauses[E] {

    override def map[E2](f: E => E2): ExtractedCauses.Failures[E2] =
      ExtractedCauses.Failures(
        failures.map { fail =>
          Cause.Fail(f(fail.value), fail.trace, fail.spans, fail.annotations)
        },
        defects,
        interrupts,
      )

    def mapEither[E2](f: E => Either[Throwable, E2]): ExtractedCauses[E2] =
      failures.partitionMap { fail =>
        f(fail.value) match {
          case Right(value) => Cause.Fail(value, fail.trace, fail.spans, fail.annotations).asLeft
          case Left(value)  => Cause.Die(value, fail.trace, fail.spans, fail.annotations).asRight
        }
      } match {
        case Ior.Left(stillFailures)             => ExtractedCauses.Failures(stillFailures, defects, interrupts)
        case Ior.Right(nowDefects)               => ExtractedCauses.Defects(defects ++: nowDefects, interrupts)
        case Ior.Both(stillFailures, nowDefects) => ExtractedCauses.Failures(stillFailures, defects ::: nowDefects.toList, interrupts)
      }

    override def recoverSome[E2 >: E](f: Throwable => Option[E2]): ExtractedCauses[E2] = {
      val (nowFailures, stillDefects): (List[Cause.Fail[E2]], List[Cause.Die]) =
        defects.partitionMap { die =>
          f(die.value) match {
            case Some(value) => Cause.Fail(value, die.trace, die.spans, die.annotations).asLeft
            case None        => die.asRight
          }
        }
      ExtractedCauses.Failures(failures :++ nowFailures, stillDefects, interrupts)
    }
    override def recoverSomeTrace[E2 >: E](f: (Throwable, StackTrace) => Option[E2]): ExtractedCauses[E2] = {
      val (nowFailures, stillDefects): (List[Cause.Fail[E2]], List[Cause.Die]) =
        defects.partitionMap { die =>
          f(die.value, die.trace) match {
            case Some(value) => Cause.Fail(value, die.trace, die.spans, die.annotations).asLeft
            case None        => die.asRight
          }
        }
      ExtractedCauses.Failures(failures :++ nowFailures, stillDefects, interrupts)
    }

    override def reduceFailures[E2 >: E](f: NonEmptyList[E] => E2): ExtractedCauses.Failures[E2] = {
      val h = failures.head
      ExtractedCauses.Failures(
        NonEmptyList.one(Cause.Fail(f(failures.map(_.value)), h.trace, h.spans, h.annotations)),
        defects,
        interrupts,
      )
    }

    override def mapEitherRecoverSome[E2](f: E => Either[Throwable, E2], g: Throwable => Option[E2]): ExtractedCauses[E2] = {
      val (stillFailures, nowDefects): (List[Cause.Fail[E2]], List[Cause.Die]) =
        failures.toList.partitionMap { fail =>
          f(fail.value) match {
            case Right(value) => Cause.Fail(value, fail.trace, fail.spans, fail.annotations).asLeft
            case Left(value)  =>
              g(value) match {
                case Some(value) => Cause.Fail(value, fail.trace, fail.spans, fail.annotations).asLeft
                case None        => Cause.Die(value, fail.trace, fail.spans, fail.annotations).asRight
              }
          }
        }
      val (nowFailures, stillDefects): (List[Cause.Fail[E2]], List[Cause.Die]) =
        defects.partitionMap { die =>
          g(die.value) match {
            case Some(value) => Cause.Fail(value, die.trace, die.spans, die.annotations).asLeft
            case None        => die.asRight
          }
        }

      ExtractedCauses.fromRootCauses(stillFailures ++ nowFailures, nowDefects ++ stillDefects, interrupts)
    }

    override def mapEitherRecoverSomeTrace[E2](f: E => Either[Throwable, E2], g: (Throwable, StackTrace) => Option[E2]): ExtractedCauses[E2] = {
      val (stillFailures, nowDefects): (List[Cause.Fail[E2]], List[Cause.Die]) =
        failures.toList.partitionMap { fail =>
          f(fail.value) match {
            case Right(value) => Cause.Fail(value, fail.trace, fail.spans, fail.annotations).asLeft
            case Left(value)  =>
              g(value, fail.trace) match {
                case Some(value) => Cause.Fail(value, fail.trace, fail.spans, fail.annotations).asLeft
                case None        => Cause.Die(value, fail.trace, fail.spans, fail.annotations).asRight
              }
          }
        }
      val (nowFailures, stillDefects): (List[Cause.Fail[E2]], List[Cause.Die]) =
        defects.partitionMap { die =>
          g(die.value, die.trace) match {
            case Some(value) => Cause.Fail(value, die.trace, die.spans, die.annotations).asLeft
            case None        => die.asRight
          }
        }

      ExtractedCauses.fromRootCauses(stillFailures ++ nowFailures, nowDefects ++ stillDefects, interrupts)
    }

  }

  final case class Defects(defects: NonEmptyList[Cause.Die], interrupts: List[Cause.Interrupt]) extends ExtractedCauses.NoFailures {

    override def recoverSome[E2 >: Nothing](f: Throwable => Option[E2]): ExtractedCauses[E2] =
      defects.partitionMap { die =>
        f(die.value) match {
          case Some(value) => Cause.Fail(value, die.trace, die.spans, die.annotations).asLeft
          case None        => die.asRight
        }
      } match {
        case Ior.Left(nowFailures)               => ExtractedCauses.Failures(nowFailures, Nil, interrupts)
        case Ior.Right(stillDefects)             => ExtractedCauses.Defects(stillDefects, interrupts)
        case Ior.Both(nowFailures, stillDefects) => ExtractedCauses.Failures(nowFailures, stillDefects.toList, interrupts)
      }

    override def recoverSomeTrace[E2 >: Nothing](f: (Throwable, StackTrace) => Option[E2]): ExtractedCauses[E2] =
      defects.partitionMap { die =>
        f(die.value, die.trace) match {
          case Some(value) => Cause.Fail(value, die.trace, die.spans, die.annotations).asLeft
          case None        => die.asRight
        }
      } match {
        case Ior.Left(nowFailures)               => ExtractedCauses.Failures(nowFailures, Nil, interrupts)
        case Ior.Right(stillDefects)             => ExtractedCauses.Defects(stillDefects, interrupts)
        case Ior.Both(nowFailures, stillDefects) => ExtractedCauses.Failures(nowFailures, stillDefects.toList, interrupts)
      }

    override def mapEitherRecoverSome[E2](f: Nothing => Either[Throwable, E2], g: Throwable => Option[E2]): ExtractedCauses[E2] = recoverSome(g)
    override def mapEitherRecoverSomeTrace[E2](f: Nothing => Either[Throwable, E2], g: (Throwable, StackTrace) => Option[E2]): ExtractedCauses[E2] = recoverSomeTrace(g)

  }

  final case class Interrupts(interrupts: NonEmptyList[Cause.Interrupt]) extends ExtractedCauses.NoDefects
  case object Empty extends ExtractedCauses.NoDefects

  def fromRootCauses[E](
      failures: List[Cause.Fail[E]],
      defects: List[Cause.Die],
      interrupts: List[Cause.Interrupt],
  ): ExtractedCauses[E] =
    NonEmptyList.fromList(failures) match {
      case Some(failures) => Failures(failures, defects, interrupts)
      case None           =>
        NonEmptyList.fromList(defects) match {
          case Some(defects) => Defects(defects, interrupts)
          case None          =>
            NonEmptyList.fromList(interrupts) match {
              case Some(interrupts) => Interrupts(interrupts)
              case None             => Empty
            }
        }
    }

  @tailrec
  private def loop[E](
      queue: List[Cause[E]],
      rFailures: List[Cause.Fail[E]],
      rDefects: List[Cause.Die],
      rInterrupts: List[Cause.Interrupt],
  ): ExtractedCauses[E] =
    queue match {
      case qHead :: qTail =>
        qHead match {
          case fail: Cause.Fail[E]        => loop(qTail, fail :: rFailures, rDefects, rInterrupts)
          case die: Cause.Die             => loop(qTail, rFailures, die :: rDefects, rInterrupts)
          case Cause.Then(left, right)    => loop(left :: right :: qTail, rFailures, rDefects, rInterrupts)
          case Cause.Both(left, right)    => loop(left :: right :: qTail, rFailures, rDefects, rInterrupts)
          case Cause.Stackless(cause, _)  => loop(cause :: qTail, rFailures, rDefects, rInterrupts)
          case Cause.Empty                => loop(qTail, rFailures, rDefects, rInterrupts)
          case interrupt: Cause.Interrupt => loop(qTail, rFailures, rDefects, interrupt :: rInterrupts)
        }
      case Nil =>
        fromRootCauses(rFailures.reverse, rDefects.reverse, rInterrupts.reverse)
    }

  def fromCause[E](cause: Cause[E]): ExtractedCauses[E] =
    loop(cause :: Nil, Nil, Nil, Nil)

}
