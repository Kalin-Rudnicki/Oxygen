package oxygen.test

import oxygen.core.collection.NonEmptyList
import oxygen.core.syntax.common.*
import scala.Console.*
import zio.test.*
import zio.test.Assertion.*

object OxygenAssertions {

  extension [A](self: Assertion[A]) {

    def cmap[B](name: String)(f: PartialFunction[B, A]): Assertion[B] =
      assertionRec[B, A](name)(self)(f.lift(_))

  }

  extension [A](self: Assertion[Seq[A]]) {

    def toNelAssertion: Assertion[NonEmptyList[A]] =
      self.cmap("NonEmptyList")(_.toList)

  }

  def assertSeq[A](assertions: Assertion[A]*): Assertion[Seq[A]] =
    assertions.toList.zipWithIndex.foldLeft[Assertion[Seq[A]]](
      hasSize(equalTo(assertions.size)),
    ) { case (accum, (a, i)) =>
      accum && hasAt(i)(a)
    }

  def seqsHaveSameSize[A]: Assertion[(Seq[A], Seq[A])] =
    Assertion.assertion("Seqs have same size") { case (seqA, seqB) => seqA.size == seqB.size }

  def testSeqElements[A](test: (A, A) => Boolean)(makeMessage: (A, A, Int) => ErrorMessage): Assertion[(Seq[A], Seq[A])] =
    Assertion(
      TestArrow.make[(Seq[A], Seq[A]), Boolean] { case (seqA, seqB) =>
        val sizes =
          TestTrace.boolean(seqA.size == seqB.size) {
            ErrorMessage.text("Size of") ++
              ErrorMessage.pretty(seqA) ++
              ErrorMessage.equals ++
              ErrorMessage.pretty(seqB)
          }

        seqA
          .zip(seqB)
          .zipWithIndex
          .map { case ((a, b), i) =>
            TestTrace.boolean(test(a, b))(makeMessage(a, b, i))
          }
          .foldLeft(sizes) { (acc, t) => acc && t }
      },
    )
  def testSeqElements[A](test: (A, A) => Boolean): Assertion[(Seq[A], Seq[A])] =
    testSeqElements(test) { (a, b, i) =>
      ErrorMessage.text("Seq test") +
        ErrorMessage.choice("passed", "failed") +
        ErrorMessage.text(s"at index $i") +
        ErrorMessage.text("with elems:") ++
        ErrorMessage.text("left =") +
        ErrorMessage.pretty(a) ++
        ErrorMessage.text("right =") +
        ErrorMessage.pretty(b)
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Equals+Diff
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private val defaultShow: PartialFunction[Matchable, String] = {
    case string: String  => string.unesc
    case array: Array[?] => array.mkString("Array(", ", ", ")")
  }

  private sealed trait Value {

    final def showPretty(color: String, show: Any => Option[String]): String = this match
      case Value.Actual(value) => show(value).fold(showGeneric(value, color, show))(_.split('\n').map(s => s"$color$s$RESET").mkString("\n"))
      case Value.MissingIndex  => s"$color[[ index missing ]]$RESET"

  }
  private object Value {
    final case class Actual(value: Any) extends Value
    case object MissingIndex extends Value
  }

  extension (self: String) {
    private def prefixAndIndent(prefix: String): String =
      prefix + self.replaceAll("\n", "\n" + (" " * prefix.stripColor.length))
  }

  private def expectedActual(expected: Value, actual: Value, show: Any => Option[String]): String =
    expected.showPretty(RED, show).prefixAndIndent(s"${CYAN}expected$RESET: ") + "\n" +
      actual.showPretty(RED, show).prefixAndIndent(s"  ${CYAN}actual$RESET: ")

  private def showGeneric(any: Any, color: String, show: Any => Option[String]): String =
    show(any) match {
      case Some(value) =>
        Value.Actual(value).showPretty(color, _.toString.some)
      case None =>
        any.asInstanceOf[Matchable] match {
          case iterable: IterableOnce[?] if !iterable.isInstanceOf[Option[?]] =>
            val pairs = Seq.from(iterable).zipWithIndex
            val maxIdxStrLen = pairs.map(_._2.toString.length).maxOption.getOrElse(0)
            s"${color}Seq[_]$RESET:" +
              pairs
                .map { case (value, i) => "\n" + showGeneric(value, color, show).prefixAndIndent(s"$MAGENTA${i.toString.alignRight(maxIdxStrLen)}$RESET: ") }
                .mkString
                .replaceAll("\n", s"\n$color|$RESET   ")
          case product: Product =>
            val pairs = product.productElementNames.zip(product.productIterator).toSeq
            val maxFieldStrLen = pairs.map(_._1.length).maxOption.getOrElse(0)
            color + product.productPrefix + RESET + ":" +
              pairs
                .map { case (field, value) => "\n" + showGeneric(value, color, show).prefixAndIndent(s"$MAGENTA${field.alignRight(maxFieldStrLen)}$RESET: ") }
                .mkString
                .replaceAll("\n", s"\n$color|$RESET   ")
          case _ =>
            Value.Actual(any).showPretty(color, _.toString.some)
        }
    }
  def showGeneric(any: Any, color: String)(show: PartialFunction[Matchable, String]): String =
    showGeneric(any, color, any => show.lift(any.asInstanceOf[Matchable]))
  def showGeneric(any: Any, color: String): String =
    showGeneric(any, color) { defaultShow }

  private object seqNotOption {
    def unapply(any: Matchable): Option[Seq[?]] = any match
      case _: Option[?]         => None
      case seq: IterableOnce[?] => Seq.from(seq).some
      case seq: Array[?]        => seq.toSeq.some
      case _                    => None
  }

  private def diffAnyFiltered(expected: Any, actual: Any, show: Any => Option[String]): Option[String] =
    if expected == actual then None
    else
      (expected.asInstanceOf[Matchable], actual.asInstanceOf[Matchable]) match {
        case (seqNotOption(expected), seqNotOption(actual)) =>
          val diffs =
            0.until(expected.length max actual.length)
              .map { i =>
                (
                  i,
                  (expected.lift(i), actual.lift(i)) match {
                    case (Some(expected), Some(actual)) => diffAnyFiltered(Value.Actual(expected), Value.Actual(actual), show)
                    case (Some(expected), None)         => expectedActual(Value.Actual(expected), Value.MissingIndex, show).some
                    case (None, Some(actual))           => expectedActual(Value.MissingIndex, Value.Actual(actual), show).some
                    case (None, None)                   => ??? // not possible
                  },
                )
              }
              .flatMap { case (i, diff) => diff.map((i, _)) }

          Option.when(diffs.nonEmpty) {
            val maxIdxStrLen = diffs.map(_._1.toString.length).max
            s"${RED}Seq[_]$RESET:" +
              diffs.map { case (i, diff) => "\n" + diff.prefixAndIndent(s"$MAGENTA${i.toString.alignRight(maxIdxStrLen)}$RESET: ") }.mkString.replaceAll("\n", s"\n$RED|$RESET   ")
          }
        case (expected: Product, actual: Product) if expected.getClass == actual.getClass =>
          val diffs =
            expected.productElementNames
              .zip(expected.productIterator)
              .zip(actual.productIterator)
              .flatMap { case ((field, expected), actual) => diffAnyFiltered(expected, actual, show).map((field, _)) }
              .toSeq

          Option.when(diffs.nonEmpty) {
            val maxFieldStrLen = diffs.map(_._1.length).max
            RED + expected.productPrefix + RESET + ":" +
              diffs
                .map { case (field, diff) => "\n" + diff.prefixAndIndent(s"$MAGENTA${field.alignRight(maxFieldStrLen)}$RESET: ") }
                .mkString
                .replaceAll("\n", s"\n$RED|$RESET   ")
          }
        case _ =>
          expectedActual(Value.Actual(expected), Value.Actual(actual), show).some
      }

  def equalTo_filteredDiff[A](expected: A)(partialShow: PartialFunction[Matchable, String]): Assertion[A] =
    Assertion(
      TestArrow.make[A, Boolean] { actual =>
        val show: Any => Option[String] = any => partialShow.lift(any.asInstanceOf[Matchable])

        TestTrace.boolean(expected == actual) {
          diffAnyFiltered(expected, actual, show) match {
            case Some(message) => ErrorMessage.text(s"diff:$RESET\n${message.split('\n').map(s => s"\n$YELLOW>$RESET                $s").mkString}\n")
            case None          => ErrorMessage.text(expectedActual(Value.Actual(expected), Value.Actual(actual), show))
          }
        }
      },
    )
  def equalTo_filteredDiff[A](expected: A): Assertion[A] =
    equalTo_filteredDiff(expected) { defaultShow }

  private def diffAnyUnfiltered(expected: Any, actual: Any, show: Any => Option[String]): String =
    (expected.asInstanceOf[Matchable], actual.asInstanceOf[Matchable]) match {
      case (seqNotOption(expected), seqNotOption(actual)) =>
        val diffs =
          0.until(expected.length max actual.length).map { i =>
            (
              i,
              (expected.lift(i), actual.lift(i)) match {
                case (Some(expected), Some(actual)) => diffAnyUnfiltered(expected, actual, show)
                case (Some(expected), None)         => expectedActual(Value.Actual(expected), Value.MissingIndex, show)
                case (None, Some(actual))           => expectedActual(Value.MissingIndex, Value.Actual(actual), show)
                case (None, None)                   => ??? // not possible
              },
            )
          }

        val maxIdxStrLen = diffs.map(_._1.toString.length).maxOption.getOrElse(0)
        val color = if expected == actual then GREEN else RED
        s"${color}Seq[_]$RESET:" +
          diffs.map { case (i, diff) => "\n" + diff.prefixAndIndent(s"$MAGENTA${i.toString.alignRight(maxIdxStrLen)}$RESET: ") }.mkString.replaceAll("\n", s"\n$color|$RESET   ")
      case (expected: Product, actual: Product) if expected.getClass == actual.getClass =>
        val diffs =
          expected.productElementNames
            .zip(expected.productIterator)
            .zip(actual.productIterator)
            .map { case ((field, expected), actual) => (field, diffAnyUnfiltered(expected, actual, show)) }
            .toSeq

        val maxFieldStrLen = diffs.map(_._1.length).maxOption.getOrElse(0)
        val color = if expected == actual then GREEN else RED
        color + expected.productPrefix + RESET + ":" +
          diffs
            .map { case (field, diff) => "\n" + diff.prefixAndIndent(s"$MAGENTA${field.alignRight(maxFieldStrLen)}$RESET: ") }
            .mkString
            .replaceAll("\n", s"\n$color|$RESET   ")
      case (expected, actual) if expected == actual =>
        showGeneric(expected, GREEN, show)
      case _ =>
        expectedActual(Value.Actual(expected), Value.Actual(actual), show)
    }

  def equalTo_unfilteredDiff[A](expected: A)(partialShow: PartialFunction[Matchable, String]): Assertion[A] =
    Assertion(
      TestArrow.make[A, Boolean] { actual =>
        val show: Any => Option[String] = any => partialShow.lift(any.asInstanceOf[Matchable])

        TestTrace.boolean(expected == actual) {
          ErrorMessage.text(s"diff:$RESET\n${diffAnyUnfiltered(expected, actual, show).split('\n').map(s => s"\n$YELLOW>$RESET                $s").mkString}\n")
        }
      },
    )
  def equalTo_unfilteredDiff[A](expected: A): Assertion[A] =
    equalTo_unfilteredDiff(expected) { defaultShow }

}
