package oxygen.core.collection.mutable

import oxygen.predef.test.*
import scala.reflect.ClassTag
import zio.test.TestResult

object LinkedListSpec extends OxygenSpecDefault {

  // override def defaultLogLevel: LogLevel = LogLevel.Debug

  private enum Insert[A] {
    case GenPrependSingle(value: A)
    case GenAppendSingle(value: A)
    case GenPrependIterable(value: Seq[A])
    case GenReversePrependIterable(value: Seq[A])
    case GenAppendIterable(value: Seq[A])

    final def toList: (List[A], List[A]) = this match
      case Insert.GenPrependSingle(value)          => (value :: Nil, Nil)
      case Insert.GenAppendSingle(value)           => (Nil, value :: Nil)
      case Insert.GenPrependIterable(value)        => (value.toList, Nil)
      case Insert.GenReversePrependIterable(value) => (value.reverse.toList, Nil)
      case Insert.GenAppendIterable(value)         => (Nil, value.toList)

    override final def toString: String = this match
      case Insert.GenPrependSingle(value)          => "Prepend Single : " + value.toString
      case Insert.GenAppendSingle(value)           => "Append Single : " + value.toString
      case Insert.GenPrependIterable(value)        => "Prepend Many : " + value.toString
      case Insert.GenReversePrependIterable(value) => "Prepend Many - Reversed : " + value.toString
      case Insert.GenAppendIterable(value)         => "Append Many : " + value.toString

  }

  private def genChunk[A](gen: UIO[A], genSizeMin: Int, genSizeMax: Int): UIO[Chunk[A]] =
    Random.nextIntBetween(genSizeMin, genSizeMax).flatMap { size => gen.replicateZIO(size).map(Chunk.from) }

  private def genCase[A](gen: UIO[A], genSizeMin: Int, genSizeMax: Int): UIO[Insert[A]] =
    Random.nextIntBounded(5).flatMap {
      case 0 => genChunk[A](gen, genSizeMin, genSizeMax).map { chunk => Insert.GenPrependIterable(chunk) }
      case 1 => genChunk[A](gen, genSizeMin, genSizeMax).map { chunk => Insert.GenReversePrependIterable(chunk) }
      case 2 => genChunk[A](gen, genSizeMin, genSizeMax).map { chunk => Insert.GenAppendIterable(chunk.toList) }
      case 3 => gen.map(Insert.GenPrependSingle(_))
      case 4 => gen.map(Insert.GenAppendSingle(_))
      case _ => ???
    }

  private final case class Snapshots[A](
      insertIdx: Int,
      insert: Insert[A],
      beforeGolden: List[A],
      beforeSnapshot: Iterable[A],
      beforeEvaluated: List[A],
      afterGolden: List[A],
      afterSnapshot: Iterable[A],
      afterEvaluated: List[A],
  ) {

    def runAssertions: Task[TestResult] =
      ZIO.attempt {
        (
          simpleEqual(beforeSnapshot.toList, beforeGolden) &&
            simpleEqual(beforeEvaluated, beforeGolden) &&
            simpleEqual(afterSnapshot.toList, afterGolden) &&
            simpleEqual(afterEvaluated, afterGolden)
        ).label(s"idx=$insertIdx")
      }

  }

  private object Snapshots {

    def make[A](builder: LinkedList[A], beforeGolden: List[A], insert: Insert[A], insertIdx: Int): Task[Snapshots[A]] =
      for {
        beforeSnapshot <- ZIO.attempt { builder.snapshot() }
        beforeEvaluated <- ZIO.attempt { beforeSnapshot.toList }
        _ <- ZIO.attempt {
          insert match {
            case Insert.GenPrependSingle(value)          => builder.prepend(value)
            case Insert.GenAppendSingle(value)           => builder.append(value)
            case Insert.GenPrependIterable(value)        => builder.prependAll(value)
            case Insert.GenReversePrependIterable(value) => builder.prependAllInReverse(value)
            case Insert.GenAppendIterable(value)         => builder.appendAll(value)
          }
        }
        afterSnapshot <- ZIO.attempt { builder.snapshot() }
        afterEvaluated <- ZIO.attempt { afterSnapshot.toList }

        (gBefore, gAfter) = insert.toList
        afterGolden = gBefore ++ beforeGolden ++ gAfter
        _ <- ZIO.logDebug(
          s"""Insert #$insertIdx : $insert
             |
             |=====| Before |=====
             |
             |--- Golden : Before ---
             |${beforeGolden.mkString("[", ", ", "]")}
             |--- Elements : Before ---
             |${beforeEvaluated.mkString("[", ", ", "]")}
             |
             |=====| Before |=====
             |
             |--- Golden : After ---
             |${afterGolden.mkString("[", ", ", "]")}
             |--- Elements : After ---
             |${afterEvaluated.mkString("[", ", ", "]")}
             |""".stripMargin,
        )
      } yield Snapshots(
        insertIdx = insertIdx,
        insert = insert,
        beforeGolden = beforeGolden,
        beforeSnapshot = beforeSnapshot,
        beforeEvaluated = beforeEvaluated,
        afterGolden = afterGolden,
        afterSnapshot = afterSnapshot,
        afterEvaluated = afterEvaluated,
      )

  }

  private def makeTest[A: ClassTag as ct](
      idx: Int,
      gen: UIO[A],
      genSizeMin: Int,
      genSizeMax: Int,
      numInserts: Int,
  )(using Trace, SourceLocation): TestSpec =
    test(s"[$idx] ${ct.runtimeClass.getName} : $genSizeMin, $genSizeMax, $numInserts") {
      for {
        inserts: Seq[Insert[A]] <- genCase[A](gen, genSizeMin, genSizeMax).replicateZIO(numInserts).map(_.toSeq)
        builder <- ZIO.attempt { LinkedList.empty[A] }
        (rAcc, _) <- ZIO.foldLeft(inserts.zipWithIndex)((List.empty[Snapshots[A]], List.empty[A])) { case ((rAcc, golden), (insert, idx)) =>
          Snapshots.make(builder, golden, insert, idx).map { snap => (snap :: rAcc, snap.afterGolden) }
        }
        asserts <- ZIO.foreach(rAcc.reverse)(_.runAssertions)
        result = asserts.foldLeft(assertCompletes) { (acc, elem) =>
          if acc.isSuccess then acc && elem
          else acc
        }
      } yield result
    }

  private def makeSuite[A: ClassTag as ct](
      gen: UIO[A],
      genSizeMin: Int,
      genSizeMax: Int,
      numInserts: Int,
      numTests: Int,
  )(using Trace, SourceLocation): TestSpec =
    suite(s"${ct.runtimeClass.getName}, $genSizeMin, $genSizeMax, $numInserts")(
      1.to(numTests).map(makeTest(_, gen, genSizeMin, genSizeMax, numInserts))*,
    )

  override def testSpec: TestSpec =
    suite("LinkedListSpec")(
      makeSuite[Int](Random.nextInt, 1, 4, 10, 4),
      makeSuite[Int](Random.nextInt, 16, 128, 10, 4),
      makeSuite[String](RandomGen.lowerCaseString(), 1, 64, 10, 4),
    )

  override def testAspects: Chunk[TestSpecAspect] = Chunk(TestAspect.nondeterministic)

}
