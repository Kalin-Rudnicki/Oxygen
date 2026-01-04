package oxygen.core.collection.mutable

import oxygen.predef.test.*
import scala.reflect.ClassTag
import zio.test.TestResult

object ArrayBuilderSpec extends OxygenSpecDefault {

  override def defaultLogLevel: LogLevel = LogLevel.Debug

  private enum Insert[A] {
    case GenArray(value: Array[A])
    case GenIterable(value: Iterable[A])
    case GenIterator(value: Iterable[A])
    case GenSingle(value: A)

    final def toList: List[A] = this match
      case Insert.GenArray(value)    => value.toList
      case Insert.GenIterable(value) => value.toList
      case Insert.GenIterator(value) => value.toList
      case Insert.GenSingle(value)   => value :: Nil

    override final def toString: String = this match
      case Insert.GenArray(value)    => value.mkString("Array: [", ", ", "]")
      case Insert.GenIterable(value) => "Iterable: " + value.toString
      case Insert.GenIterator(value) => "Iterator: " + value.toString
      case Insert.GenSingle(value)   => "Single: " + value.toString

  }

  private def genChunk[A](gen: UIO[A], genSizeMin: Int, genSizeMax: Int): UIO[Chunk[A]] =
    Random.nextIntBetween(genSizeMin, genSizeMax).flatMap { size => gen.replicateZIO(size).map(Chunk.from) }

  private def genCase[A: ClassTag](gen: UIO[A], genSizeMin: Int, genSizeMax: Int): UIO[Insert[A]] =
    Random.nextIntBounded(7).flatMap {
      case 0 => genChunk[A](gen, genSizeMin, genSizeMax).map { chunk => Insert.GenArray(chunk.toArray[A]) }
      case 1 => genChunk[A](gen, genSizeMin, genSizeMax).map { chunk => Insert.GenIterable(chunk) }
      case 2 => genChunk[A](gen, genSizeMin, genSizeMax).map { chunk => Insert.GenIterable(chunk.toList) }
      case 3 => genChunk[A](gen, genSizeMin, genSizeMax).map { chunk => Insert.GenIterable(chunk.toVector) }
      case 4 => genChunk[A](gen, genSizeMin, genSizeMax).map { chunk => Insert.GenIterable(ArraySeq.from(chunk)) }
      case 5 => genChunk[A](gen, genSizeMin, genSizeMax).map { chunk => Insert.GenIterator(chunk) }
      case 6 => gen.map(Insert.GenSingle(_))
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
        assertTrue(
          beforeSnapshot == beforeGolden,
          beforeEvaluated == beforeGolden,
          afterSnapshot == afterGolden,
          afterEvaluated == afterGolden,
        ).label(s"idx=$insertIdx")
      }

  }
  private object Snapshots {

    def make[A](builder: ArrayBuilder[A], beforeGolden: List[A], insert: Insert[A], insertIdx: Int): Task[Snapshots[A]] =
      for {
        beforeInternalState <- ZIO.attempt { builder.showInternalState() }
        beforeSnapshot <- ZIO.attempt { builder.snapshot() }
        beforeEvaluated <- ZIO.attempt { beforeSnapshot.toList }
        _ <- ZIO.attempt {
          insert match {
            case Insert.GenArray(value)    => builder.addAllArrayElements(value)
            case Insert.GenIterable(value) => builder.addAllIterable(value)
            case Insert.GenIterator(value) => builder.addAllIterator(value.iterator)
            case Insert.GenSingle(value)   => builder.addSingle(value)
          }
        }
        afterInternalState <- ZIO.attempt { builder.showInternalState() }
        afterSnapshot <- ZIO.attempt { builder.snapshot() }
        afterEvaluated <- ZIO.attempt { afterSnapshot.toList }

        afterGolden = beforeGolden ++ insert.toList
        _ <- ZIO.logDebug(
          s"""Insert #$insertIdx : $insert
             |
             |=====| Before |=====
             |
             |--- Golden : Before ---
             |${beforeGolden.mkString("[", ", ", "]")}
             |--- Elements : Before ---
             |${beforeEvaluated.mkString("[", ", ", "]")}
             |--- Internal State : Before ---
             |$beforeInternalState
             |
             |=====| Before |=====
             |
             |--- Golden : After ---
             |${afterGolden.mkString("[", ", ", "]")}
             |--- Elements : After ---
             |${afterEvaluated.mkString("[", ", ", "]")}
             |--- Internal State : After ---
             |$afterInternalState
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
      initialSizeMin: Int,
      initialSizeMax: Int,
      genSizeMin: Int,
      genSizeMax: Int,
      numInserts: Int,
  )(using Trace, SourceLocation): TestSpec =
    test(s"[$idx] ${ct.runtimeClass.getName}, $initialSizeMin, $initialSizeMax, $genSizeMin, $genSizeMax, $numInserts") {
      for {
        inserts: Seq[Insert[A]] <- genCase[A](gen, genSizeMin, genSizeMax).replicateZIO(numInserts).map(_.toSeq)
        initialSize <- Random.nextIntBetween(initialSizeMin, initialSizeMax)
        builder <- ZIO.attempt { ArrayBuilder.emptyThreadUnsafe[A](initialSize) }
        (rAcc, _) <- ZIO.foldLeft(inserts.zipWithIndex)((List.empty[Snapshots[A]], List.empty[A])) { case ((rAcc, golden), (insert, idx)) =>
          Snapshots.make(builder, golden, insert, idx).map { snap => (snap :: rAcc, snap.afterGolden) }
        }
        asserts <- ZIO.foreach(rAcc)(_.runAssertions)
      } yield TestResult.allSuccesses(asserts)
    }

  private def makeSuite[A: ClassTag as ct](
      gen: UIO[A],
      initialSizeMin: Int,
      initialSizeMax: Int,
      genSizeMin: Int,
      genSizeMax: Int,
      numInserts: Int,
      numTests: Int,
  )(using Trace, SourceLocation): TestSpec =
    suite(s"${ct.runtimeClass.getName}, $initialSizeMin, $initialSizeMax, $genSizeMin, $genSizeMax, $numInserts")(
      1.to(numTests).map(makeTest(_, gen, initialSizeMin, initialSizeMax, genSizeMin, genSizeMax, numInserts))*,
    )

  override def testSpec: TestSpec =
    suite("ArrayBuilderSpec")( // FIX-PRE-MERGE (KR) :
      makeSuite[Int](Random.nextInt, 8, 16, 1, 4, 1, 3),
      // makeSuite[Int](Random.nextInt, 16, 32, 2, 128, 4, 4),
      // makeSuite[String](RandomGen.lowerCaseString(), 1, 1, 32, 64, 4, 4),
    )

}
