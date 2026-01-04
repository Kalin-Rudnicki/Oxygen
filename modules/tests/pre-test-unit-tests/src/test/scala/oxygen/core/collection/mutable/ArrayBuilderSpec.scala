package oxygen.core.collection.mutable

import oxygen.predef.test.*
import scala.reflect.ClassTag

object ArrayBuilderSpec extends OxygenSpecDefault {

  private enum GenCase[A] {
    case GenArray(value: Array[A])
    case GenIterable(value: Iterable[A])
    case GenIterator(value: Iterable[A])
    case GenSingle(value: A)
  }

  private def genChunk[A](gen: UIO[A], genSizeMin: Int, genSizeMax: Int): UIO[Chunk[A]] =
    Random.nextIntBetween(genSizeMin, genSizeMax).flatMap { size => gen.replicateZIO(size).map(Chunk.from) }

  private def genCase[A: ClassTag](gen: UIO[A], genSizeMin: Int, genSizeMax: Int): UIO[GenCase[A]] =
    Random.nextIntBounded(4).flatMap {
      case 0 => ???
      case 1 => ???
      case 2 => ???
      case 3 => ???
      case _ => ???
    }

  private def makeSuite[A: ClassTag](gen: UIO[A], initialSizeMin: Int, initialSizeMax: Int, genSizeMin: Int, genSizeMax: Int, numTests: Int): TestSpec =
    ??? // FIX-PRE-MERGE (KR) :

  override def testSpec: TestSpec =
    suite("ArrayBuilderSpec")(
      // FIX-PRE-MERGE (KR) :
    )

}
