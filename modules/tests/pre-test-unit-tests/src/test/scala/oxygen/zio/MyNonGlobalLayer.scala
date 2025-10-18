package oxygen.zio

import oxygen.test.*
import zio.*

object MyNonGlobalLayer {

  private val initCountRef: Ref[Int] = Unsafe.unsafely { Ref.unsafe.make(0) }
  private val allValuesRef: Ref[Set[String]] = Unsafe.unsafely { Ref.unsafe.make(Set.empty[String]) }

  val initCount: UIO[Int] = initCountRef.get
  val allValues: UIO[Set[String]] = allValuesRef.get

  val layer: Layer[Nothing, String] =
    ZLayer { initCountRef.update(_ + 1) *> RandomGen.lowerCaseString().tap(s => allValuesRef.update(_ + s)) }

}
