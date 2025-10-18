package oxygen.zio

import oxygen.predef.test.*

object MyNonGlobalLayerSpec extends OxygenSpec[String] {

  private def makeTest(i: Int)(using Trace, SourceLocation): TestSpec =
    test(s"test #$i") {
      for {
        value <- ZIO.service[String]
        allValues <- MyNonGlobalLayer.allValues
        initCount <- MyNonGlobalLayer.initCount
      } yield assertTrue(
        allValues.contains(value),
        initCount == i,
      )
    }

  override def testSpec: TestSpec =
    suite("MyNonGlobalLayerSpec")(
      makeTest(1),
      makeTest(2),
      makeTest(3),
      makeTest(4),
      makeTest(5),
    )

  override def testAspects: Chunk[TestSpecAspect] = Chunk(TestAspect.sequential)

  override def layerProvider: LayerProvider[Env] =
    LayerProvider.providePerTest[Env](
      MyNonGlobalLayer.layer,
    )

}
