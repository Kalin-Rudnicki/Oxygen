package oxygen.zio

import oxygen.predef.test.*

object MyGlobalLayerSpec extends OxygenSpec[String] {

  private def makeTest(i: Int)(using Trace, SourceLocation): TestSpec =
    test(s"test #$i") {
      for {
        value <- ZIO.service[String]
        allValues <- MyGlobalLayer.allValues
        initCount <- MyGlobalLayer.initCount
      } yield assertTrue(
        allValues == Set(value),
        initCount == 1,
      )
    }

  override def testSpec: TestSpec =
    suite("MyGlobalLayerSpec")(
      makeTest(1),
      makeTest(2),
      makeTest(3),
      makeTest(4),
      makeTest(5),
    )

  override def testAspects: Chunk[TestSpecAspect] = Chunk(TestAspect.sequential)

  override def layerProvider: LayerProvider[Env] =
    LayerProvider.providePerTest[Env](
      MyGlobalLayer.layer,
    )

}
