package oxygen.core

import oxygen.predef.test.*

object LazyStringSpec extends OxygenSpecDefault {

  private val strs: Seq[LazyString] =
    Seq(
      LazyString.fromString(""),
      LazyString.fromString("hello"),
      LazyString.fromString("hello\nthere"),
      LazyString.fromString("hello\nthere") |> "my child" |> "and grandchild",
    )

  strs.foreach { str =>
    println()
    println("... ... ...")
    println(str.buildNowSimple(LazyString.Config.make(ColorMode.Extended, "|   ")))
    println("...")
    println(str.buildNowSimple(LazyString.Config.make(ColorMode.Extended, ">   ", Color.Named.Red, Color.RGB.hex("#ff64a0"))))
    println("...")
    println(str.buildNowSimple(LazyString.Config.make(ColorMode.Colorless, ">   ", Color.Named.Red, Color.RGB.hex("#ff64a0"))))
  }

  override def testSpec: TestSpec =
    suite("LazyStringSpec")(
      // FIX-PRE-MERGE (KR) :
    )

}
