package oxygen.meta

import oxygen.predef.test.*

object DerivedFromAndToExprSpec extends OxygenSpecDefault {

  override def testSpec: TestSpec =
    suite("DerivedFromAndToExprSpec")(
      suite("ToExpr")(
        test("product") {
          val exp = Macros.ProductToExprExample("hi", true, Macros.InnerExample('_').some)
          val act = Macros.productToExprExample("hi", true, '_')
          assertTrue(exp == act)
        },
        test("sum") {
          val exp = (Macros.SumToExprExample.Case1("hi", true.some, Macros.InnerExample('_')), Macros.SumToExprExample.Case2(5), Macros.SumToExprExample.Case3)
          val act = Macros.sumToExprExample("hi", true, '_', 5)
          assertTrue(exp == act)
        },
      ),
      suite("FromExpr")(
        test("product 1") {
          assertTrue(
            Macros.productRoundTrip(Macros.ProductToExprExample("hi", true, Macros.InnerExample('_').some)) ==
              Macros.ProductToExprExample("hi", true, Macros.InnerExample('_').some),
          )
        },
        test("product 2") {
          assertTrue(
            Macros.productRoundTrip(new Macros.ProductToExprExample("hi", true, Macros.InnerExample('_').some)) ==
              Macros.ProductToExprExample("hi", true, Macros.InnerExample('_').some),
          )
        },
        test("sum 1") {
          assertTrue(
            Macros.sumRoundTrip(Macros.SumToExprExample.Case1("hi", Some(true), Macros.InnerExample('_'))) ==
              Macros.SumToExprExample.Case1("hi", Some(true), Macros.InnerExample('_')),
          )
        },
        test("sum 2") {
          assertTrue(
            Macros.sumRoundTrip(Macros.SumToExprExample.Case2(5)) ==
              Macros.SumToExprExample.Case2(5),
          )
        },
        test("sum 3") {
          assertTrue(
            Macros.sumRoundTrip(Macros.SumToExprExample.Case3) ==
              Macros.SumToExprExample.Case3,
          )
        },
      ),
    )

}
