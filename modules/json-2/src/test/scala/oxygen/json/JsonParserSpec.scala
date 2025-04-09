package oxygen.json

import oxygen.predef.test.*
import zio.test.{check, TestAspect}

object JsonParserSpec extends OxygenSpecDefault {

  @scala.annotation.unused
  private def makeTest(input: String)(exp: Json)(using SourceLocation): TestSpec =
    test(input) {
      val start = java.lang.System.nanoTime()
      val res = Json.parse(input)
      val end = java.lang.System.nanoTime()
      showStats(input, exp, end - start)
      assert(res)(isRight(equalTo_filteredDiff(exp)))
    }

  private def showStats(input: String, exp: Json, duration: Long): Unit = {
    val ne = numElems(exp)
    println(s"length = ${input.length}, numElems = $ne, duration = $duration, duration/numElems = ${duration / ne}")
  }

  private def numElems(json: Json): Int = json match
    case Json.Arr(value) => 1 + value.map(numElems).sum
    case Json.Obj(value) => 1 + value.map { case (_, v) => numElems(v) }.sum
    case _               => 1

  override def testSpec: TestSpec =
    suite("JsonParserSpec")(
      test("random") {
        check(Generators.strings.anyJsonGen(10)) { case (json, input) =>
          val start = java.lang.System.nanoTime()
          val res = Json.parse(input)
          val end = java.lang.System.nanoTime()

          res.leftMap {
            case JsonError(_, JsonError.Cause.InvalidJson(idx)) =>
              if (idx < input.length) {
                val (a, _) = input.splitAt(idx)
                println("lineNo = " + a.count(_ == '\n'))
              } else
                println("EOF?")
            case _ =>
              ???
          }

          if (res.isLeft) {
            println()
            println()
            println()
            println(input)
            println()
            println()
            println()
          }

          showStats(input, json, end - start)
          assert(res)(isRight(equalTo_filteredDiff(json)))
        }
      } @@ TestAspect.samples(100) @@ TestAspect.nondeterministic,
    )

}
