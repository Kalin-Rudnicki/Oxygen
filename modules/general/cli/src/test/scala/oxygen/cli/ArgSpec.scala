package oxygen.cli

import oxygen.predef.test.*

object ArgSpec extends OxygenSpecDefault {

  private def passingSpec(name: String)(args: String*)(exp: Arg*)(using SourceLocation): TestSpec =
    test(name) {
      assert(Arg.parse(args.toList).map { case (a, p) => a ::: p })(isRight(equalTo(exp.toList)))
    }

  private def splitOnTest(name: String)(inputs: String*)(expLeft: List[String], expRight: List[String])(using SourceLocation): TestSpec =
    test(name) {
      val (left, right) = Arg.splitOn_--(inputs.toList)
      assertTrue(
        left == expLeft,
        right == expRight,
      )
    }

  override def testSpec: TestSpec =
    suite("ArgSpec")(
      suite("parse")(
        suite("passes")(
          passingSpec("empty")()(),
          passingSpec("args only")("arg-1", "arg-2", "arg-3")(
            Arg.Value(0, "arg-1"),
            Arg.Value(1, "arg-2"),
            Arg.Value(2, "arg-3"),
          ),
          passingSpec("params only")("--param-1", "--param-2", "--param-3")(
            Arg.ScopedParam(0, LongName("param-1"), Nil),
            Arg.ScopedParam(1, LongName("param-2"), Nil),
            Arg.ScopedParam(2, LongName("param-3"), Nil),
          ),
          passingSpec("params with args")("--param-1", "--param-2", "arg 2.1", "--param-3", "arg 3.1", "arg 3.2")(
            Arg.ScopedParam(0, LongName("param-1"), List()),
            Arg.ScopedParam(1, LongName("param-2"), List(Arg.Value(2, "arg 2.1"))),
            Arg.ScopedParam(3, LongName("param-3"), List(Arg.Value(4, "arg 3.1"), Arg.Value(5, "arg 3.2"))),
          ),
          passingSpec("params with brackets")("--param-1", "arg-1", "{", "arg-2", "--param-2", "}", "arg-3")(
            Arg.ScopedParam(
              0,
              LongName("param-1"),
              List(
                Arg.Value(1, "arg-1"),
                Arg.Bracketed(
                  2,
                  Arg.Value(3, "arg-2") :: Nil,
                  Arg.ScopedParam(4, LongName("param-2"), Nil) :: Nil,
                ),
                Arg.Value(6, "arg-3"),
              ),
            ),
          ),
        ),
        suite("fails")(
          // TODO (KR) :
        ),
      ),
      suite("splitOn_--")(
        splitOnTest("empty")()(Nil, Nil),
        splitOnTest("left only (explicit)")("a", "b", "c", "--")(List("a", "b", "c"), Nil),
        splitOnTest("right only (explicit)")("--", "a", "b", "c")(Nil, List("a", "b", "c")),
        splitOnTest("right only (using)")("a", "b", "c")(Nil, List("a", "b", "c")),
        splitOnTest("left and right")("a", "b", "c", "--", "d", "e", "f")(List("a", "b", "c"), List("d", "e", "f")),
      ),
    )

}
