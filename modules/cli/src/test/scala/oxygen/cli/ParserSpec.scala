package oxygen.cli

import oxygen.predef.test.*

object ParserSpec extends OxygenSpecDefault {

  private def makePassingTest[A](name: String)(parser: Parser[A], exp: A)(args: String*)(implicit loc: SourceLocation): TestSpec =
    test(name) {
      Arg.parse(args.toList) match {
        case Right((values, params)) =>
          parser.parse(values, params) match {
            case Parser.ParseResult.Success(value, _, _, _) => assertTrue(value == exp)
            case Parser.ParseResult.Fail(_, help) =>
              val fullHelp = parser.build.toOption.get.helpMessage.removeEmpties
              throw new RuntimeException(s"\n\n\n$fullHelp\n\n\n$help\n\n\n")
          }
        case Left(error) => throw new RuntimeException(s"Unable to parse args: $error")
      }
    }

  final case class Person(
      firstName: String,
      lastName: String,
  )
  object Person {
    val valueParser: Values[Person] = (Values.value[String]("first-name") ^>> Values.value[String]("last-name")).map(Person.apply)
    val paramParser: Params[Person] =
      (
        Params.value[String]("first-name", hints = List("The persons first name", "(obviously)")) &&
          Params.value[String]("last-name", hints = List("The persons last name", "(obviously)"))
      ).map(Person.apply)
  }

  final case class ExampleConfig(
      labels: List[String],
      maxWidth: Int,
  )
  object ExampleConfig {

    val parser: Parser[ExampleConfig] =
      (
        Values.value[String]("label").repeated ^>>
          Params.value[Int]("max-width").withDefault(100)
      ).map(ExampleConfig.apply)

    val parserWithKey: Params[ExampleConfig] =
      Params.valueWith("config")(parser.bracketed("config").withDefault(ExampleConfig(Nil, 100)))

  }

  val booleanParams: Params[(Boolean, Boolean)] =
    Params.toggle.prefixFalse("no", "bool-a", shortName = Defaultable.Some(BooleanShortName('A', 'a'))) &&
      Params.toggle.prefixFalse("no", "bool-b", shortName = Defaultable.Some(BooleanShortName('B', 'b')))

  private val passesSpec: TestSpec =
    suite("passes")(
      makePassingTest("empty")(Parser.Empty, ())(),
      suite("values")(
        makePassingTest("simple value")(Values.value[Int]("int"), 5)("5"),
        makePassingTest("optional value - some")(Values.value[Int]("int").optional, 5.some)("5"),
        makePassingTest("optional value - none")(Values.value[Int]("int").optional, None)(),
        makePassingTest("then")(Values.value[Int]("int") ^>> Values.value[Float]("float") ^>> Values.value[Boolean]("boolean"), (5, 10.5f, true))("5", "10.5", "true"),
        makePassingTest("repeated value - list 0")(Values.value[Int]("int").repeated, List())(),
        makePassingTest("repeated value - list 1")(Values.value[Int]("int").repeated, List(1))("1"),
        makePassingTest("repeated value - list 3")(Values.value[Int]("int").repeated, List(1, 2, 3))("1", "2", "3"),
        makePassingTest("repeated value - nel 1")(Values.value[Int]("int").repeatedNel, NonEmptyList.of(1))("1"),
        makePassingTest("repeated value - nel 3")(Values.value[Int]("int").repeatedNel, NonEmptyList.of(1, 2, 3))("1", "2", "3"),
        makePassingTest("or - left")(Values.value[Int]("int") <||> Values.value[Boolean]("boolean"), 5.asLeft)("5"),
        makePassingTest("or - right")(Values.value[Int]("int") <||> Values.value[Boolean]("boolean"), false.asRight)("false"),
        makePassingTest("repeated + then")(Values.value[Int]("int").repeated ^>> Values.value[Boolean]("boolean"), (List(1, 2, 3), true))("1", "2", "3", "true"),
        makePassingTest("person")(Person.valueParser, Person("first", "last"))("first", "last"),
        makePassingTest("person - repeated")(Person.valueParser.repeated, List(Person("first-1", "last-1"), Person("first-2", "last-2")))("first-1", "last-1", "first-2", "last-2"),
      ),
      suite("params")(
        makePassingTest("simple param - 2 args")(Params.value[Int]("int"), 5)("--int", "5"),
        makePassingTest("simple param - 1 arg with =")(Params.value[Int]("int"), 5)("--int=5"),
        makePassingTest("boolean flag - present")(Params.flag("flag"), true)("--flag"),
        makePassingTest("boolean flag - missing")(Params.flag("flag"), false)(),
        makePassingTest("toggle - prefix true - true")(Params.toggle.prefixTrue("true", "base"), true)("--true-base"),
        makePassingTest("toggle - prefix true - false")(Params.toggle.prefixTrue("true", "base"), false)("--base"),
        makePassingTest("toggle - prefix false - true")(Params.toggle.prefixFalse("false", "base"), true)("--base"),
        makePassingTest("toggle - prefix false - false")(Params.toggle.prefixFalse("false", "base"), false)("--false-base"),
        makePassingTest("toggle - prefix false - true")(Params.toggle.prefixBoth("true", "false", "base"), true)("--true-base"),
        makePassingTest("toggle - prefix false - false")(Params.toggle.prefixBoth("true", "false", "base"), false)("--false-base"),
        makePassingTest("valueWith - list 0")(Params.valueWith("ints")(Values.value[Int]("int").repeated), List())("--ints"),
        makePassingTest("valueWith - list 1 - 1 arg with =")(Params.valueWith("ints")(Values.value[Int]("int").repeated), List(5))("--ints=5"),
        makePassingTest("valueWith - list 1 - 2 args")(Params.valueWith("ints")(Values.value[Int]("int").repeated), List(5))("--ints", "5"),
        makePassingTest("valueWith - list 3")(Params.valueWith("ints")(Values.value[Int]("int").repeated), List(1, 2, 3))("--ints", "1", "2", "3"),
        makePassingTest("person")(Params.valueWith("ints")(Values.value[Int]("int").repeated), List(1, 2, 3))("--ints", "1", "2", "3"),
        makePassingTest("multi-short-booleans - 1")(booleanParams, (true, true))("-AB"),
        makePassingTest("multi-short-booleans - 2")(booleanParams, (true, true))("-BA"),
        makePassingTest("multi-short-booleans - 3")(booleanParams, (false, true))("-Ba"),
      ),
      suite("bracketed")(
        makePassingTest("bracketed person")(Person.paramParser.bracketed("person"), Person("f-1", "l-1"))("{", "--first-name=f-1", "--last-name=l-1", "}"),
        makePassingTest("bracketed person - repeated")(Person.paramParser.bracketed("person").repeated(false), List(Person("f-1", "l-1"), Person("f-2", "l-2")))(
          "{",
          "--first-name",
          "f-1",
          "--last-name=l-1",
          "}",
          "{",
          "--last-name=l-2",
          "--first-name=f-2",
          "}",
        ),
        makePassingTest("bracketed config - no brackets")(ExampleConfig.parserWithKey, ExampleConfig(Nil, 100))("--config"),
        makePassingTest("bracketed config - empty brackets")(ExampleConfig.parserWithKey, ExampleConfig(Nil, 100))("--config", "{", "}"),
        makePassingTest("bracketed config - labels")(ExampleConfig.parserWithKey, ExampleConfig(List("a", "b", "c"), 100))("--config", "{", "a", "b", "c", "}"),
        makePassingTest("bracketed config - max-width")(ExampleConfig.parserWithKey, ExampleConfig(Nil, 50))("--config", "{", "--max-width", "50", "}"),
        makePassingTest("bracketed config - labels + max-width")(ExampleConfig.parserWithKey, ExampleConfig(List("a", "b", "c"), 50))("--config", "{", "a", "b", "c", "--max-width", "50", "}"),
      ),
      suite("help")(
        makePassingTest("case - 1")(Parser.help <||> Params.value[Int]("int"), HelpType.HelpExtra.asLeft)("--help-extra", "a", "b", "--c", "d"),
        makePassingTest("case - 2")(Parser.help <||> Params.value[Int]("int"), HelpType.HelpExtra.asLeft)("a", "b", "--c", "d", "--help-extra"),
        makePassingTest("case - 3")(Parser.help <||> Params.value[Int]("int"), HelpType.HelpExtra.asLeft)("--help", "a", "b", "--c", "d", "--help-extra"),
        makePassingTest("case - 4")(Parser.help <||> Params.value[Int]("int"), HelpType.Help.asLeft)("--help", "a", "b", "--c", "d"),
        makePassingTest("case - 5")(Parser.help <||> Params.value[Int]("int"), 5.asRight)("--int=5"),
      ),
    )

  override def testSpec: TestSpec =
    suite("ParserSpec")(
      passesSpec,
      // TODO (KR) : fails
    )

}
