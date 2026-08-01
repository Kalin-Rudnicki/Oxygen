package oxygen.cli

import oxygen.predef.test.*

object ArgsParserSpec extends OxygenSpecDefault {

  private def makePositional[A: PositionalArgsParser.Builder as p](name: String): PositionalArgsParser[A] =
    p.build(name, SubHelp.Empty)

  private def parse[A](parser: ArgsParser[A], args: String*): CliParseResult[A, Args] =
    Args.parse(args.toList) match
      case Right(a)  => parser.parseArgs(a)
      case Left(msg) => throw new RuntimeException(s"failed to tokenize args: $msg")

  // Count the distinct leaf errors in a (possibly RootAnd/Or-combined) error tree.
  private def leafErrorCount(error: CliParseError): Int = error match
    case CliParseError.RootAnd(l, r)      => leafErrorCount(l) + leafErrorCount(r)
    case CliParseError.RootOr(l, r)       => leafErrorCount(l) + leafErrorCount(r)
    case CliParseError.PositionalOr(l, r) => leafErrorCount(l) + leafErrorCount(r)
    case CliParseError.NamedOr(l, r)      => leafErrorCount(l) + leafErrorCount(r)
    case _                                => 1

  override def testSpec: TestSpec =
    suite("ArgsParserSpec")(
      suite("accumulates all errors (not just the first)")(
        test("two missing positionals both reported") {
          val p = makePositional[String]("first") ^>>&& makePositional[String]("second")
          val n = parse(p) match
            case CliParseResult.Fail(error, _) => leafErrorCount(error)
            case _                             => 0
          assertTrue(n == 2)
        },
        test("missing positional AND missing flag both reported") {
          val parser: ArgsParser[(String, String)] =
            makePositional[String]("name") ^>>&& NamedArgsParser.Named("token", Defaultable.Default, PositionalArgsParser.singlePlain[String]("token"), SubHelp.Empty)
          val n = parse(parser) match
            case CliParseResult.Fail(error, _) => leafErrorCount(error)
            case _                             => 0
          assertTrue(n == 2)
        },
      ),
      suite("positional")(
        test("required - present") {
          val p = PositionalArgsParser.singlePlain[String]("name")
          assertTrue(parse(p, "hello") == CliParseResult.Success("hello", Args.empty))
        },
        test("required - missing fails with MissingRequiredPositional") {
          val p = PositionalArgsParser.singlePlain[String]("name")
          val isExpected = parse(p) match
            case CliParseResult.Fail(CliParseError.MissingRequiredPositional("name"), _) => true
            case _                                                                       => false
          assertTrue(isExpected)
        },
        test("optional - missing yields None") {
          val p = PositionalArgsParser.Optional(PositionalArgsParser.singlePlain[Int]("count"))
          assertTrue(parse(p) == CliParseResult.Success(None, Args.empty))
        },
        test("optional - present yields Some") {
          val p = PositionalArgsParser.Optional(PositionalArgsParser.singlePlain[Int]("count"))
          assertTrue(parse(p, "7") == CliParseResult.Success(Some(7), Args.empty))
        },
        test("withDefault - missing yields default") {
          val p = PositionalArgsParser.singlePlain[String]("name").withDefault("anon")
          assertTrue(parse(p) == CliParseResult.Success("anon", Args.empty))
        },
        test("withDefault - present overrides default") {
          val p = PositionalArgsParser.singlePlain[String]("name").withDefault("anon")
          assertTrue(parse(p, "kalin") == CliParseResult.Success("kalin", Args.empty))
        },
      ),
      suite("named / flag")(
        test("flag - present is true") {
          val p = NamedArgsParser.Flag("verbose", Defaultable.Default, default = false, SubHelp.Empty)
          assertTrue(parse(p, "--verbose") == CliParseResult.Success(true, Args.empty))
        },
        test("flag - absent is default") {
          val p = NamedArgsParser.Flag("verbose", Defaultable.Default, default = false, SubHelp.Empty)
          assertTrue(parse(p) == CliParseResult.Success(false, Args.empty))
        },
      ),
      suite("resolveAutoShortNames (global two-phase pass)")(
        test("two autos sharing a first char: first wins it, second gets the case-flipped char") {
          val parser =
            NamedArgsParser.Flag("verbose", Defaultable.Default, default = false, SubHelp.Empty) &&
              NamedArgsParser.Flag("version", Defaultable.Default, default = false, SubHelp.Empty)
          NamedArgsParser.resolveAutoShortNames(parser) match
            case NamedArgsParser.AndWith(a: NamedArgsParser.Flag, b: NamedArgsParser.Flag, _) =>
              assertTrue(a.shortName == Defaultable.Explicit(Some('v')), b.shortName == Defaultable.Explicit(Some('V')))
            case other => assertTrue(false).label(s"unexpected shape: $other")
        },
        test("explicit short is reserved and wins over a colliding auto, regardless of order") {
          val parser =
            NamedArgsParser.Flag("verbose", Defaultable.Default, default = false, SubHelp.Empty) &&
              NamedArgsParser.Flag("verify", Defaultable.Explicit(Some('v')), default = false, SubHelp.Empty)
          NamedArgsParser.resolveAutoShortNames(parser) match
            // `verify` keeps its explicit -v; the auto `verbose` doesn't steal it, falling back to -V.
            case NamedArgsParser.AndWith(a: NamedArgsParser.Flag, b: NamedArgsParser.Flag, _) =>
              assertTrue(a.shortName == Defaultable.Explicit(Some('V')), b.shortName == Defaultable.Explicit(Some('v')))
            case other => assertTrue(false).label(s"unexpected shape: $other")
        },
        test("colliding autos fall back to the case-flipped first char, then give up") {
          val parser =
            NamedArgsParser.Flag("host", Defaultable.Default, default = false, SubHelp.Empty) &&
              NamedArgsParser.Flag("hotness", Defaultable.Default, default = false, SubHelp.Empty) &&
              NamedArgsParser.Flag("height", Defaultable.Default, default = false, SubHelp.Empty)
          NamedArgsParser.resolveAutoShortNames(parser) match
            case NamedArgsParser.AndWith(NamedArgsParser.AndWith(a: NamedArgsParser.Flag, b: NamedArgsParser.Flag, _), c: NamedArgsParser.Flag, _) =>
              assertTrue(
                a.shortName == Defaultable.Explicit(Some('h')), // as-is
                b.shortName == Defaultable.Explicit(Some('H')), // 'h' taken -> case-flipped
                c.shortName == Defaultable.Explicit(None), // both 'h' and 'H' taken -> none
              )
            case other => assertTrue(false).label(s"unexpected shape: $other")
        },
        test("two params explicitly claiming the same short are rejected") {
          val parser =
            NamedArgsParser.Flag("foo", Defaultable.Explicit(Some('f')), default = false, SubHelp.Empty) &&
              NamedArgsParser.Flag("bar", Defaultable.Explicit(Some('f')), default = false, SubHelp.Empty)
          val err = scala.util.Try(NamedArgsParser.resolveAutoShortNames(parser)).failed.toOption
          assertTrue(err.exists(e => e.getMessage.contains("-f") && e.getMessage.contains("--foo") && e.getMessage.contains("--bar")))
        },
        test("an explicit short colliding with an auto is fine (auto yields, no error)") {
          val parser =
            NamedArgsParser.Flag("foo", Defaultable.Explicit(Some('f')), default = false, SubHelp.Empty) &&
              NamedArgsParser.Flag("force", Defaultable.Default, default = false, SubHelp.Empty)
          NamedArgsParser.resolveAutoShortNames(parser) match
            case NamedArgsParser.AndWith(a: NamedArgsParser.Flag, b: NamedArgsParser.Flag, _) =>
              assertTrue(a.shortName == Defaultable.Explicit(Some('f')), b.shortName == Defaultable.Explicit(Some('F')))
            case other => assertTrue(false).label(s"unexpected shape: $other")
        },
        test("non-colliding autos each keep their first char") {
          val parser =
            NamedArgsParser.Flag("verbose", Defaultable.Default, default = false, SubHelp.Empty) &&
              NamedArgsParser.Flag("count", Defaultable.Default, default = false, SubHelp.Empty)
          NamedArgsParser.resolveAutoShortNames(parser) match
            case NamedArgsParser.AndWith(a: NamedArgsParser.Flag, b: NamedArgsParser.Flag, _) =>
              assertTrue(a.shortName == Defaultable.Explicit(Some('v')), b.shortName == Defaultable.Explicit(Some('c')))
            case other => assertTrue(false).label(s"unexpected shape: $other")
        },
        test("resolved short parses; the loser only matches by long name") {
          val parser: ArgsParser[(Boolean, Boolean)] =
            (NamedArgsParser.Flag("verbose", Defaultable.Default, default = false, SubHelp.Empty) &&
              NamedArgsParser.Flag("version", Defaultable.Default, default = false, SubHelp.Empty)).resolveAutoShortNames
          assertTrue(
            parse(parser, "-v") == CliParseResult.Success((true, false), Args.empty),
            parse(parser, "--version") == CliParseResult.Success((false, true), Args.empty),
          )
        },
      ),
      suite("combined positional + named (two independent passes)")(
        test("positional then flag both parse") {
          val parser: ArgsParser[(String, Boolean)] =
            makePositional[String]("name") ^>>&& NamedArgsParser.Flag("verbose", Defaultable.Default, default = false, SubHelp.Empty)
          assertTrue(parse(parser, "hello", "--verbose") == CliParseResult.Success(("hello", true), Args.empty))
        },
        test("named-before-positional still recombines correctly") {
          // Order of the streams is independent: a missing positional should still fail cleanly.
          val parser: ArgsParser[(String, Boolean)] =
            makePositional[String]("name") ^>>&& NamedArgsParser.Flag("verbose", Defaultable.Default, default = false, SubHelp.Empty)
          val isExpected = parse(parser, "--verbose") match
            case CliParseResult.Fail(CliParseError.MissingRequiredPositional("name"), _) => true
            case _                                                                       => false
          assertTrue(isExpected)
        },
      ),
    )

}
