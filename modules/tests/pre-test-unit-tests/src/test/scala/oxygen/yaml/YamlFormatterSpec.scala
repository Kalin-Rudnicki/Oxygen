package oxygen.yaml

import oxygen.core.StringBuilder
import oxygen.json.Json
import oxygen.predef.test.*
import scala.util.Try

object YamlFormatterSpec extends OxygenSpecDefault {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Config helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private val indent2: YamlFormatter.Config =
    YamlFormatter.Config(indentSize = 2)

  private val indent4: YamlFormatter.Config =
    YamlFormatter.Config(indentSize = 4)

  private val defaultConfig: YamlFormatter.Config =
    YamlFormatter.Config()

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Assertion helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def formatExact(
      label: String,
      json: Json,
      expected: String,
      config: YamlFormatter.Config,
  )(using SourceLocation): TestSpec =
    test(label) {
      val formatted = YamlFormatter.format(json, config)
      assert(formatted)(equalTo(expected))
    }

  /**
    * Format then re-parse with [[YamlParser]]. Prefer this for nested layouts while spacing
    * beyond top-level key separation is still in flux.
    */
  private def roundTrip(
      label: String,
      json: Json,
      config: YamlFormatter.Config = indent2,
  )(using SourceLocation): TestSpec =
    test(label) {
      val formatted = YamlFormatter.format(json, config)
      val parsed = YamlParser.parseJson(formatted)
      assert(parsed)(isRight(equalTo(json))).label(s"rendered:\n$formatted")
    }

  private def both(
      label: String,
      json: Json,
      expected: String,
      config: YamlFormatter.Config = indent2,
  )(using SourceLocation): TestSpec =
    suite(label)(
      formatExact("exact", json, expected, config),
      roundTrip("round-trip", json, config),
    )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Spec
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override def testSpec: TestSpec =
    suite("YamlFormatterSpec")(
      //////////////////////////////////////////////////////////////////////////////////////////////////
      // Config
      //////////////////////////////////////////////////////////////////////////////////////////////////
      suite("Config")(
        test("indentSize < 2 is rejected") {
          assertTrue(Try(YamlFormatter.Config(indentSize = 1)).isFailure)
        },
        test("indentSize = 0 is rejected") {
          assertTrue(Try(YamlFormatter.Config(indentSize = 0)).isFailure)
        },
        test("default config is indentSize 2") {
          assertTrue(defaultConfig.indentSize == 2)
        },
      ),
      //////////////////////////////////////////////////////////////////////////////////////////////////
      // Root scalars / empty collections
      //////////////////////////////////////////////////////////////////////////////////////////////////
      suite("root · inline")(
        // Leading + trailing newline around the document.
        both("null", Json.Null, "\nnull\n"),
        both("true", Json.boolean(true), "\ntrue\n"),
        both("false", Json.boolean(false), "\nfalse\n"),
        both("int", Json.number(42), "\n42\n"),
        both("negative int", Json.number(-7), "\n-7\n"),
        both("decimal", Json.number(BigDecimal("3.14")), "\n3.14\n"),
        both("empty string", Json.string(""), "\n\"\"\n"),
        both("simple string", Json.string("hello"), "\n\"hello\"\n"),
        both("string with space", Json.string("hello world"), "\n\"hello world\"\n"),
        both("string with quote", Json.string("say \"hi\""), "\n\"say \\\"hi\\\"\"\n"),
        both("string with newline", Json.string("a\nb"), "\n\"a\\nb\"\n"),
        both("string with tab", Json.string("a\tb"), "\n\"a\\tb\"\n"),
        both("string with backslash", Json.string("a\\b"), "\n\"a\\\\b\"\n"),
        both("empty array", Json.arr(), "\n[]\n"),
        both("empty object", Json.obj(), "\n{}\n"),
      ),
      //////////////////////////////////////////////////////////////////////////////////////////////////
      // Top-level object keys: always a single blank line between them
      //////////////////////////////////////////////////////////////////////////////////////////////////
      suite("root · object · top-level spacing")(
        both(
          "single field",
          Json.obj("name" -> Json.string("oxygen")),
          """
            |name: "oxygen"
            |""".stripMargin,
        ),
        both(
          "multiple fields · blank line between top-level keys",
          Json.obj(
            "name" -> Json.string("oxygen"),
            "enabled" -> Json.boolean(true),
            "port" -> Json.number(8080),
            "note" -> Json.Null,
          ),
          """
            |name: "oxygen"
            |
            |enabled: true
            |
            |port: 8080
            |
            |note: null
            |""".stripMargin,
        ),
        both(
          "empty collections as field values stay compact",
          Json.obj(
            "tags" -> Json.arr(),
            "meta" -> Json.obj(),
          ),
          """
            |tags: []
            |
            |meta: {}
            |""".stripMargin,
        ),
        both(
          "field order is preserved",
          Json.obj(
            "z" -> Json.number(1),
            "a" -> Json.number(2),
          ),
          """
            |z: 1
            |
            |a: 2
            |""".stripMargin,
        ),
      ),
      //////////////////////////////////////////////////////////////////////////////////////////////////
      // Key encoding
      //////////////////////////////////////////////////////////////////////////////////////////////////
      suite("keys")(
        both(
          "safe key: letters",
          Json.obj("abcXYZ" -> Json.number(1)),
          """
            |abcXYZ: 1
            |""".stripMargin,
        ),
        both(
          "safe key: hyphen and underscore",
          Json.obj("key-name_ok" -> Json.number(1)),
          """
            |key-name_ok: 1
            |""".stripMargin,
        ),
        both(
          "safe key: digits allowed when not leading",
          Json.obj("key1" -> Json.number(1), "a2b3" -> Json.number(2)),
          """
            |key1: 1
            |
            |a2b3: 2
            |""".stripMargin,
        ),
        both(
          "unsafe key: leading digit is quoted",
          Json.obj("1key" -> Json.number(1), "123" -> Json.number(2)),
          """
            |"1key": 1
            |
            |"123": 2
            |""".stripMargin,
        ),
        both(
          "unsafe key: spaces are quoted",
          Json.obj("my key" -> Json.number(1)),
          """
            |"my key": 1
            |""".stripMargin,
        ),
        both(
          "unsafe key: empty string is quoted",
          Json.obj("" -> Json.number(1)),
          """
            |"": 1
            |""".stripMargin,
        ),
        both(
          "unsafe key: colon / special chars are quoted",
          Json.obj("a:b" -> Json.number(1), "x.y" -> Json.number(2)),
          """
            |"a:b": 1
            |
            |"x.y": 2
            |""".stripMargin,
        ),
      ),
      //////////////////////////////////////////////////////////////////////////////////////////////////
      // Nested structure — exact only where layout is structural; prefer round-trip otherwise
      //////////////////////////////////////////////////////////////////////////////////////////////////
      suite("nested · structure (round-trip)")(
        roundTrip(
          "nested object",
          Json.obj(
            "http" -> Json.obj(
              "port" -> Json.number(3210),
              "expose" -> Json.boolean(true),
            ),
          ),
        ),
        roundTrip(
          "two levels of objects",
          Json.obj(
            "db" -> Json.obj(
              "target" -> Json.obj(
                "host" -> Json.string("localhost"),
                "port" -> Json.number(5432),
              ),
            ),
          ),
        ),
        roundTrip(
          "sibling nested objects at root (blank line between top-level keys)",
          Json.obj(
            "a" -> Json.obj("x" -> Json.number(1)),
            "b" -> Json.obj("y" -> Json.number(2)),
          ),
        ),
        roundTrip(
          "mix of inline and nested top-level fields",
          Json.obj(
            "name" -> Json.string("svc"),
            "config" -> Json.obj("debug" -> Json.boolean(false)),
            "port" -> Json.number(80),
          ),
        ),
        roundTrip(
          "array of scalars under key",
          Json.obj("items" -> Json.arr(Json.number(1), Json.number(2), Json.number(3))),
        ),
        roundTrip(
          "array of arrays under key",
          Json.obj(
            "matrix" -> Json.arr(
              Json.arr(Json.number(1), Json.number(2)),
              Json.arr(Json.number(3), Json.number(4)),
            ),
          ),
        ),
        roundTrip(
          "array of objects under key",
          Json.obj(
            "people" -> Json.arr(
              Json.obj("name" -> Json.string("a"), "age" -> Json.number(1)),
              Json.obj("name" -> Json.string("b"), "age" -> Json.number(2)),
            ),
          ),
        ),
        roundTrip(
          "array element with nested object field",
          Json.obj(
            "items" -> Json.arr(
              Json.obj(
                "id" -> Json.number(1),
                "meta" -> Json.obj("ok" -> Json.boolean(true)),
              ),
            ),
          ),
        ),
        roundTrip(
          "array element with nested array field",
          Json.obj(
            "groups" -> Json.arr(
              Json.obj(
                "name" -> Json.string("g1"),
                "ids" -> Json.arr(Json.number(1), Json.number(2)),
              ),
            ),
          ),
        ),
        roundTrip(
          "jagged nested arrays",
          Json.obj(
            "x" -> Json.arr(
              Json.arr(Json.arr(Json.arr(Json.number(9)))),
              Json.arr(),
              Json.arr(Json.string("a"), Json.arr(Json.boolean(true))),
            ),
          ),
        ),
        roundTrip(
          "root array of scalars",
          Json.arr(Json.number(1), Json.number(2), Json.number(3)),
        ),
        roundTrip(
          "root array of arrays",
          Json.arr(
            Json.arr(Json.number(1), Json.number(2)),
            Json.arr(Json.number(3), Json.number(4)),
          ),
        ),
        roundTrip(
          "root array of objects",
          Json.arr(
            Json.obj("a" -> Json.number(1)),
            Json.obj("b" -> Json.number(2)),
          ),
        ),
        roundTrip(
          "root array mixed elements",
          Json.arr(
            Json.number(1),
            Json.obj("k" -> Json.string("v")),
            Json.arr(Json.boolean(true)),
            Json.Null,
          ),
        ),
        roundTrip(
          "mini service config",
          Json.obj(
            "http" -> Json.obj(
              "port" -> Json.number(3210),
              "errors" -> Json.obj(
                "exposeInternalErrors" -> Json.boolean(true),
              ),
            ),
            "db" -> Json.obj(
              "target" -> Json.obj(
                "database" -> Json.string("oxygen_example"),
                "host" -> Json.string("localhost"),
                "port" -> Json.number(5210),
              ),
              "pool" -> Json.obj(
                "minConnections" -> Json.number(2),
                "maxConnections" -> Json.number(16),
              ),
            ),
            "features" -> Json.arr(
              Json.string("a"),
              Json.string("b"),
            ),
          ),
        ),
      ),
      roundTrip(
        "random-case-1",
        Json.arr(
          Json.arr(Json.string("0")),
          Json.arr(
            Json.obj(
              "k0" -> Json.arr(),
              "k1" -> Json.Null,
              "k2" -> Json.number(123),
            ),
          ),
        ),
        config = indent2,
      ),
      roundTrip(
        "random-case-2",
        Json.arr(
          Json.arr(Json.string("0")),
          Json.arr(
            Json.obj(
              "k0" -> Json.arr(),
              "k1" -> Json.Null,
              "k2" -> Json.number(123),
            ),
          ),
        ),
        config = indent4,
      ),
      //////////////////////////////////////////////////////////////////////////////////////////////////
      // indentSize = 4 (structure only)
      //////////////////////////////////////////////////////////////////////////////////////////////////
      suite("indentSize 4")(
        roundTrip(
          "nested object",
          Json.obj(
            "http" -> Json.obj(
              "port" -> Json.number(3210),
              "expose" -> Json.boolean(true),
            ),
          ),
          indent4,
        ),
        roundTrip(
          "array of arrays",
          Json.obj(
            "matrix" -> Json.arr(
              Json.arr(Json.number(1), Json.number(2)),
              Json.arr(Json.number(3), Json.number(4)),
            ),
          ),
          indent4,
        ),
        both(
          "top-level blank line still applies with indent 4",
          Json.obj(
            "a" -> Json.number(1),
            "b" -> Json.number(2),
          ),
          """
            |a: 1
            |
            |b: 2
            |""".stripMargin,
          indent4,
        ),
      ),
      //////////////////////////////////////////////////////////////////////////////////////////////////
      // API
      //////////////////////////////////////////////////////////////////////////////////////////////////
      suite("API")(
        test("formatInto writes into the provided StringBuilder") {
          val sb = StringBuilder.emptyThreadUnsafe
          sb.append("PREFIX:")
          YamlFormatter.formatInto(
            Json.obj("a" -> Json.number(1)),
            sb,
            indent2,
          )
          assert(sb.build())(equalTo("PREFIX:\na: 1\n"))
        },
        test("format defaults to Config()") {
          val json = Json.obj("a" -> Json.number(1), "b" -> Json.number(2))
          assert(YamlFormatter.format(json))(equalTo(YamlFormatter.format(json, defaultConfig)))
        },
      ),
      //////////////////////////////////////////////////////////////////////////////////////////////////
      //      Random
      //////////////////////////////////////////////////////////////////////////////////////////////////
      test("random round trip") {
        zio.test.check(zio.test.Gen.fromZIO(randomJson(0, 4))) { json =>
          val formatted = YamlFormatter.format(json)
          val parsed = YamlParser.parseJson(formatted)
          assertTrue(parsed == json.asRight)
        }
      } @@ TestAspect.samples(64) @@ TestAspect.shrinks(0),
    )

  private def randomFlatJson: UIO[Json] =
    Random.nextIntBounded(6).flatMap {
      case 0 => ZIO.succeed(Json.obj())
      case 1 => ZIO.succeed(Json.arr())
      case 2 => RandomGen.lowerCaseString().map(Json.string)
      case 3 => Random.nextInt.map(Json.number)
      case 4 => Random.nextDouble.map(Json.number)
      case 5 => ZIO.succeed(Json.Null)
      case _ => ZIO.dieMessage("not possible...")
    }

  private def randomJson(
      depth: Int,
      maxDepth: Int,
  ): UIO[Json] =
    Random.nextIntBounded(6).flatMap {
      case 0 | 1 | 2 if depth < maxDepth =>
        for {
          numFields <- Random.nextIntBetween(1, 4)
          genField = RandomGen.lowerCaseString() <*> randomJson(depth + 1, maxDepth)
          fields <- genField.replicateZIO(numFields)
        } yield Json.obj(fields.toSeq*)
      case 3 if depth < maxDepth =>
        for {
          numElems <- Random.nextIntBetween(1, 4)
          elems <- randomJson(depth + 1, maxDepth).replicateZIO(numElems)
        } yield Json.arr(elems.toSeq*)
      case _ =>
        randomFlatJson
    }

}
