package oxygen.cli

import oxygen.predef.core.*
import oxygen.predef.test.*
import oxygen.schema.PlainTextSchema
import zio.*

object CompletionSpec extends OxygenSpecDefault {

  enum Mood derives StrictEnum { case Chill, Hype }

  private val portParser: NamedArgsParser[Int] =
    NamedArgsParser.named("port", PositionalArgsParser.single("port", SubHelp.Empty)(using PlainTextSchema.int, summon[CompletionOptions[Int]]), shortName = Some('p'))

  private val moodParser: NamedArgsParser[Mood] =
    NamedArgsParser.named("mood", PositionalArgsParser.single("mood", SubHelp.Empty)(using PlainTextSchema.`enum`[Mood], summon[CompletionOptions[Mood]]))

  private val serverParser: ArgsParser[?] = portParser ^>> moodParser

  private def complete(parser: ArgsParser[?], args: List[String], argIdx: Int): Task[List[String]] =
    parser.complete(CompletionRequest(args.length, argIdx, args, "\n"), args.lift(argIdx).getOrElse(""))

  override def testSpec: TestSpec =
    suite("CompletionSpec")(
      test("completes flag names for empty cursor") {
        complete(serverParser, List("--"), 0).map { out =>
          assertTrue(out.contains("--port"), out.contains("--mood"))
        }
      },
      test("completes partial flag prefix") {
        complete(serverParser, List("--po"), 0).map { out =>
          assertTrue(out == List("--port"))
        }
      },
      test("completes enum values after --mood") {
        complete(serverParser, List("--mood", ""), 1).map { out =>
          assertTrue(out.contains("Chill"), out.contains("Hype"))
        }
      },
      test("completes enum values after partial --mood value") {
        complete(serverParser, List("--mood", "Ch"), 1).map { out =>
          assertTrue(out == List("Chill"))
        }
      },
      test("Then routes to second parser after first is satisfied") {
        complete(serverParser, List("--port", "8080", "--mo"), 2).map { out =>
          assertTrue(out == List("--mood"))
        }
      },
      test("completes all flags at empty cursor") {
        complete(serverParser, Nil, 0).map { out =>
          assertTrue(out.contains("--port"), out.contains("--mood"), out.contains("-p"))
        }
      },
    )

}
