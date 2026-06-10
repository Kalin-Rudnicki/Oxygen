package oxygen.executable

import oxygen.cli.*
import oxygen.predef.test.*
import zio.*

final case class HelpLeafApp() extends CliApp[Any, Any] {

  @execute
  def run(
      @named @shortName('t') token: String,
      @positional note: Option[String],
  ): Effect =
    ZIO.logInfo(s"token=$token note=$note")

}

final case class HelpParentL2() extends CliApp[Any, Any] {

  @command
  def bottom: HelpLeafApp = HelpLeafApp()

}

final case class HelpParentL1() extends CliApp[Any, Any] {

  @command
  def down: HelpParentL2 = HelpParentL2()

}

final case class HelpRootApp() extends CliApp[Any, Any] {

  @command
  def deep: HelpParentL1 = HelpParentL1()

}

object NestedHelpSpec extends OxygenSpecDefault {

  private val compiled: CompiledCliApp[Any] = summon[DeriveCliApp.Root[HelpRootApp]].app

  private def leafHelpText: String =
    val bottom =
      compiled
        .subCommands("deep")
        .subCommands("down")
        .subCommands("bottom")
    CliHelp
      .compose(
        bottom.helpParser,
        HelpType.HelpExtra,
        title = Some(Help.CommandTitle("deep down bottom")),
      )
      .toString

  override def testSpec: TestSpec =
    suite("NestedHelpSpec")(
      test("nested execute-only leaf help includes @execute params") {
        val help = leafHelpText
        assertTrue(
          help.contains("--token"),
          help.contains("[note]"),
        )
      },
    )

}
