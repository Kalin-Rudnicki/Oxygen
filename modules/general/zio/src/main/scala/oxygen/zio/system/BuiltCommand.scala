package oxygen.zio.system

import oxygen.predef.core.*

final case class BuiltCommand(
    command: String,
    args: List[String],
    cwd: Option[Path],
    env: Map[String, String],
) extends Showable {

  def commandIgnoreSudo: String = (command, args) match
    case ("sudo", cmd :: _) => cmd
    case _                  => command

  def showCommand: Text = showCommand(false)
  def showCommand(forceEscape: Boolean): Text =
    Text.foreachJoined(command :: args, " ") { s => Text.fromString(BuiltCommand.safeShow(s, forceEscape)) }

  override def show: Text = showCommand

}
object BuiltCommand {

  given Conversion[Command, BuiltCommand] = _.build

  /**
    * Render a single command token for _display / logging only_ (commands are executed via an explicit
    * argv list through `ProcessBuilder`, never through a shell). A token is left bare only when every one
    * of its characters is shell-safe; otherwise it is wrapped in POSIX single quotes, with embedded single
    * quotes escaped using the standard `'\''` idiom, so the rendered string round-trips through a POSIX
    * shell back to the original token.
    *
    * The safe set is an allow-list (matching `shlex.quote`): alphanumerics plus `@%+=:,./-_`. Everything
    * else — whitespace, quotes, `$`, backticks, backslashes, globs, redirections, … — forces quoting. An
    * allow-list is used rather than a deny-list precisely so a character like `\` (which the shell would
    * otherwise consume out of a bare token) can never slip through un-quoted.
    */
  def safeShow(value: String, forceEscape: Boolean): String =
    if forceEscape || value.isEmpty || value.exists { c => !isShellSafe(c) } then s"'${value.replace("'", "'\\''")}'"
    else value

  private def isShellSafe(c: Char): Boolean =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || "@%+=:,./-_".contains(c)

}
