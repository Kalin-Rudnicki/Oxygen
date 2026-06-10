package oxygen.cli

sealed trait Help
object Help {
  case object Empty extends Help
  final case class Extra(help: Help) extends Help
}