package oxygen.cli

sealed trait Help
object Help {

  final case class Extra(help: Help) extends Help

}
