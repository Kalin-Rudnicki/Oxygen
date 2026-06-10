package oxygen.cli

sealed trait HelpHint
object HelpHint {
  final case class EnumValues(values: Seq[String]) extends HelpHint
}