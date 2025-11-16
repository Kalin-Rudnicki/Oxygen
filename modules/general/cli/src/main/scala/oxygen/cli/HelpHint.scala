package oxygen.cli

sealed trait HelpHint
object HelpHint {

  final case class Help(message: String) extends HelpHint
  final case class HelpExtra(message: String) extends HelpHint

  final case class EnumValues(values: Seq[String]) extends HelpHint
  final case class Default(value: String) extends HelpHint
  case object Optional extends HelpHint
  case object Repeated extends HelpHint
  case object RepeatedNel extends HelpHint

  final case class Error(message: String) extends HelpHint

  type Make = String | HelpHint

  def apply(make: HelpHint.Make): HelpHint = make match
    case string: String => HelpHint.Help(string)
    case hint: HelpHint => hint

}
