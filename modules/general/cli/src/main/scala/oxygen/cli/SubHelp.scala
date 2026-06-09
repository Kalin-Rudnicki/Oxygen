package oxygen.cli

sealed trait SubHelp {

  def docs: List[String]
  def hints: List[HelpHint]

  def stripDocs: SubHelp = this match
    case SubHelp.Annotated(_, hints) => SubHelp.Annotated(Nil, hints)
    case SubHelp.Empty               => SubHelp.Empty

  final def withHints(more: List[HelpHint]): SubHelp = this match
    case self: SubHelp.Annotated => self.copy(hints = self.hints ::: more)
    case SubHelp.Empty           => SubHelp.Annotated(Nil, more)

}
object SubHelp {

  case object Empty extends SubHelp {
    override val docs: List[String] = Nil
    override val hints: List[HelpHint] = Nil
  }

  final case class Annotated(docs: List[String], hints: List[HelpHint] = Nil) extends SubHelp

  def apply(docs: String*): SubHelp = Annotated(docs.toList)
  def fromDocs(docs: List[String]): SubHelp = Annotated(docs)

}
