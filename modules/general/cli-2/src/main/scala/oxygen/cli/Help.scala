package oxygen.cli

import oxygen.predef.color.{*, given}
import oxygen.predef.core.*

sealed trait Help {

  def subHelp: SubHelp = SubHelp.Empty

  final def withHints(hints: List[HelpHint]): Help = (hints, this) match
    case (_ :: _, Help.WithHints(annotated, msgs)) => Help.WithHints(annotated, msgs ++ hints)
    case (h :: t, self: Help.Base)                 => Help.WithHints(self, NonEmptyList(h, t))
    case _                                         => this

  private[cli] final def isEmpty: Boolean = this match
    case Help.Empty            => true
    case Help.Or(left, right)  => left.isEmpty && right.isEmpty
    case Help.And(left, right) => left.isEmpty && right.isEmpty
    case _                     => false

  private[cli] final def removeEmpties: Help = this match
    case Help.Or(left, right) if left.isEmpty     => right.removeEmpties
    case Help.Or(left, right) if right.isEmpty    => left.removeEmpties
    case Help.Or(left, right)                     => Help.Or(left.removeEmpties, right.removeEmpties)
    case Help.And(left, right) if left.isEmpty    => right.removeEmpties
    case Help.And(left, right) if right.isEmpty   => left.removeEmpties
    case Help.And(left, right)                    => Help.And(left.removeEmpties, right.removeEmpties)
    case _                                        => this

  private[cli] final def flattenOrs: List[Help] = this.removeEmpties match
    case Help.Or(left, right) => left.flattenOrs ::: right.flattenOrs
    case Help.Empty           => Nil
    case self                 => self :: Nil

  private[cli] final def toRepr(extraHints: List[HelpHint]): List[Help.Repr] = this match
    case Help.Empty => Nil
    case Help.Raw(message) => Help.Repr(message.detailedSplit("\n".r, true, true).toList.map(ColorString.make(_)), Nil, color"") :: Nil
    case Help.Positional(name, subHelp) => Help.Repr(color"[$name]".magentaFg :: Nil, Help.Repr.formatSubHelp(subHelp, extraHints), color"|    ") :: Nil
    case Help.Named(longName, shortName, valueHelp, subHelp) =>
      val main: ColorString = shortName.fold(color"--$longName".cyanFg)(s => color"--$longName, -${s.toString}".cyanFg)
      ValueHelpRepr.inlineParts(valueHelp) match
        case Some(valueParts) if !ValueHelpRepr.needsScopedChildren(valueHelp) =>
          val hints = ValueHelpRepr.valueHints(valueHelp.removeEmpties)
          val inlined: ColorString = valueParts.foldLeft(main) { (acc, part) => acc + color" " + part }
          Help.Repr(inlined :: Nil, Help.Repr.formatSubHelp(subHelp, extraHints ::: hints), color"|    ") :: Nil
        case _ =>
          Help.Repr(main :: Nil, Help.Repr.formatSubHelp(subHelp, extraHints), color"|    ") :: valueHelp.removeEmpties.toRepr(Nil).map(_.scoped)
    case Help.Flag(longName, shortName, subHelp) =>
      val main: ColorString = shortName.fold(color"--$longName".cyanFg)(s => color"--$longName, -${s.toString}".cyanFg)
      Help.Repr(main :: Nil, Help.Repr.formatSubHelp(subHelp, extraHints), color"|    ") :: Nil
    case Help.Toggle(trueName, falseName, shortNames, subHelp) =>
      val main: ColorString = shortNames match
        case Some((t, f)) => color"--$trueName / --$falseName, -${t.toString} / -${f.toString}".cyanFg
        case None         => color"--$trueName / --$falseName".cyanFg
      Help.Repr(main :: Nil, Help.Repr.formatSubHelp(subHelp, extraHints), color"|    ") :: Nil
    case Help.And(left, right) => left.toRepr(Nil) ::: right.toRepr(Nil)
    case or @ Help.Or(_, _) => or.flattenOrs.map(_.toRepr(Nil)).intersperse(Help.Repr(color"OR" :: Nil, Nil, color"") :: Nil).flatten
    case Help.WithHints(annotated, messages) => annotated.toRepr(messages.toList)
    case Help.UnparsedPositional(args) => Help.Repr.fromUnparsedPositional(args) :: Nil
    case Help.UnparsedNamed(args) => Help.Repr.fromUnparsedNamed(args) :: Nil
    case Help.Extra(help) => help.toRepr(Nil)
    case Help.Config(envVar, subHelp) =>
      Help.Repr(color"$$$envVar".greenFg :: Nil, Help.Repr.formatSubHelp(subHelp, extraHints), color"|    ") :: Nil
    case Help.Annotated(subHelp) => Help.Repr(Nil, Help.Repr.formatSubHelp(subHelp, extraHints), color"") :: Nil

  override final def toString: String = Help.Repr.format(this.removeEmpties.toRepr(Nil))

}
object Help {

  sealed trait Base extends Help

  case object Empty extends Help
  final case class Raw(message: String) extends Help.Base
  final case class Positional(name: String, override val subHelp: SubHelp) extends Help.Base
  final case class Named(longName: String, shortName: Option[Char], valueHelp: Help, override val subHelp: SubHelp) extends Help.Base
  final case class Flag(longName: String, shortName: Option[Char], override val subHelp: SubHelp) extends Help.Base
  final case class Toggle(trueName: String, falseName: String, shortNames: Option[(Char, Char)], override val subHelp: SubHelp) extends Help.Base
  final case class And(left: Help, right: Help) extends Help.Base
  final case class Or(left: Help, right: Help) extends Help.Base
  final case class WithHints(annotated: Help.Base, messages: NonEmptyList[HelpHint]) extends Help.Base
  final case class UnparsedPositional(args: NonEmptyList[PositionalArg]) extends Help
  final case class UnparsedNamed(args: NonEmptyList[NamedArg]) extends Help
  final case class Extra(help: Help) extends Help
  final case class Config(envVar: String, override val subHelp: SubHelp) extends Help.Base
  final case class Annotated(override val subHelp: SubHelp) extends Help

  sealed trait Repr {
    final def scoped: Repr = prefixLeft(color"|   ")
    final def prefixLeft(prefix: ColorString): Repr = Repr.PrefixLeft(prefix, this)
    final def normalize(prefix: ColorString): (List[ColorString], List[ColorString]) = this match
      case Repr.Simple(left, right, defaultLeft) =>
        val newLeft = left.map(prefix + _)
        val leftSize = newLeft.size
        val rightSize = right.size
        if leftSize < rightSize then (newLeft ::: List.fill(rightSize - leftSize)(prefix + defaultLeft), right)
        else if leftSize > rightSize then (newLeft, right ::: List.fill(leftSize - rightSize)(color""))
        else (newLeft, right)
      case Repr.PrefixLeft(childPrefix, child) => child.normalize(prefix + childPrefix)
  }
  object Repr {
    final case class Simple(left: List[ColorString], right: List[ColorString], defaultLeft: ColorString) extends Repr
    final case class PrefixLeft(prefix: ColorString, child: Repr) extends Repr
    export Simple.apply
    def formatSubHelp(subHelp: SubHelp, hints: List[HelpHint]): List[ColorString] =
      (subHelp.docs ::: hints.map(hintToColorString) ::: subHelp.hints.map(hintToColorString)).map(ColorString.make(_))
    def fromHints(hints: NonEmptyList[HelpHint]): Repr = Repr(hints.toList.map(hintToColorString), Nil, color"")
    def fromUnparsedPositional(args: NonEmptyList[PositionalArg]): Repr =
      Repr(args.toList.map(a => color"Unparsed value ${a.value.unesc.yellowFg} at index ${a.idx.toString.yellowFg}".redFg), Nil, color"")
    def fromUnparsedNamed(args: NonEmptyList[NamedArg]): Repr = Repr(args.toList.map(unparsedNamedToColorString), Nil, color"")
    private def hintToColorString(hint: HelpHint): ColorString = hint match
      case HelpHint.Help(message) => message
      case HelpHint.HelpExtra(message) => message
      case HelpHint.EnumValues(values) => s"Enum: ${values.mkString(", ")}"
      case HelpHint.Default(value) => s"Default: $value"
      case HelpHint.Optional => "(Optional)"
      case HelpHint.Repeated => "(Repeated)"
      case HelpHint.RepeatedNel => "(Repeated - Non Empty)"
      case HelpHint.Error(message) => message.redFg
    private def unparsedNamedToColorString(arg: NamedArg): ColorString = arg match
      case LongNameArg(idx, name, _) => color"Unparsed param --$name at index ${idx.toString.yellowFg}".redFg
      case ShortNameArg(idx, name, _) => color"Unparsed param -${name.toString.yellowFg} at index ${idx.toString.yellowFg}".redFg
      case MultiShortNameArg(idx, subIdx, name) => color"Unparsed param -${name.toString.yellowFg} at index ${idx.toString.yellowFg}.${subIdx.toString.yellowFg}".redFg
    def format(reprs: List[Repr]): String = {
      val normalized = reprs.map(_.normalize(color""))
      val lefts = normalized.flatMap(_._1)
      val rights = normalized.flatMap(_._2)
      val tmpLefts = lefts.map(s => (s, s.rawString.length))
      val maxLeft = tmpLefts.map(_._2).maxOption.getOrElse(0)
      tmpLefts.map { case (str, len) => str + (" " * (maxLeft - len)) }.zip(rights).map { case (left, right) => color"$left    $right" }.csMkString("\n").toString
    }
  }

}