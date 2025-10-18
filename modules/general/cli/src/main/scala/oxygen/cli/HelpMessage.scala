package oxygen.cli

import oxygen.predef.base.*
import oxygen.predef.color.{*, given}

sealed trait HelpMessage {

  def addHints(messages: List[HelpHint]): HelpMessage

  private[cli] final def isEmpty: Boolean = this match
    case HelpMessage.RootMessage.Empty              => true
    case HelpMessage.ValueMessage.Empty             => true
    case HelpMessage.ParamMessage.Empty             => true
    case HelpMessage.RootMessage.Or(left, right)    => left.isEmpty && right.isEmpty
    case HelpMessage.ValueMessage.Or(left, right)   => left.isEmpty && right.isEmpty
    case HelpMessage.ParamMessage.Or(left, right)   => left.isEmpty && right.isEmpty
    case HelpMessage.RootMessage.And(left, right)   => left.isEmpty && right.isEmpty
    case HelpMessage.ValueMessage.Then(left, right) => left.isEmpty && right.isEmpty
    case HelpMessage.ParamMessage.And(left, right)  => left.isEmpty && right.isEmpty
    case _                                          => false

  // TODO (KR) : implement in children for better type safety
  private[cli] final def removeEmpties: HelpMessage = this match
    case HelpMessage.RootMessage.Or(left, right) if left.isEmpty     => right.removeEmpties
    case HelpMessage.RootMessage.Or(left, right) if right.isEmpty    => left.removeEmpties
    case HelpMessage.RootMessage.Or(left, right)                     => HelpMessage.RootMessage.Or(left.removeEmpties, right.removeEmpties)
    case HelpMessage.ValueMessage.Or(left, right) if left.isEmpty    => right.removeEmpties
    case HelpMessage.ValueMessage.Or(left, right) if right.isEmpty   => left.removeEmpties
    case HelpMessage.ValueMessage.Or(left, right)                    => HelpMessage.ValueMessage.Or(left.removeEmpties.asInstanceOf, right.removeEmpties.asInstanceOf)
    case HelpMessage.ParamMessage.Or(left, right) if left.isEmpty    => right.removeEmpties
    case HelpMessage.ParamMessage.Or(left, right) if right.isEmpty   => left.removeEmpties
    case HelpMessage.ParamMessage.Or(left, right)                    => HelpMessage.ParamMessage.Or(left.removeEmpties.asInstanceOf, right.removeEmpties.asInstanceOf)
    case HelpMessage.RootMessage.And(left, right) if left.isEmpty    => right.removeEmpties
    case HelpMessage.RootMessage.And(left, right) if right.isEmpty   => left.removeEmpties
    case HelpMessage.RootMessage.And(left, right)                    => HelpMessage.RootMessage.And(left.removeEmpties, right.removeEmpties)
    case HelpMessage.ValueMessage.Then(left, right) if left.isEmpty  => right.removeEmpties
    case HelpMessage.ValueMessage.Then(left, right) if right.isEmpty => left.removeEmpties
    case HelpMessage.ValueMessage.Then(left, right)                  => HelpMessage.ValueMessage.Then(left.removeEmpties.asInstanceOf, right.removeEmpties.asInstanceOf)
    case HelpMessage.ParamMessage.And(left, right) if left.isEmpty   => right.removeEmpties
    case HelpMessage.ParamMessage.And(left, right) if right.isEmpty  => left.removeEmpties
    case HelpMessage.ParamMessage.And(left, right)                   => HelpMessage.ParamMessage.And(left.removeEmpties.asInstanceOf, right.removeEmpties.asInstanceOf)
    case _                                                           => this

  private[cli] final def flattenOrs: List[HelpMessage] = this.removeEmpties match
    case HelpMessage.RootMessage.Or(left, right)  => left.flattenOrs ::: right.flattenOrs
    case HelpMessage.ValueMessage.Or(left, right) => left.flattenOrs ::: right.flattenOrs
    case HelpMessage.ParamMessage.Or(left, right) => left.flattenOrs ::: right.flattenOrs
    case HelpMessage.RootMessage.Empty            => Nil
    case HelpMessage.ValueMessage.Empty           => Nil
    case HelpMessage.ParamMessage.Empty           => Nil
    case self                                     => self :: Nil

  private[cli] final def orToRepr: List[HelpMessage.Repr] = this.flattenOrs match
    case head :: Nil => head.toRepr(Nil)
    case Nil         => Nil
    case ors         => ors.map(_.toRepr(Nil).map(_.scoped)).intersperse(HelpMessage.Repr(color"OR" :: Nil, Nil, color"") :: Nil).flatten

  private[cli] final def toRepr(hints: List[HelpHint]): List[HelpMessage.Repr] =
    this match {

      // --- Empty ---
      case HelpMessage.RootMessage.Empty  => Nil
      case HelpMessage.ValueMessage.Empty => Nil
      case HelpMessage.ParamMessage.Empty => Nil

      // --- Special ---

      case HelpMessage.ValueMessage.Raw(name) =>
        HelpMessage.Repr(color"[${name.showValue}] (raw value)" :: Nil, hints.map(HelpMessage.Repr.hintToColorString), color"|    ") :: Nil
      case HelpMessage.ValueMessage.Value(name) =>
        HelpMessage.Repr(color"[${name.showValue}] (value)" :: Nil, hints.map(HelpMessage.Repr.hintToColorString), color"|    ") :: Nil
      case HelpMessage.ValueMessage.Bracketed(name, child) =>
        HelpMessage.Repr(color"[${name.showValue}] (bracketed)" :: Nil, hints.map(HelpMessage.Repr.hintToColorString), color"|    ") ::
          child.removeEmpties.toRepr(Nil).map(_.scoped)
      case HelpMessage.ParamMessage.Raw(name) =>
        HelpMessage.Repr(color"${name.showParamRaw} (raw param)" :: Nil, hints.map(HelpMessage.Repr.hintToColorString), color"|    ") :: Nil
      case HelpMessage.ParamMessage.Param(longReference, shortReference, aliases, valueMessage) =>
        val mainLine: ColorString = shortReference.fold(longReference.showParam)(s => color"${longReference.showParam}, ${s.showParam}")
        val aliasesLine: Option[ColorString] = Option.when(aliases.nonEmpty)(aliases.map(_.showParam).csMkString(", "))
        HelpMessage.Repr(mainLine :: aliasesLine.toList, hints.map(HelpMessage.Repr.hintToColorString), color"|    ") :: valueMessage.removeEmpties.toRepr(Nil).map(_.scoped)

      case _ if hints.nonEmpty =>
        HelpMessage.Repr.fromHints(NonEmptyList.unsafeFromList(hints)) :: this.toRepr(Nil).map(_.scoped)

      // --- Base ---
      case HelpMessage.RootMessage.And(left, right) => left.toRepr(Nil) ::: right.toRepr(Nil)
      case or @ HelpMessage.RootMessage.Or(_, _)    => or.flattenOrs.map(_.toRepr(Nil)).intersperse(HelpMessage.Repr(color"" :: color"OR" :: color"" :: Nil, Nil, color"") :: Nil).flatten

      case or @ HelpMessage.ValueMessage.Or(_, _)     => or.orToRepr
      case HelpMessage.ValueMessage.Then(left, right) => left.toRepr(Nil) ::: right.toRepr(Nil)

      case or @ HelpMessage.ParamMessage.Or(_, _)    => or.orToRepr
      case HelpMessage.ParamMessage.And(left, right) => left.toRepr(Nil) ::: right.toRepr(Nil)

      // --- With Hints ---
      case HelpMessage.RootMessage.WithHints(annotated, messages)  => annotated.toRepr(messages.toList)
      case HelpMessage.ValueMessage.WithHints(annotated, messages) => annotated.toRepr(messages.toList)
      case HelpMessage.ParamMessage.WithHints(annotated, messages) => annotated.toRepr(messages.toList)

      // --- Unparsed ---
      case HelpMessage.ValueMessage.UnparsedArgs(args) => HelpMessage.Repr.fromUnparsed(args) :: Nil
      case HelpMessage.ParamMessage.UnparsedArgs(args) => HelpMessage.Repr.fromUnparsed(args) :: Nil

    }

  override final def toString: String = HelpMessage.Repr.format(this.removeEmpties.toRepr(Nil))

}
object HelpMessage {

  final case class Config(
      tightenLeft: Boolean,
      leftPadding: Int,
      leftWidth: Int,
      centerPadding: Int,
      rightWidth: Int,
      indent: Int,
  )
  object Config {

    val default: Config =
      Config(
        tightenLeft = true,
        leftPadding = 2,
        leftWidth = 75,
        centerPadding = 4,
        rightWidth = 75,
        indent = 2,
      )

  }

  sealed trait Repr {

    final def prefixLeft(prefix: ColorString): Repr = Repr.PrefixLeft(prefix, this)

    final def scoped: Repr = prefixLeft(color"|   ")

    final def withHints(hints: NonEmptyList[HelpHint]): Repr = this match
      case Repr.Simple(left, right, defaultLeft) => Repr.Simple(left, right ::: hints.toList.map(Repr.hintToColorString), defaultLeft)
      case Repr.PrefixLeft(prefix, child)        => Repr.PrefixLeft(prefix, child.withHints(hints))

    final def normalize(prefix: ColorString): (List[ColorString], List[ColorString]) = this match {
      case Repr.Simple(left, right, defaultLeft) =>
        val newLeft = left.map(prefix + _)
        val leftSize = newLeft.size
        val rightSize = right.size

        if (leftSize < rightSize) (newLeft ::: List.fill(rightSize - leftSize)(prefix + defaultLeft), right)
        else if (leftSize > rightSize) (newLeft, right ::: List.fill(leftSize - rightSize)(color""))
        else (newLeft, right)
      case Repr.PrefixLeft(_prefix, child) => child.normalize(prefix + _prefix)
    }

  }
  object Repr {

    final case class Simple(left: List[ColorString], right: List[ColorString], defaultLeft: ColorString) extends Repr
    final case class PrefixLeft(prefix: ColorString, child: Repr) extends Repr

    export Simple.apply

    val break: Repr = Repr("" :: Nil, Nil, color"")

    def fromHints(hints: NonEmptyList[HelpHint]): Repr =
      Repr(hints.toList.map(hintToColorString), Nil, color"")

    def fromUnparsed(args: NonEmptyList[Arg]): Repr =
      Repr(args.toList.map(unparsedArgToColorString), Nil, color"")

    private[HelpMessage] def hintToColorString(hint: HelpHint): ColorString = hint match
      case HelpHint.Help(message)      => message
      case HelpHint.HelpExtra(message) => message
      case HelpHint.EnumValues(values) => s"Enum: ${values.mkString(", ")}"
      case HelpHint.Default(value)     => s"Default: $value"
      case HelpHint.Optional           => "(Optional)"
      case HelpHint.Repeated           => "(Repeated)"
      case HelpHint.RepeatedNel        => "(Repeated - Non Empty)"
      case HelpHint.Error(message)     => message.redFg

    private def unparsedArgToColorString(arg: Arg): ColorString = arg match
      case Arg.Value(index, value)             => color"Unparsed value ${value.unesc.yellowFg} at index ${index.toString.yellowFg}".redFg
      case Arg.Bracketed(index, _, _)          => color"Unparsed bracket value at index ${index.toString.yellowFg}".redFg
      case Arg.ShortParamMulti(index, _, name) => color"Unparsed param ${name.showParam.yellowFg} at index ${index.toString.yellowFg}".redFg
      case Arg.ScopedParam(index, name, _)     => color"Unparsed param ${name.showParam.yellowFg} at index ${index.toString.yellowFg}".redFg

    def format(reprs: List[Repr]): String = {
      val normalized: List[(List[ColorString], List[ColorString])] = reprs.map(_.normalize(color""))
      val lefts: List[ColorString] = normalized.flatMap(_._1)
      val rights: List[ColorString] = normalized.flatMap(_._2)

      val tmpLefts: List[(ColorString, Int)] = lefts.map { s => (s, s.rawString.length) }
      val maxLeft: Int = tmpLefts.map(_._2).maxOption.getOrElse(0)

      tmpLefts.map { case (str, len) => str + (" " * (maxLeft - len)) }.zip(rights).map { case (left, right) => color"$left    $right" }.csMkString("\n").toString
    }

  }

  sealed trait RootMessage extends HelpMessage {

    final override def addHints(messages: List[HelpHint]): RootMessage = (messages, this) match
      case (h :: t, self: RootMessage.Base)                 => RootMessage.WithHints(self, NonEmptyList(h, t))
      case (_ :: _, RootMessage.WithHints(annotated, msgs)) => RootMessage.WithHints(annotated, msgs ++ messages)
      case _                                                => this

  }
  object RootMessage {

    sealed trait Base extends RootMessage
    final case class And(left: HelpMessage, right: HelpMessage) extends RootMessage.Base
    final case class Or(left: HelpMessage, right: HelpMessage) extends RootMessage.Base

    case object Empty extends RootMessage
    final case class WithHints(annotated: RootMessage.Base, messages: NonEmptyList[HelpHint]) extends RootMessage

  }

  sealed trait ValueMessage extends HelpMessage {

    final override def addHints(messages: List[HelpHint]): ValueMessage = (messages, this) match
      case (h :: t, self: ValueMessage.Base)                 => ValueMessage.WithHints(self, NonEmptyList(h, t))
      case (_ :: _, ValueMessage.WithHints(annotated, msgs)) => ValueMessage.WithHints(annotated, msgs ++ messages)
      case _                                                 => this

  }
  object ValueMessage {

    sealed trait Base extends ValueMessage
    final case class Raw(name: LongName) extends ValueMessage.Base
    final case class Value(name: LongName) extends ValueMessage.Base
    final case class Bracketed(name: LongName, child: HelpMessage) extends ValueMessage.Base
    final case class Or(left: ValueMessage, right: ValueMessage) extends ValueMessage.Base
    final case class Then(left: ValueMessage, right: ValueMessage) extends ValueMessage.Base

    case object Empty extends ValueMessage
    final case class UnparsedArgs(args: NonEmptyList[Arg.ValueLike]) extends ValueMessage
    final case class WithHints(annotated: ValueMessage.Base, messages: NonEmptyList[HelpHint]) extends ValueMessage

  }

  sealed trait ParamMessage extends HelpMessage {

    final override def addHints(messages: List[HelpHint]): ParamMessage = (messages, this) match
      case (h :: t, self: ParamMessage.Base)                 => ParamMessage.WithHints(self, NonEmptyList(h, t))
      case (_ :: _, ParamMessage.WithHints(annotated, msgs)) => ParamMessage.WithHints(annotated, msgs ++ messages)
      case _                                                 => this

  }
  object ParamMessage {

    sealed trait Base extends ParamMessage
    final case class Raw(name: LongName) extends ParamMessage.Base
    final case class Param(longReference: LongReference, shortReference: Option[ShortReference], aliases: List[Name], valueMessage: ValueMessage) extends ParamMessage.Base
    final case class Or(left: ParamMessage, right: ParamMessage) extends ParamMessage.Base
    final case class And(left: ParamMessage, right: ParamMessage) extends ParamMessage.Base

    case object Empty extends ParamMessage
    final case class UnparsedArgs(args: NonEmptyList[Arg.ParamLike]) extends ParamMessage
    final case class WithHints(annotated: ParamMessage.Base, messages: NonEmptyList[HelpHint]) extends ParamMessage

  }

}
