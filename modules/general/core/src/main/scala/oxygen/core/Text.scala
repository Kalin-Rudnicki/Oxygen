package oxygen.core

import oxygen.core.collection.NonEmptyList
import oxygen.core.typeclass.{SeqRead, Showable}
import oxygen.quoted.*
import scala.collection.immutable.ArraySeq
import scala.quoted.*

/**
  * Lazy representation of a [[String]].
  * For example, with [[String]], if you want to do [[toString]] on an object 5 levels deep,
  * you are doing a BOATLOAD of string allocations, copying, and re-copying.
  * With [[Text]], you build up the structure of the final string you want to build,
  * and only when you finally want to do something with the string, like print it, do you evaluate the final string.
  */
sealed trait Text {

  final def +(that: Text.Auto): Text = Text.impl.Concat._2(this, that)
  final def ++(that: Text.Auto): Text = Text.impl.Concat._2(this, that)

  // `|` : newline
  // `/` : indent0
  // `>` : indentN
  final def |/>(that: Text.Auto): Text = this ++ that.indentCustom(Text.IndentPrefixMode.NewlineAndIndent, Text.IndentType.Default)
  final def />(that: Text.Auto): Text = this ++ that.indentCustom(Text.IndentPrefixMode.IndentOnly, Text.IndentType.Default)
  final def >(that: Text.Auto): Text = this ++ that.indentCustom(Text.IndentPrefixMode.Empty, Text.IndentType.Default)
  @deprecated("use |/>")
  final def |>(that: Text.Auto): Text = this |/> that

  final def indentedInitial(idt: Text.Auto): Text = this.indentCustom(Text.IndentPrefixMode.NewlineAndIndent, Text.IndentType.CustomInitial(idt))
  final def indented(idt: Text.Auto): Text = this.indentCustom(Text.IndentPrefixMode.NewlineAndIndent, Text.IndentType.Custom(idt))
  final def indented: Text = this.indentCustom(Text.IndentPrefixMode.NewlineAndIndent, Text.IndentType.Default)
  final def indentedNoLeadingInitial(idt: Text.Auto): Text = this.indentCustom(Text.IndentPrefixMode.IndentOnly, Text.IndentType.CustomInitial(idt))
  final def indentedNoLeading(idt: Text.Auto): Text = this.indentCustom(Text.IndentPrefixMode.IndentOnly, Text.IndentType.Custom(idt))
  final def indentedNoLeading: Text = this.indentCustom(Text.IndentPrefixMode.IndentOnly, Text.IndentType.Default)

  final def indentCustom(prefixMode: Text.IndentPrefixMode, idt: Text.IndentType): Text = Text.impl.Indented(this, prefixMode, idt)

  final def when(cond: Boolean): Text = Text.impl.When(this, cond)
  final def unless(cond: Boolean): Text = Text.impl.When(this, !cond)

  final def colorizeFg(fg: Specified[Color] = ___): Text = fg.fold(this)(Text.impl.Colorize.ColorizeFg(this, _))
  final def colorizeBg(bg: Specified[Color] = ___): Text = bg.fold(this)(Text.impl.Colorize.ColorizeBg(this, _))
  final def colorizeFgBg(fg: Specified[Color] = ___, bg: Specified[Color] = ___): Text = (fg, bg) match
    case (Specified.WasSpecified(fg), Specified.WasSpecified(bg)) => Text.impl.Colorize.ColorizeFgBg(this, fg, bg)
    case (Specified.WasSpecified(fg), Specified.WasNotSpecified)  => Text.impl.Colorize.ColorizeFg(this, fg)
    case (Specified.WasNotSpecified, Specified.WasSpecified(bg))  => Text.impl.Colorize.ColorizeBg(this, bg)
    case (Specified.WasNotSpecified, Specified.WasNotSpecified)   => this

  final def blackFg: Text = this.colorizeFg(Color.Named.Black)
  final def redFg: Text = this.colorizeFg(Color.Named.Red)
  final def greenFg: Text = this.colorizeFg(Color.Named.Green)
  final def yellowFg: Text = this.colorizeFg(Color.Named.Yellow)
  final def blueFg: Text = this.colorizeFg(Color.Named.Blue)
  final def magentaFg: Text = this.colorizeFg(Color.Named.Magenta)
  final def cyanFg: Text = this.colorizeFg(Color.Named.Cyan)
  final def whiteFg: Text = this.colorizeFg(Color.Named.White)
  final def resetFg: Text = this.colorizeFg(Color.Default)

  final def blackBg: Text = this.colorizeBg(Color.Named.Black)
  final def redBg: Text = this.colorizeBg(Color.Named.Red)
  final def greenBg: Text = this.colorizeBg(Color.Named.Green)
  final def yellowBg: Text = this.colorizeBg(Color.Named.Yellow)
  final def blueBg: Text = this.colorizeBg(Color.Named.Blue)
  final def magentaBg: Text = this.colorizeBg(Color.Named.Magenta)
  final def cyanBg: Text = this.colorizeBg(Color.Named.Cyan)
  final def whiteBg: Text = this.colorizeBg(Color.Named.White)
  final def resetBg: Text = this.colorizeBg(Color.Default)

  final def resetFgBg: Text = this.colorizeFgBg(Color.Default, Color.Default)

  /////// Internal ///////////////////////////////////////////////////////////////

  def textWriteSimple(cfg: Text.Config, builder: StringBuilder): Unit
  def textWriteComplex(cfg: Text.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit

  final def textBuildRootSimple(cfg: Text.Config): String =
    StringBuilder.makeString(this.textWriteSimple(cfg, _))
  final def textBuildRootComplex(cfg: Text.Config, currentIndent: String, colorState: ColorStateV2): String =
    StringBuilder.makeString(this.textWriteComplex(cfg, _, currentIndent, colorState))

  final def toStringBlank: String = textBuildRootSimple(Text.Config.defaultBlank)
  final def toStringBraced: String = textBuildRootSimple(Text.Config.defaultBraced)
  override final def toString: String = toStringBlank

}
object Text {

  /**
    * Use this where you would like to be able to automatically convert from [[String]] => [[Text]].
    * Note: This should primarily only be used for inputs, and not return values.
    */
  opaque type Auto <: Text = Text
  object Auto {
    given Conversion[Text, Text.Auto] = ConversionUtils.id[Text]
    given Conversion[Showable, Text.Auto] = _.show
    given Conversion[String, Text.Auto] = Text.fromString(_)
  }

  // =====|  |=====

  val empty: Text = Text.impl.Empty
  val newLine: Text = Text.impl.Newline

  def fromString(value: String): Text = value match
    case null => Text.impl.Str("null")
    case _    => Text.impl.Str(value)

  def fromAny(value: Any): Text = value.asInstanceOf[Matchable] match
    case null                           => Text.impl.Str("null")
    case value: Text                    => value
    case value: String if value.isEmpty => Text.impl.Empty
    case value: String                  => Text.impl.Str(value)
    case _                              => Text.impl.Str(value.toString)

  /**
    * myInterp"abc${hi}def"
    * call this for `abc` and `def`
    * it handles strange scala un-escape behavior, as well as multiline |
    */
  def stringContextConst(str: String): Text =
    if str.exists { c => c == '\\' || c == '\n' } then Text.impl.InterpolatedConstStr(str)
    else Text.impl.Str(str, false)

  def when(cond: Boolean)(str: => Text): Text =
    if cond then str
    else Text.empty

  def foreach[S[_]: SeqRead as seqRead, A](in: S[A])(f: A => Text): Text = {
    val builder = ArraySeq.newBuilder[Text]
    builder.sizeHint(seqRead.knownSize(in))
    seqRead.newIterator(in).foreach { a => builder.addOne(f(a)) }
    Text.impl.Concat.ManyLazyStrings(SeqRead.arraySeq, builder.result())
  }

  def foreachJoined[S[_]: SeqRead as seqRead, A](in: S[A], join: Text.Auto)(f: A => Text): Text = {
    val builder = ArraySeq.newBuilder[Text]
    builder.sizeHint(seqRead.knownSize(in))
    seqRead.newIterator(in).foreach { a => builder.addOne(f(a)) }
    Text.mkString(builder.result(), join)
  }

  def foreachJoinedWithIndex[S[_]: SeqRead as seqRead, A](in: S[A], join: Text.Auto)(f: (A, Int) => Text): Text = {
    val builder = ArraySeq.newBuilder[Text]
    builder.sizeHint(seqRead.knownSize(in))
    var idx: Int = 0
    seqRead.newIterator(in).foreach { a =>
      builder.addOne(f(a, idx))
      idx = idx + 1
    }
    Text.mkString(builder.result(), join)
  }

  def mkString[S[_]: SeqRead as seqRead](in: S[Text]): Text = Text.impl.Concat.ManyLazyStrings(seqRead, in)
  def mkString[S[_]: SeqRead as seqRead](in: S[Text], join: Text.Auto): Text = mkString(in, empty, join, empty)
  def mkString[S[_]: SeqRead as seqRead](in: S[Text], prefix: Text.Auto, join: Text.Auto, suffix: Text.Auto): Text =
    Text.impl.Concat.SurroundLazyStrings(seqRead, in, prefix, join, suffix)

  def interpolateStrInternal(sc: Expr[StringContext], args: Expr[Seq[Text.Auto]])(using Quotes): Expr[Text] = {
    val scPos: Position = sc.toTerm.pos
    val isMultiLine: Boolean = scPos.startLine != scPos.endLine

    '{ Text.interpolateStr($sc, $args, ${ Expr(isMultiLine) }) }
  }

  def interpolateStr(sc: StringContext, args: Seq[Text.Auto], multiline: Boolean): Text = {
    if sc.parts.length == 1 then {
      val tmpStr: String = sc.parts.head
      if multiline then Text.stringContextConst(tmpStr)
      else Text.stringContextConst(tmpStr)
    } else {
      val tmpStrings: IArray[String] = IArray.from(sc.parts)

      Text.impl.Interpolated(
        strings =
          if multiline then tmpStrings.map(Text.stringContextConst)
          else tmpStrings.map(Text.stringContextConst),
        args = IArray.from(args),
      )
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Config / State
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  enum IndentPrefixMode {
    case NewlineAndIndent
    case IndentOnly
    case Empty
  }

  enum IndentType {
    case Default
    case Custom(indent: Text)
    case CustomInitial(indent: Text)
  }

  final case class Config private[Text] (
      colorMode: ColorMode,
      rootDefaultIndents: NonEmptyList[Text],
      indentQueue: NonEmptyList[Text],
  ) {

    def popIndent: (Text, Config) = indentQueue.tail match
      case h :: t => (indentQueue.head, copy(indentQueue = NonEmptyList(h, t)))
      case Nil    => (indentQueue.head, copy(indentQueue = rootDefaultIndents))

  }
  object Config {

    def make(colorMode: ColorMode, indents: NonEmptyList[Text]): Config =
      Config(colorMode, indents, indents)
    def make(colorMode: ColorMode, indent0: Text.Auto, indentN: Text.Auto*): Config =
      Config.make(colorMode, NonEmptyList(indent0, indentN.toList))

    def make(colorMode: ColorMode, base: Text.Auto, color0: Color, colorN: Color*): Config =
      Config.make(colorMode, NonEmptyList(color0, colorN.toList).map(c => base.colorizeFg(fg = c)))

    val defaultBlank: Config = Config.make(ColorMode.Extended, "   ")
    val defaultBraced: Config = Config.make(ColorMode.Extended, "|   ")

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Impl
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * If you extend this you better do it right...
    */
  private[oxygen] trait UnsafeCustom extends Text

  private object impl {

    case object Empty extends Text {
      override def textWriteSimple(cfg: Text.Config, builder: StringBuilder): Unit = ()
      override def textWriteComplex(cfg: Text.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = ()
    }

    case object Newline extends Text {

      override def textWriteSimple(cfg: Text.Config, builder: StringBuilder): Unit =
        builder.append('\n')

      override def textWriteComplex(cfg: Text.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
        builder.append(colorState.revert)
        builder.append('\n')
        builder.append(currentIndent)
        builder.append(colorState.apply)
      }

    }

    final case class Str(value: String, unknownNewline: Boolean = true) extends Text {

      override def textWriteSimple(cfg: Text.Config, builder: StringBuilder): Unit =
        builder.append(value)

      override def textWriteComplex(cfg: Text.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit =
        if unknownNewline && value.contains('\n') then
          value.foreach {
            case '\n' => Newline.textWriteComplex(cfg, builder, currentIndent, colorState)
            case c    => builder.append(c)
          }
        else
          builder.append(value)

    }

    final case class InterpolatedConstStr(value: String) extends Text {

      private def shared(builder: StringBuilder, onNewLine: () => Unit): Unit = {
        val len: Int = value.length
        var idx: Int = 0
        while idx < len do {
          value(idx) match {
            case '\\' =>
              idx = idx + 1
              if idx < len then
                value(idx) match {
                  case 'n' => onNewLine()
                  case 't' => builder.append('\t')
                  case 'r' => builder.append('\r')
                  case 'b' => builder.append('\b')
                  case 'f' => builder.append('\f')
                  case c   => builder.append(c)
                }
              else
                builder.append('\\')
              idx = idx + 1
            case '\n' =>
              idx = idx + 1
              var tmpIdx: Int = idx
              var loop: Boolean = true

              onNewLine()

              while loop do
                if tmpIdx < len then
                  value(tmpIdx) match {
                    case ' ' | '\t' =>
                      tmpIdx = tmpIdx + 1
                    case '|' =>
                      idx = tmpIdx + 1
                      loop = false
                    case _ =>
                      loop = false
                      while idx <= tmpIdx do {
                        builder.append(value(idx))
                        idx = idx + 1
                      }
                  }
                else {
                  loop = false
                  while idx < tmpIdx do {
                    builder.append(value(idx))
                    idx = idx + 1
                  }
                }
            case c =>
              builder.append(c)
              idx = idx + 1
          }
        }
      }

      override def textWriteSimple(cfg: Text.Config, builder: StringBuilder): Unit =
        shared(builder, () => builder.append('\n'))

      override def textWriteComplex(cfg: Text.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit =
        shared(builder, () => Newline.textWriteComplex(cfg, builder, currentIndent, colorState))

    }

    final case class When(underlying: Text, cond: Boolean) extends Text {

      override def textWriteSimple(cfg: Config, builder: StringBuilder): Unit =
        if cond then underlying.textWriteSimple(cfg, builder)

      override def textWriteComplex(cfg: Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit =
        if cond then underlying.textWriteComplex(cfg, builder, currentIndent, colorState)

    }

    final case class Interpolated(strings: IArray[Text], args: IArray[Text]) extends Text {

      override def textWriteSimple(cfg: Text.Config, builder: StringBuilder): Unit = {
        val size: Int = args.length
        var _idx: Int = 0
        while _idx < size do {
          strings(_idx).textWriteSimple(cfg, builder)
          args(_idx).textWriteSimple(cfg, builder)
          _idx = _idx + 1
        }
        strings(_idx).textWriteSimple(cfg, builder)
      }

      override def textWriteComplex(cfg: Text.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
        val size: Int = args.length
        var _idx: Int = 0
        while _idx < size do {
          strings(_idx).textWriteComplex(cfg, builder, currentIndent, colorState)
          args(_idx).textWriteComplex(cfg, builder, currentIndent, colorState)
          _idx = _idx + 1
        }
        strings(_idx).textWriteComplex(cfg, builder, currentIndent, colorState)
      }

    }

    sealed trait Indented extends Text {
      val underlying: Text
      val indent: IndentType

      protected final def calculateSimpleIndentInternal(cfg: Text.Config): (idt0: String, idtN: String, newCfg: Config) =
        indent match {
          case IndentType.Default =>
            val (poppedIdt, newCfg) = cfg.popIndent
            val newIdt = poppedIdt.textBuildRootSimple(cfg)
            (newIdt, newIdt, newCfg)
          case IndentType.Custom(indent) =>
            val newIdt = indent.textBuildRootSimple(cfg)
            (newIdt, newIdt, cfg)
          case IndentType.CustomInitial(indent) =>
            val initialIdt = indent.textBuildRootSimple(cfg)
            val restIdt = " " * initialIdt.length // TODO (KR) : might be worth evaluating `indent` without color
            (initialIdt, restIdt, cfg)
        }

      protected final def calculateComplexIndentInternal(cfg: Text.Config, currentIndent: String, colorState: ColorStateV2): (idt0: String, idtN: String, newCfg: Config) =
        indent match {
          case IndentType.Default =>
            val (poppedIdt, newCfg) = cfg.popIndent
            val newIdt = currentIndent + poppedIdt.textBuildRootComplex(cfg, currentIndent, colorState)
            (newIdt, newIdt, newCfg)
          case IndentType.Custom(indent) =>
            val newIdt = currentIndent + indent.textBuildRootComplex(cfg, currentIndent, colorState)
            (newIdt, newIdt, cfg)
          case IndentType.CustomInitial(indent) =>
            val tmp = indent.textBuildRootComplex(cfg, currentIndent, colorState)
            val initialIdt = currentIndent + tmp
            val restIdt = currentIndent + (" " * tmp.length)
            (initialIdt, restIdt, cfg)
        }

    }
    object Indented {

      def apply(underlying: Text, leading: IndentPrefixMode, indent: IndentType): Indented = leading match
        case IndentPrefixMode.NewlineAndIndent => NewlineAndIndent(underlying, indent)
        case IndentPrefixMode.IndentOnly       => IndentOnly(underlying, indent)
        case IndentPrefixMode.Empty            => Empty(underlying, indent)

      final case class NewlineAndIndent(underlying: Text, indent: IndentType) extends Indented {

        override def textWriteSimple(cfg: Text.Config, builder: StringBuilder): Unit = {
          val next = this.calculateSimpleIndentInternal(cfg)
          val tmpBuilder = StringBuilder.empty
          underlying.textWriteComplex(next.newCfg, tmpBuilder, next.idtN, ColorStateV2.Empty)
          if tmpBuilder.nonEmpty() then {
            Newline.textWriteComplex(next.newCfg, builder, next.idt0, ColorStateV2.Empty)
            builder.append(tmpBuilder)
          }
        }

        override def textWriteComplex(cfg: Text.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
          val next = this.calculateComplexIndentInternal(cfg, currentIndent, colorState)
          val tmpBuilder = StringBuilder.empty
          underlying.textWriteComplex(next.newCfg, tmpBuilder, next.idtN, colorState)
          if tmpBuilder.nonEmpty() then {
            Newline.textWriteComplex(next.newCfg, builder, next.idt0, colorState)
            builder.append(tmpBuilder)
          }
        }

      }

      final case class IndentOnly(underlying: Text, indent: IndentType) extends Indented {

        override def textWriteSimple(cfg: Text.Config, builder: StringBuilder): Unit = {
          val next = this.calculateSimpleIndentInternal(cfg)
          builder.append(next.idt0)
          underlying.textWriteComplex(next.newCfg, builder, next.idtN, ColorStateV2.Empty)
        }

        override def textWriteComplex(cfg: Text.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
          val next = this.calculateComplexIndentInternal(cfg, currentIndent, colorState)
          builder.append(next.idt0)
          underlying.textWriteComplex(next.newCfg, builder, next.idtN, colorState)
        }

      }

      final case class Empty(underlying: Text, indent: IndentType) extends Indented {

        override def textWriteSimple(cfg: Text.Config, builder: StringBuilder): Unit = {
          val next = this.calculateSimpleIndentInternal(cfg)
          underlying.textWriteComplex(next.newCfg, builder, next.idtN, ColorStateV2.Empty)
        }

        override def textWriteComplex(cfg: Text.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
          val next = this.calculateComplexIndentInternal(cfg, currentIndent, colorState)
          underlying.textWriteComplex(next.newCfg, builder, next.idtN, colorState)
        }

      }

    }

    sealed trait Colorize extends Text {

      protected val underlying: Text
      protected def patch(colorMode: ColorMode, current: ColorStateV2): Option[ColorStateV2.Patch]

      override final def textWriteSimple(cfg: Text.Config, builder: StringBuilder): Unit =
        patch(cfg.colorMode, ColorStateV2.Empty) match {
          case Some(patch) =>
            builder.append(patch.apply)
            underlying.textWriteComplex(cfg, builder, "", patch.newState)
            builder.append(patch.revert)
          case None =>
            underlying.textWriteSimple(cfg, builder)
        }

      override final def textWriteComplex(cfg: Text.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit =
        patch(cfg.colorMode, colorState) match {
          case Some(patch) =>
            builder.append(patch.apply)
            underlying.textWriteComplex(cfg, builder, currentIndent, patch.newState)
            builder.append(patch.revert)
          case None =>
            underlying.textWriteComplex(cfg, builder, currentIndent, colorState)
        }

    }
    object Colorize {

      final case class ColorizeFg(underlying: Text, fg: Color) extends Colorize {
        override protected def patch(colorMode: ColorMode, current: ColorStateV2): Option[ColorStateV2.Patch] = current.patchFg(colorMode, fg)
      }

      final case class ColorizeBg(underlying: Text, bg: Color) extends Colorize {
        override protected def patch(colorMode: ColorMode, current: ColorStateV2): Option[ColorStateV2.Patch] = current.patchBg(colorMode, bg)
      }

      final case class ColorizeFgBg(underlying: Text, fg: Color, bg: Color) extends Colorize {
        override protected def patch(colorMode: ColorMode, current: ColorStateV2): Option[ColorStateV2.Patch] = current.patchFgBg(colorMode, fg, bg)
      }

    }

    final case class FlatMap[S[_], A](seqOps: SeqRead[S], underlying: S[A], f: A => Text) extends Text {

      override def textWriteSimple(cfg: Config, builder: StringBuilder): Unit = {
        val iter = seqOps.newIterator(underlying)
        while iter.hasNext do f(iter.next()).textWriteSimple(cfg, builder)
      }

      override def textWriteComplex(cfg: Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
        val iter = seqOps.newIterator(underlying)
        while iter.hasNext do f(iter.next()).textWriteComplex(cfg, builder, currentIndent, colorState)
      }

    }

    object Concat {

      final case class _2(first: Text, second: Text) extends Text {

        override def textWriteSimple(cfg: Text.Config, builder: StringBuilder): Unit = {
          first.textWriteSimple(cfg, builder)
          second.textWriteSimple(cfg, builder)
        }

        override def textWriteComplex(cfg: Text.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
          first.textWriteComplex(cfg, builder, currentIndent, colorState)
          second.textWriteComplex(cfg, builder, currentIndent, colorState)
        }

      }

      final case class ManyLazyStrings[S[_]](seqOps: SeqRead[S], underlying: S[Text]) extends Text {

        override def textWriteSimple(cfg: Text.Config, builder: StringBuilder): Unit = {
          val iter = seqOps.newIterator(underlying)
          while iter.hasNext do iter.next().textWriteSimple(cfg, builder)
        }

        override def textWriteComplex(cfg: Text.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
          val iter = seqOps.newIterator(underlying)
          while iter.hasNext do iter.next().textWriteComplex(cfg, builder, currentIndent, colorState)
        }

      }

      final case class SurroundLazyStrings[S[_]](seqOps: SeqRead[S], underlying: S[Text], prefix: Text, join: Text, suffix: Text) extends Text {

        override def textWriteSimple(cfg: Text.Config, builder: StringBuilder): Unit = {
          val iter = seqOps.newIterator(underlying)
          val renderedJoin = join.textBuildRootSimple(cfg)

          prefix.textWriteSimple(cfg, builder)

          if iter.hasNext then
            iter.next().textWriteSimple(cfg, builder)

          while iter.hasNext do {
            builder.append(renderedJoin)
            iter.next().textWriteSimple(cfg, builder)
          }

          suffix.textWriteSimple(cfg, builder)
        }

        override def textWriteComplex(cfg: Text.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
          val renderedJoin = join.textBuildRootComplex(cfg, currentIndent, colorState)

          prefix.textWriteComplex(cfg, builder, currentIndent, colorState)

          val iter = seqOps.newIterator(underlying)
          if iter.hasNext then
            iter.next().textWriteComplex(cfg, builder, currentIndent, colorState)

          while iter.hasNext do {
            builder.append(renderedJoin)
            iter.next().textWriteComplex(cfg, builder, currentIndent, colorState)
          }

          suffix.textWriteComplex(cfg, builder, currentIndent, colorState)
        }

      }

    }

  }

}

extension (inline sc: StringContext)
  inline def str(inline args: Text.Auto*): Text = ${ Text.interpolateStrInternal('sc, 'args) }
