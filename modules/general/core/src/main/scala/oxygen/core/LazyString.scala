package oxygen.core

import oxygen.core.collection.NonEmptyList
import oxygen.core.typeclass.SeqRead

sealed trait LazyString {

  final def ++(that: LazyString.Auto): LazyString = LazyString.impl.Concat._2(this, that)
  final def |>(that: LazyString.Auto): LazyString = this ++ (LazyString.impl.Newline ++ that).indented

  final def indented(idt: LazyString.Auto): LazyString = LazyString.impl.CustomIndented(this, idt)
  final def indented: LazyString = LazyString.impl.DefaultIndented(this)

  final def colorizeFg(fg: Specified[Color] = ___): LazyString = fg.fold(this)(LazyString.impl.Colorize.ColorizeFg(this, _))
  final def colorizeBg(bg: Specified[Color] = ___): LazyString = bg.fold(this)(LazyString.impl.Colorize.ColorizeBg(this, _))
  final def colorizeFgBg(fg: Specified[Color] = ___, bg: Specified[Color] = ___): LazyString = (fg, bg) match
    case (Specified.WasSpecified(fg), Specified.WasSpecified(bg)) => LazyString.impl.Colorize.ColorizeFgBg(this, fg, bg)
    case (Specified.WasSpecified(fg), Specified.WasNotSpecified)  => LazyString.impl.Colorize.ColorizeFg(this, fg)
    case (Specified.WasNotSpecified, Specified.WasSpecified(bg))  => LazyString.impl.Colorize.ColorizeBg(this, bg)
    case (Specified.WasNotSpecified, Specified.WasNotSpecified)   => this

  /////// Internal ///////////////////////////////////////////////////////////////
  def showableStringBuilderWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit
  def showableStringBuilderWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit

  // FIX-PRE-MERGE (KR) : rename
  final def buildNowSimple(cfg: LazyString.Config): String =
    StringBuilder.makeString(this.showableStringBuilderWriteSimple(cfg, _))
  final def buildNowComplex(cfg: LazyString.Config, currentIndent: String, colorState: ColorStateV2): String =
    StringBuilder.makeString(this.showableStringBuilderWriteComplex(cfg, _, currentIndent, colorState))

  override final def toString: String = buildNowSimple(LazyString.Config.default)

}
object LazyString {

  /**
    * Use this where you would like to be able to automatically convert from [[String]] => [[LazyString]].
    * Note: This should primarily only be used for inputs, and not return values.
    */
  opaque type Auto <: LazyString = LazyString
  object Auto {
    given Conversion[LazyString, LazyString.Auto] = ConversionUtils.id[LazyString]
    given Conversion[String, LazyString.Auto] = LazyString.fromString(_)
  }

  // =====|  |=====

  val empty: LazyString = LazyString.impl.Empty
  val newLine: LazyString = LazyString.impl.Newline

  def fromString(value: String): LazyString = value match
    case null => LazyString.impl.Str("null")
    case _    => LazyString.impl.Str(value)

  def fromAny(value: Any): LazyString = value.asInstanceOf[Matchable] match
    case null                           => LazyString.impl.Str("null")
    case value: LazyString              => value
    case value: String if value.isEmpty => LazyString.impl.Empty
    case value: String                  => LazyString.impl.Str(value)
    case _                              => LazyString.impl.Str(value.toString)

  def foreach[S[_]: SeqRead as seqRead, A](in: S[A])(f: A => LazyString): LazyString = ???

  def mkString[S[_]: SeqRead as seqRead](in: S[LazyString]): LazyString = LazyString.impl.Concat.ManyLazyStrings(seqRead, in)
  def mkString[S[_]: SeqRead as seqRead](in: S[LazyString], join: LazyString.Auto): LazyString = mkString(in, empty, join, empty)
  def mkString[S[_]: SeqRead as seqRead](in: S[LazyString], prefix: LazyString.Auto, join: LazyString.Auto, suffix: LazyString.Auto): LazyString =
    LazyString.impl.Concat.SurroundLazyStrings(seqRead, in, prefix, join, suffix)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Config / State
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Config private[LazyString] (
      colorMode: ColorMode,
      rootDefaultIndents: NonEmptyList[LazyString],
      indentQueue: NonEmptyList[LazyString],
  ) {

    def popIndent: (LazyString, Config) = indentQueue.tail match
      case h :: t => (indentQueue.head, copy(indentQueue = NonEmptyList(h, t)))
      case Nil    => (indentQueue.head, copy(indentQueue = rootDefaultIndents))

  }
  object Config {

    def make(colorMode: ColorMode, indents: NonEmptyList[LazyString]): Config =
      Config(colorMode, indents, indents)
    def make(colorMode: ColorMode, indent0: LazyString.Auto, indentN: LazyString.Auto*): Config =
      Config.make(colorMode, NonEmptyList(indent0, indentN.toList))

    def make(colorMode: ColorMode, base: LazyString.Auto, color0: Color, colorN: Color*): Config =
      Config.make(colorMode, NonEmptyList(color0, colorN.toList).map(c => base.colorizeFg(fg = c)))

    val default: Config = Config.make(ColorMode.Extended, "|    ")

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Impl
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * If you extend this you better do it right...
    */
  private[oxygen] trait UnsafeCustom extends LazyString

  private object impl {

    case object Empty extends LazyString {
      override def showableStringBuilderWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = ()
      override def showableStringBuilderWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = ()
    }

    case object Newline extends LazyString {

      override def showableStringBuilderWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit =
        builder.append('\n')

      override def showableStringBuilderWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
        builder.append(colorState.revert)
        builder.append('\n')
        builder.append(currentIndent)
        builder.append(colorState.apply)
      }

    }

    final case class Str(value: String) extends LazyString {

      override def showableStringBuilderWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit =
        builder.append(value)

      override def showableStringBuilderWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit =
        if value.contains('\n') then
          value.foreach {
            case '\n' => Newline.showableStringBuilderWriteComplex(cfg, builder, currentIndent, colorState)
            case c    => builder.append(c)
          }
        else
          builder.append(value)

    }

    final case class Interpolated(strings: IArray[String], args: IArray[LazyString]) extends LazyString {

      override def showableStringBuilderWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
        val size: Int = args.length
        var _idx: Int = 0
        while _idx < size do {
          builder.append(strings(_idx))
          args(_idx).showableStringBuilderWriteSimple(cfg, builder)
          _idx = _idx + 1
        }
        builder.append(strings(_idx))
      }

      override def showableStringBuilderWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
        val size: Int = args.length
        var _idx: Int = 0
        while _idx < size do {
          Str(strings(_idx)).showableStringBuilderWriteComplex(cfg, builder, currentIndent, colorState)
          args(_idx).showableStringBuilderWriteComplex(cfg, builder, currentIndent, colorState)
          _idx = _idx + 1
        }
        Str(strings(_idx)).showableStringBuilderWriteComplex(cfg, builder, currentIndent, colorState)
      }

    }

    final case class CustomIndented(underlying: LazyString, indent: LazyString) extends LazyString {

      override def showableStringBuilderWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit =
        underlying.showableStringBuilderWriteComplex(cfg, builder, indent.buildNowSimple(cfg), ColorStateV2.Empty)

      override def showableStringBuilderWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit =
        underlying.showableStringBuilderWriteComplex(cfg, builder, currentIndent + indent.buildNowComplex(cfg, currentIndent, colorState), colorState)

    }

    final case class DefaultIndented(underlying: LazyString) extends LazyString {

      override def showableStringBuilderWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
        val (indent, newCfg) = cfg.popIndent
        underlying.showableStringBuilderWriteComplex(newCfg, builder, indent.buildNowSimple(cfg), ColorStateV2.Empty)
      }

      override def showableStringBuilderWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
        val (indent, newCfg) = cfg.popIndent
        underlying.showableStringBuilderWriteComplex(newCfg, builder, currentIndent + indent.buildNowComplex(cfg, currentIndent, colorState), colorState)
      }

    }

    sealed trait Colorize extends LazyString {

      protected val underlying: LazyString
      protected def patch(colorMode: ColorMode, current: ColorStateV2): Option[ColorStateV2.Patch]

      override final def showableStringBuilderWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit =
        patch(cfg.colorMode, ColorStateV2.Empty) match {
          case Some(patch) =>
            builder.append(patch.apply)
            underlying.showableStringBuilderWriteComplex(cfg, builder, "", patch.newState)
            builder.append(patch.revert)
          case None =>
            underlying.showableStringBuilderWriteSimple(cfg, builder)
        }

      override final def showableStringBuilderWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit =
        patch(cfg.colorMode, colorState) match {
          case Some(patch) =>
            builder.append(patch.apply)
            underlying.showableStringBuilderWriteComplex(cfg, builder, currentIndent, patch.newState)
            builder.append(patch.revert)
          case None =>
            underlying.showableStringBuilderWriteComplex(cfg, builder, currentIndent, colorState)
        }

    }
    object Colorize {

      final case class ColorizeFg(underlying: LazyString, fg: Color) extends Colorize {
        override protected def patch(colorMode: ColorMode, current: ColorStateV2): Option[ColorStateV2.Patch] = current.patchFg(colorMode, fg)
      }

      final case class ColorizeBg(underlying: LazyString, bg: Color) extends Colorize {
        override protected def patch(colorMode: ColorMode, current: ColorStateV2): Option[ColorStateV2.Patch] = current.patchBg(colorMode, bg)
      }

      final case class ColorizeFgBg(underlying: LazyString, fg: Color, bg: Color) extends Colorize {
        override protected def patch(colorMode: ColorMode, current: ColorStateV2): Option[ColorStateV2.Patch] = current.patchFgBg(colorMode, fg, bg)
      }

    }

    final case class FlatMap[S[_], A](seqOps: SeqRead[S], underlying: S[A], f: A => LazyString) extends LazyString {

      override def showableStringBuilderWriteSimple(cfg: Config, builder: StringBuilder): Unit = ???

      override def showableStringBuilderWriteComplex(cfg: Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = ???

    }

    object Concat {

      final case class _2(first: LazyString, second: LazyString) extends LazyString {

        override def showableStringBuilderWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
          first.showableStringBuilderWriteSimple(cfg, builder)
          second.showableStringBuilderWriteSimple(cfg, builder)
        }

        override def showableStringBuilderWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
          first.showableStringBuilderWriteComplex(cfg, builder, currentIndent, colorState)
          second.showableStringBuilderWriteComplex(cfg, builder, currentIndent, colorState)
        }

      }

      final case class ManyLazyStrings[S[_]](seqOps: SeqRead[S], underlying: S[LazyString]) extends LazyString {

        override def showableStringBuilderWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
          val iter = seqOps.newIterator(underlying)
          while iter.hasNext do iter.next().showableStringBuilderWriteSimple(cfg, builder)
        }

        override def showableStringBuilderWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
          val iter = seqOps.newIterator(underlying)
          while iter.hasNext do iter.next().showableStringBuilderWriteComplex(cfg, builder, currentIndent, colorState)
        }

      }

      final case class SurroundLazyStrings[S[_]](seqOps: SeqRead[S], underlying: S[LazyString], prefix: LazyString, join: LazyString, suffix: LazyString) extends LazyString {

        override def showableStringBuilderWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
          val iter = seqOps.newIterator(underlying)
          val renderedJoin = join.buildNowSimple(cfg)

          prefix.showableStringBuilderWriteSimple(cfg, builder)

          if iter.hasNext then
            iter.next().showableStringBuilderWriteSimple(cfg, builder)

          while iter.hasNext do {
            builder.append(renderedJoin)
            iter.next().showableStringBuilderWriteSimple(cfg, builder)
          }

          suffix.showableStringBuilderWriteSimple(cfg, builder)
        }

        override def showableStringBuilderWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
          val renderedJoin = join.buildNowComplex(cfg, currentIndent, colorState)

          prefix.showableStringBuilderWriteComplex(cfg, builder, currentIndent, colorState)

          val iter = seqOps.newIterator(underlying)
          if iter.hasNext then
            iter.next().showableStringBuilderWriteComplex(cfg, builder, currentIndent, colorState)

          while iter.hasNext do {
            builder.append(renderedJoin)
            iter.next().showableStringBuilderWriteComplex(cfg, builder, currentIndent, colorState)
          }

          suffix.showableStringBuilderWriteComplex(cfg, builder, currentIndent, colorState)

        }

      }

    }

  }

}
