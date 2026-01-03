package oxygen.core

import oxygen.core.collection.NonEmptyList
import oxygen.core.typeclass.{SeqRead, Showable}
import oxygen.quoted.*
import scala.collection.immutable.ArraySeq
import scala.quoted.*

sealed trait LazyString {

  final def +(that: LazyString.Auto): LazyString = LazyString.impl.Concat._2(this, that)
  final def ++(that: LazyString.Auto): LazyString = LazyString.impl.Concat._2(this, that)
  final def |>(that: LazyString.Auto): LazyString = this ++ that.indented

  final def indentedInitial(idt: LazyString.Auto): LazyString = LazyString.impl.CustomIndentedInitial(this, idt)
  final def indented(idt: LazyString.Auto): LazyString = LazyString.impl.CustomIndented(this, idt)
  final def indented: LazyString = LazyString.impl.DefaultIndented(this)

  final def when(cond: Boolean): LazyString = LazyString.impl.When(this, cond)
  final def unless(cond: Boolean): LazyString = LazyString.impl.When(this, !cond)

  final def colorizeFg(fg: Specified[Color] = ___): LazyString = fg.fold(this)(LazyString.impl.Colorize.ColorizeFg(this, _))
  final def colorizeBg(bg: Specified[Color] = ___): LazyString = bg.fold(this)(LazyString.impl.Colorize.ColorizeBg(this, _))
  final def colorizeFgBg(fg: Specified[Color] = ___, bg: Specified[Color] = ___): LazyString = (fg, bg) match
    case (Specified.WasSpecified(fg), Specified.WasSpecified(bg)) => LazyString.impl.Colorize.ColorizeFgBg(this, fg, bg)
    case (Specified.WasSpecified(fg), Specified.WasNotSpecified)  => LazyString.impl.Colorize.ColorizeFg(this, fg)
    case (Specified.WasNotSpecified, Specified.WasSpecified(bg))  => LazyString.impl.Colorize.ColorizeBg(this, bg)
    case (Specified.WasNotSpecified, Specified.WasNotSpecified)   => this

  final def blackFg: LazyString = this.colorizeFg(Color.Named.Black)
  final def redFg: LazyString = this.colorizeFg(Color.Named.Red)
  final def greenFg: LazyString = this.colorizeFg(Color.Named.Green)
  final def yellowFg: LazyString = this.colorizeFg(Color.Named.Yellow)
  final def blueFg: LazyString = this.colorizeFg(Color.Named.Blue)
  final def magentaFg: LazyString = this.colorizeFg(Color.Named.Magenta)
  final def cyanFg: LazyString = this.colorizeFg(Color.Named.Cyan)
  final def whiteFg: LazyString = this.colorizeFg(Color.Named.White)
  final def resetFg: LazyString = this.colorizeFg(Color.Default)

  final def blackBg: LazyString = this.colorizeBg(Color.Named.Black)
  final def redBg: LazyString = this.colorizeBg(Color.Named.Red)
  final def greenBg: LazyString = this.colorizeBg(Color.Named.Green)
  final def yellowBg: LazyString = this.colorizeBg(Color.Named.Yellow)
  final def blueBg: LazyString = this.colorizeBg(Color.Named.Blue)
  final def magentaBg: LazyString = this.colorizeBg(Color.Named.Magenta)
  final def cyanBg: LazyString = this.colorizeBg(Color.Named.Cyan)
  final def whiteBg: LazyString = this.colorizeBg(Color.Named.White)
  final def resetBg: LazyString = this.colorizeBg(Color.Default)

  final def resetFgBg: LazyString = this.colorizeFgBg(Color.Default, Color.Default)

  /////// Internal ///////////////////////////////////////////////////////////////
  def lazyStringWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit
  def lazyStringWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit

  final def lazyStringBuildRootSimple(cfg: LazyString.Config): String =
    StringBuilder.makeString(this.lazyStringWriteSimple(cfg, _))
  final def lazyStringBuildRootComplex(cfg: LazyString.Config, currentIndent: String, colorState: ColorStateV2): String =
    StringBuilder.makeString(this.lazyStringWriteComplex(cfg, _, currentIndent, colorState))

  override final def toString: String = lazyStringBuildRootSimple(LazyString.Config.default)

}
object LazyString {

  /**
    * Use this where you would like to be able to automatically convert from [[String]] => [[LazyString]].
    * Note: This should primarily only be used for inputs, and not return values.
    */
  opaque type Auto <: LazyString = LazyString
  object Auto {
    given Conversion[LazyString, LazyString.Auto] = ConversionUtils.id[LazyString]
    given Conversion[Showable, LazyString.Auto] = _.show
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

  def when(cond: Boolean)(str: => LazyString): LazyString =
    if cond then str
    else LazyString.empty

  def foreach[S[_]: SeqRead as seqRead, A](in: S[A])(f: A => LazyString): LazyString = {
    val builder = ArraySeq.newBuilder[LazyString]
    builder.sizeHint(seqRead.knownSize(in))
    seqRead.newIterator(in).foreach { a => builder.addOne(f(a)) }
    LazyString.impl.Concat.ManyLazyStrings(SeqRead.arraySeq, builder.result())
  }

  def foreachJoined[S[_]: SeqRead as seqRead, A](in: S[A], join: LazyString.Auto)(f: A => LazyString): LazyString = {
    val builder = ArraySeq.newBuilder[LazyString]
    builder.sizeHint(seqRead.knownSize(in))
    seqRead.newIterator(in).foreach { a => builder.addOne(f(a)) }
    LazyString.mkString(builder.result(), join)
  }

  def foreachJoinedWithIndex[S[_]: SeqRead as seqRead, A](in: S[A], join: LazyString.Auto)(f: (A, Int) => LazyString): LazyString = {
    val builder = ArraySeq.newBuilder[LazyString]
    builder.sizeHint(seqRead.knownSize(in))
    var idx: Int = 0
    seqRead.newIterator(in).foreach { a =>
      builder.addOne(f(a, idx))
      idx = idx + 1
    }
    LazyString.mkString(builder.result(), join)
  }

  def mkString[S[_]: SeqRead as seqRead](in: S[LazyString]): LazyString = LazyString.impl.Concat.ManyLazyStrings(seqRead, in)
  def mkString[S[_]: SeqRead as seqRead](in: S[LazyString], join: LazyString.Auto): LazyString = mkString(in, empty, join, empty)
  def mkString[S[_]: SeqRead as seqRead](in: S[LazyString], prefix: LazyString.Auto, join: LazyString.Auto, suffix: LazyString.Auto): LazyString =
    LazyString.impl.Concat.SurroundLazyStrings(seqRead, in, prefix, join, suffix)

  def interpolateStrInternal(sc: Expr[StringContext], args: Expr[Seq[LazyString.Auto]])(using Quotes): Expr[LazyString] = {
    val scPos: Position = sc.toTerm.pos
    val isMultiLine: Boolean = scPos.startLine != scPos.endLine

    '{ LazyString.interpolateStr($sc, $args, ${ Expr(isMultiLine) }) }
  }

  private val multilineReg: String = "\n[ \t]+\\|".r.toString
  def interpolateStr(sc: StringContext, args: Seq[LazyString.Auto], multiline: Boolean): LazyString = {
    if sc.parts.length == 1 then {
      val tmpStr: String = StringContext.processEscapes(sc.parts.head)
      if multiline then LazyString.impl.Str(tmpStr.replaceAll(multilineReg, "\n"))
      else LazyString.impl.Str(tmpStr)
    } else {
      val tmpStrings: IArray[String] = IArray.from(sc.parts)

      LazyString.impl.Interpolated(
        strings =
          if multiline then tmpStrings.map(str => StringContext.processEscapes(str).replaceAll(multilineReg, "\n"))
          else tmpStrings,
        args = IArray.from(args),
      )
    }
  }

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

    val default: Config = Config.make(ColorMode.Extended, "|   ")

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
      override def lazyStringWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = ()
      override def lazyStringWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = ()
    }

    case object Newline extends LazyString {

      override def lazyStringWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit =
        builder.append('\n')

      override def lazyStringWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
        builder.append(colorState.revert)
        builder.append('\n')
        builder.append(currentIndent)
        builder.append(colorState.apply)
      }

    }

    final case class Str(value: String) extends LazyString {

      override def lazyStringWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit =
        builder.append(value)

      override def lazyStringWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit =
        if value.contains('\n') then
          value.foreach {
            case '\n' => Newline.lazyStringWriteComplex(cfg, builder, currentIndent, colorState)
            case c    => builder.append(c)
          }
        else
          builder.append(value)

    }

    final case class When(underlying: LazyString, cond: Boolean) extends LazyString {

      override def lazyStringWriteSimple(cfg: Config, builder: StringBuilder): Unit =
        if cond then underlying.lazyStringWriteSimple(cfg, builder)

      override def lazyStringWriteComplex(cfg: Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit =
        if cond then underlying.lazyStringWriteComplex(cfg, builder, currentIndent, colorState)
    }

    final case class Interpolated(strings: IArray[String], args: IArray[LazyString]) extends LazyString {

      override def lazyStringWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
        val size: Int = args.length
        var _idx: Int = 0
        while _idx < size do {
          builder.append(strings(_idx))
          args(_idx).lazyStringWriteSimple(cfg, builder)
          _idx = _idx + 1
        }
        builder.append(strings(_idx))
      }

      override def lazyStringWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
        val size: Int = args.length
        var _idx: Int = 0
        while _idx < size do {
          Str(strings(_idx)).lazyStringWriteComplex(cfg, builder, currentIndent, colorState)
          args(_idx).lazyStringWriteComplex(cfg, builder, currentIndent, colorState)
          _idx = _idx + 1
        }
        Str(strings(_idx)).lazyStringWriteComplex(cfg, builder, currentIndent, colorState)
      }

    }

    final case class CustomIndented(underlying: LazyString, indent: LazyString) extends LazyString {

      override def lazyStringWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
        val newIdt = indent.lazyStringBuildRootSimple(cfg)
        builder.withCommit {
          Newline.lazyStringWriteComplex(cfg, builder, newIdt, ColorStateV2.Empty)
          val snap = builder.snapshot()
          underlying.lazyStringWriteComplex(cfg, builder, newIdt, ColorStateV2.Empty)
          snap.underlyingChanged()
        }
      }

      override def lazyStringWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
        val newIdt = currentIndent + indent.lazyStringBuildRootComplex(cfg, currentIndent, colorState)
        builder.withCommit {
          Newline.lazyStringWriteComplex(cfg, builder, newIdt, colorState)
          val snap = builder.snapshot()
          underlying.lazyStringWriteComplex(cfg, builder, newIdt, colorState)
          snap.underlyingChanged()
        }
      }

    }

    final case class CustomIndentedInitial(underlying: LazyString, indent: LazyString) extends LazyString {

      override def lazyStringWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
        val initialIdt = indent.lazyStringBuildRootSimple(cfg)
        val restIdt = " " * initialIdt.length // TODO (KR) : might be worth evaluating `indent` without color
        builder.withCommit {
          Newline.lazyStringWriteComplex(cfg, builder, initialIdt, ColorStateV2.Empty)
          val snap = builder.snapshot()
          underlying.lazyStringWriteComplex(cfg, builder, restIdt, ColorStateV2.Empty)
          snap.underlyingChanged()
        }
      }

      override def lazyStringWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
        val tmp = indent.lazyStringBuildRootComplex(cfg, currentIndent, colorState)
        val initialIdt = currentIndent + tmp
        val restIdt = currentIndent + (" " * tmp.length)
        builder.withCommit {
          Newline.lazyStringWriteComplex(cfg, builder, initialIdt, colorState)
          val snap = builder.snapshot()
          underlying.lazyStringWriteComplex(cfg, builder, restIdt, colorState)
          snap.underlyingChanged()
        }
      }

    }

    final case class DefaultIndented(underlying: LazyString) extends LazyString {

      override def lazyStringWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
        val (indent, newCfg) = cfg.popIndent
        val newIdt = indent.lazyStringBuildRootSimple(cfg)
        builder.withCommit {
          Newline.lazyStringWriteComplex(newCfg, builder, newIdt, ColorStateV2.Empty)
          val snap = builder.snapshot()
          underlying.lazyStringWriteComplex(newCfg, builder, newIdt, ColorStateV2.Empty)
          snap.underlyingChanged()
        }
      }

      override def lazyStringWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
        val (indent, newCfg) = cfg.popIndent
        val newIdt = currentIndent + indent.lazyStringBuildRootComplex(cfg, currentIndent, colorState)
        builder.withCommit {
          Newline.lazyStringWriteComplex(newCfg, builder, newIdt, colorState)
          val snap = builder.snapshot()
          underlying.lazyStringWriteComplex(newCfg, builder, newIdt, colorState)
          snap.underlyingChanged()
        }
      }

    }

    sealed trait Colorize extends LazyString {

      protected val underlying: LazyString
      protected def patch(colorMode: ColorMode, current: ColorStateV2): Option[ColorStateV2.Patch]

      override final def lazyStringWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit =
        patch(cfg.colorMode, ColorStateV2.Empty) match {
          case Some(patch) =>
            builder.append(patch.apply)
            underlying.lazyStringWriteComplex(cfg, builder, "", patch.newState)
            builder.append(patch.revert)
          case None =>
            underlying.lazyStringWriteSimple(cfg, builder)
        }

      override final def lazyStringWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit =
        patch(cfg.colorMode, colorState) match {
          case Some(patch) =>
            builder.append(patch.apply)
            underlying.lazyStringWriteComplex(cfg, builder, currentIndent, patch.newState)
            builder.append(patch.revert)
          case None =>
            underlying.lazyStringWriteComplex(cfg, builder, currentIndent, colorState)
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

      override def lazyStringWriteSimple(cfg: Config, builder: StringBuilder): Unit = {
        val iter = seqOps.newIterator(underlying)
        while iter.hasNext do f(iter.next()).lazyStringWriteSimple(cfg, builder)
      }

      override def lazyStringWriteComplex(cfg: Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
        val iter = seqOps.newIterator(underlying)
        while iter.hasNext do f(iter.next()).lazyStringWriteComplex(cfg, builder, currentIndent, colorState)
      }

    }

    object Concat {

      final case class _2(first: LazyString, second: LazyString) extends LazyString {

        override def lazyStringWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
          first.lazyStringWriteSimple(cfg, builder)
          second.lazyStringWriteSimple(cfg, builder)
        }

        override def lazyStringWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
          first.lazyStringWriteComplex(cfg, builder, currentIndent, colorState)
          second.lazyStringWriteComplex(cfg, builder, currentIndent, colorState)
        }

      }

      final case class ManyLazyStrings[S[_]](seqOps: SeqRead[S], underlying: S[LazyString]) extends LazyString {

        override def lazyStringWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
          val iter = seqOps.newIterator(underlying)
          while iter.hasNext do iter.next().lazyStringWriteSimple(cfg, builder)
        }

        override def lazyStringWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
          val iter = seqOps.newIterator(underlying)
          while iter.hasNext do iter.next().lazyStringWriteComplex(cfg, builder, currentIndent, colorState)
        }

      }

      final case class SurroundLazyStrings[S[_]](seqOps: SeqRead[S], underlying: S[LazyString], prefix: LazyString, join: LazyString, suffix: LazyString) extends LazyString {

        override def lazyStringWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
          val iter = seqOps.newIterator(underlying)
          val renderedJoin = join.lazyStringBuildRootSimple(cfg)

          prefix.lazyStringWriteSimple(cfg, builder)

          if iter.hasNext then
            iter.next().lazyStringWriteSimple(cfg, builder)

          while iter.hasNext do {
            builder.append(renderedJoin)
            iter.next().lazyStringWriteSimple(cfg, builder)
          }

          suffix.lazyStringWriteSimple(cfg, builder)
        }

        override def lazyStringWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit = {
          val renderedJoin = join.lazyStringBuildRootComplex(cfg, currentIndent, colorState)

          prefix.lazyStringWriteComplex(cfg, builder, currentIndent, colorState)

          val iter = seqOps.newIterator(underlying)
          if iter.hasNext then
            iter.next().lazyStringWriteComplex(cfg, builder, currentIndent, colorState)

          while iter.hasNext do {
            builder.append(renderedJoin)
            iter.next().lazyStringWriteComplex(cfg, builder, currentIndent, colorState)
          }

          suffix.lazyStringWriteComplex(cfg, builder, currentIndent, colorState)
        }

      }

    }

  }

}

extension (inline sc: StringContext)
  inline def str(inline args: LazyString.Auto*): LazyString = ${ LazyString.interpolateStrInternal('sc, 'args) }
