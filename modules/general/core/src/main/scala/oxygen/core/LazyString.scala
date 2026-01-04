package oxygen.core

import oxygen.core.collection.NonEmptyList
import oxygen.core.typeclass.{SeqOps, Showable}

sealed trait LazyString {

  private[LazyString] def writeSimple(cfg: LazyString.Config, builder: StringBuilder): Unit
  private[LazyString] def writeComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: LazyString.ColorState): Unit

  final def ++(that: LazyString): LazyString = LazyString.impl.Concat2(this, that)
  final def |>(that: LazyString): LazyString = this ++ (LazyString.impl.Newline ++ that).indented

  final def indented(idt: LazyString): LazyString = LazyString.impl.CustomIndented(this, idt)
  final def indented: LazyString = LazyString.impl.DefaultIndented(this)

  final def buildNowSimple(cfg: LazyString.Config): String = StringBuilder.makeString(this.writeSimple(cfg, _))
  final def buildNowComplex(cfg: LazyString.Config, currentIndent: String, colorState: LazyString.ColorState): String = StringBuilder.makeString(this.writeComplex(cfg, _, currentIndent, colorState))

}
object LazyString {

  val empty: LazyString = LazyString.impl.Empty

  def fromString(value: String): LazyString =
    ??? // FIX-PRE-MERGE (KR) :

  def fromAny(value: Any): LazyString = value.asInstanceOf[Matchable] match
    case null                           => LazyString.impl.Str("null")
    case value: LazyString              => value
    case value: Showable                => value.show
    case value: String if value.isEmpty => LazyString.impl.Empty
    case value: String                  => LazyString.impl.Str(value)
    case _                              => LazyString.impl.Str(value.toString)

  final case class Config(
      colorMode: ColorMode,
      rootDefaultIndents: NonEmptyList[LazyString],
      indentQueue: NonEmptyList[LazyString],
  ) {

    def popIndent: (LazyString, Config) = indentQueue.tail match
      case h :: t => (indentQueue.head, copy(indentQueue = NonEmptyList(h, t)))
      case Nil    => (indentQueue.head, copy(indentQueue = rootDefaultIndents))

  }

  final case class ColorState(
      fg: Specified[Color.Concrete],
      bg: Specified[Color.Concrete],
      colorize: String,
      decolorize: String,
  ) {

    val isColorized: Boolean = fg.isSpecified || bg.isSpecified

  }
  object ColorState {

    val empty: ColorState = ColorState(___, ___, "", "")

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Impl
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private object impl {

    case object Empty extends LazyString {
      override private[LazyString] def writeSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = ()
      override private[LazyString] def writeComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: LazyString.ColorState): Unit = ()
    }

    case object Newline extends LazyString {

      override private[LazyString] def writeSimple(cfg: LazyString.Config, builder: StringBuilder): Unit =
        builder.append('\n')

      override private[LazyString] def writeComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: LazyString.ColorState): Unit = {
        if colorState.isColorized then
          builder.append(colorState.decolorize)
        builder.append('\n')
        builder.append(currentIndent)
      }

    }

    final case class Str(value: String) extends LazyString {

      override private[LazyString] def writeSimple(cfg: LazyString.Config, builder: StringBuilder): Unit =
        builder.append(value)

      override private[LazyString] def writeComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: LazyString.ColorState): Unit =
        if (currentIndent ne Empty) && value.contains('\n') then
          value.foreach {
            case '\n' => Newline.writeComplex(cfg, builder, currentIndent, colorState)
            case c    => builder.append(c)
          }
        else
          builder.append(value)

    }

    final case class Interpolated(strings: IArray[String], args: IArray[LazyString]) extends LazyString {

      override private[LazyString] def writeSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
        val size: Int = args.length
        var _idx: Int = 0
        while _idx < size do {
          builder.append(strings(_idx))
          args(_idx).writeSimple(cfg, builder)
          _idx = _idx + 1
        }
        builder.append(strings(_idx))
      }

      override private[LazyString] def writeComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: LazyString.ColorState): Unit = {
        val size: Int = args.length
        var _idx: Int = 0
        while _idx < size do {
          Str(strings(_idx)).writeComplex(cfg, builder, currentIndent, colorState)
          args(_idx).writeComplex(cfg, builder, currentIndent, colorState)
          _idx = _idx + 1
        }
        Str(strings(_idx)).writeComplex(cfg, builder, currentIndent, colorState)
      }

    }

    final case class CustomIndented(underlying: LazyString, indent: LazyString) extends LazyString {

      override private[LazyString] def writeSimple(cfg: LazyString.Config, builder: StringBuilder): Unit =
        underlying.writeComplex(cfg, builder, indent.buildNowSimple(cfg), ColorState.empty)

      override private[LazyString] def writeComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: LazyString.ColorState): Unit =
        underlying.writeComplex(cfg, builder, currentIndent + indent.buildNowComplex(cfg, currentIndent, colorState), ColorState.empty)

    }

    final case class DefaultIndented(underlying: LazyString) extends LazyString {

      override private[LazyString] def writeSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
        val (indent, newCfg) = cfg.popIndent
        underlying.writeComplex(newCfg, builder, indent.buildNowSimple(cfg), ColorState.empty)
      }

      override private[LazyString] def writeComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: LazyString.ColorState): Unit = {
        val (indent, newCfg) = cfg.popIndent
        underlying.writeComplex(newCfg, builder, currentIndent + indent.buildNowComplex(cfg, currentIndent, colorState), ColorState.empty)
      }

    }

    final case class Colorized(underlying: LazyString, fg: Specified[Color], bg: Specified[Color]) extends LazyString {

      override private[LazyString] def writeSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
        /*
        if cfg.colorMode eq ColorMode.Colorless then underlying.writeSimple(cfg, builder)
        else {
          val concreteFg = fg.flatMapOption(cfg.colorMode.toConcrete)
          val concreteBg = bg.flatMapOption(cfg.colorMode.toConcrete)
          ??? // FIX-PRE-MERGE (KR) :
        }
         */
        ???
      }

      override private[LazyString] def writeComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: LazyString.ColorState): Unit =
        ??? // FIX-PRE-MERGE (KR) :

    }

    object Concat {

      final case class _2(first: LazyString, second: LazyString) extends LazyString {

        override private[LazyString] def writeSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
          first.writeSimple(cfg, builder)
          second.writeSimple(cfg, builder)
        }

        override private[LazyString] def writeComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: LazyString.ColorState): Unit = {
          first.writeComplex(cfg, builder, currentIndent, colorState)
          second.writeComplex(cfg, builder, currentIndent, colorState)
        }

      }

      final case class ManyLazyStrings[S[_]](seqOps: SeqOps[S], underlying: S[LazyString]) extends LazyString {

        override private[LazyString] def writeSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
          val iter = seqOps.newIterator(underlying)
          while iter.hasNext do iter.next().writeSimple(cfg, builder)
        }

        override private[LazyString] def writeComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: LazyString.ColorState): Unit = {
          val iter = seqOps.newIterator(underlying)
          while iter.hasNext do iter.next().writeComplex(cfg, builder, currentIndent, colorState)
        }

      }

      final case class SurroundLazyStrings[S[_]](seqOps: SeqOps[S], underlying: S[LazyString], prefix: LazyString, join: LazyString, suffix: LazyString) extends LazyString {

        override private[LazyString] def writeSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
          val iter = seqOps.newIterator(underlying)
          val renderedJoin = join.buildNowSimple(cfg)

          prefix.writeSimple(cfg, builder)

          if iter.hasNext then
            iter.next().writeSimple(cfg, builder)

          while iter.hasNext do {
            builder.append(renderedJoin)
            iter.next().writeSimple(cfg, builder)
          }

          suffix.writeSimple(cfg, builder)
        }

        override private[LazyString] def writeComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: LazyString.ColorState): Unit = {
          val renderedJoin = join.buildNowComplex(cfg, currentIndent, colorState)

          prefix.writeComplex(cfg, builder, currentIndent, colorState)

          val iter = seqOps.newIterator(underlying)
          if iter.hasNext then
            iter.next().writeComplex(cfg, builder, currentIndent, colorState)

          while iter.hasNext do {
            builder.append(renderedJoin)
            iter.next().writeComplex(cfg, builder, currentIndent, colorState)
          }

          suffix.writeComplex(cfg, builder, currentIndent, colorState)

        }

      }

    }

    object SeqJoin {

      final case class Simple[S[_]](seqOps: SeqOps[S], underlying: S[Any]) extends SeqJoin {

        override private[LazyString] def writeSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
          val iter = seqOps.newIterator(underlying)
          while iter.hasNext do LazyString.fromAny(iter.next()).writeSimple(cfg, builder)
        }

        override private[LazyString] def writeComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: LazyString.ColorState): Unit = {
          val iter = seqOps.newIterator(underlying)
          while iter.hasNext do LazyString.fromAny(iter.next()).writeComplex(cfg, builder, currentIndent, colorState)
        }

      }

      final case class Join[S[_]](seqOps: SeqOps[S], underlying: S[Any], join: LazyString) extends SeqJoin {

        override private[LazyString] def writeSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
          val iter = seqOps.newIterator(underlying)
          val renderedJoin = join.buildNowSimple(cfg)

          if iter.hasNext then
            LazyString.fromAny(iter.next()).writeSimple(cfg, builder)

          while iter.hasNext do {
            builder.append(renderedJoin)
            LazyString.fromAny(iter.next()).writeSimple(cfg, builder)
          }
        }

        override private[LazyString] def writeComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: LazyString.ColorState): Unit = {
          val iter = seqOps.newIterator(underlying)
          val renderedJoin = join.buildNowComplex(cfg, currentIndent, colorState)

          if iter.hasNext then
            LazyString.fromAny(iter.next()).writeComplex(cfg, builder, currentIndent, colorState)

          while iter.hasNext do {
            builder.append(renderedJoin)
            LazyString.fromAny(iter.next()).writeComplex(cfg, builder, currentIndent, colorState)
          }
        }

      }

      final case class Surround[S[_]](seqOps: SeqOps[S], underlying: S[Any], prefix: LazyString, join: LazyString, suffix: LazyString) extends SeqJoin {

        override private[LazyString] def writeSimple(cfg: LazyString.Config, builder: StringBuilder): Unit = {
          val iter = seqOps.newIterator(underlying)
          val renderedJoin = join.buildNowSimple(cfg)

          prefix.writeSimple(cfg, builder)

          if iter.hasNext then
            LazyString.fromAny(iter.next()).writeSimple(cfg, builder)

          while iter.hasNext do {
            builder.append(renderedJoin)
            LazyString.fromAny(iter.next()).writeSimple(cfg, builder)
          }

          suffix.writeSimple(cfg, builder)
        }

        override private[LazyString] def writeComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: LazyString.ColorState): Unit = {
          val renderedJoin = join.buildNowComplex(cfg, currentIndent, colorState)

          prefix.writeComplex(cfg, builder, currentIndent, colorState)

          val iter = seqOps.newIterator(underlying)
          if iter.hasNext then
            LazyString.fromAny(iter.next()).writeComplex(cfg, builder, currentIndent, colorState)

          while iter.hasNext do {
            builder.append(renderedJoin)
            LazyString.fromAny(iter.next()).writeComplex(cfg, builder, currentIndent, colorState)
          }

          suffix.writeComplex(cfg, builder, currentIndent, colorState)

        }

      }

    }

  }

}
