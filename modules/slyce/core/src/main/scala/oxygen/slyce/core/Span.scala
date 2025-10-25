package oxygen.slyce.core

sealed trait Span {

  val source: Source

}
object Span {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def range(startInclusive: SourcePosition.NonEOF, endInclusive: SourcePosition.NonEOF): Span.Range = Span.Range(startInclusive, endInclusive)

  def exact(startEndInclusive: SourcePosition.NonEOF): Span.Range = Span.range(startEndInclusive, startEndInclusive)

  def zeroWidthBefore(startExclusive: SourcePosition): Span.ZeroWidth = Span.ZeroWidth(startExclusive)

  def eof(source: Source): Span.ZeroWidth = Span.zeroWidthBefore(source.position(source.length))

  def fromLine(line: SourceLine): Span.HasPosition = line match
    case line: SourceLine.Empty    => Span.zeroWidthBefore(line.eol)
    case line: SourceLine.NonEmpty => Span.range(line.firstChar, line.lastChar)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Type
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait HasPosition extends Span

  final case class Range(startInclusive: SourcePosition.NonEOF, endInclusive: SourcePosition.NonEOF) extends Span.HasPosition {

    if (!startInclusive.source.exactSame(endInclusive.source))
      throw new RuntimeException(s"Tried to create Span.Range with positions from different sources:\n  startInclusive: $startInclusive\n  endInclusive: $endInclusive")

    override val source: Source = startInclusive.source

  }

  final case class ZeroWidth(startExclusive: SourcePosition) extends Span.HasPosition {
    override val source: Source = startExclusive.source
  }

  final case class NoPosition(source: Source) extends Span

  // FIX-PRE-MERGE (KR) :

  // Range
  // ZeroWidth
  // EOF
  // Unknown (needed? covered by ZeroWidth?)

}
