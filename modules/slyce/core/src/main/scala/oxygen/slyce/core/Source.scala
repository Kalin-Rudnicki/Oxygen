package oxygen.slyce.core

final case class Source(
    rawText: String,
    sourceType: Source.Type,
) {

  val chars: IArray[Char] = IArray.unsafeFromArray(rawText.toCharArray)
  val length: Int = chars.length
  
  val finalCharIdx: Int = if (isEmpty) 0 else length - 1
  val eofIdx: Int = length

  def isEmpty: Boolean = rawText.isEmpty
  def nonEmpty: Boolean = rawText.nonEmpty

  def substringInclusive(startInclusive: SourcePosition, endInclusive: SourcePosition): String =
    ??? // FIX-PRE-MERGE (KR) :

  def substringExclusive(startInclusive: SourcePosition, endExclusive: SourcePosition): String =
    ??? // FIX-PRE-MERGE (KR) :

  inline def apply(idx: Int): Char = chars(idx)

  def equiv(that: Source): Boolean =
    this.sourceType == that.sourceType && this.rawText == that.rawText

  def exactSame(that: Source): Boolean =
    this eq that

  override def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: Source => this.exactSame(that) || this.equiv(that)
    case _            => false

}
object Source {

  def file(fileName: String, fileContents: String): Source =
    Source(fileContents, Source.Type.File(fileName))

  def rawText(rawText: String): Source =
    Source(rawText, Source.Type.Raw)

  // TODO (KR) : maybe use java Path? full? relative?
  enum Type {
    case File(fileName: String)
    case Raw
  }

}
