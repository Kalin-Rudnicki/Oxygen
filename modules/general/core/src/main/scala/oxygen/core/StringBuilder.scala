package oxygen.core

import oxygen.core.collection.mutable.ArrayBuilder

final class StringBuilder(threadSafe: Boolean) {

  private val chars: ArrayBuilder[Char] =
    if threadSafe then ArrayBuilder.emptyThreadSafe[Char]
    else ArrayBuilder.emptyThreadUnsafe[Char]

  def append(string: String): Unit = chars.addStringChars(string)
  def <<(string: String): StringBuilder = {
    chars.addStringChars(string)
    this
  }

  def append(that: StringBuilder): Unit =
    if that ne null then this.chars.addAllBuilder(that.chars)
    else chars.addAllArrayElements(StringBuilder.nullChars)

  def append(value: Boolean): Unit =
    if value then chars.addAllArrayElements(StringBuilder.trueChars)
    else chars.addAllArrayElements(StringBuilder.falseChars)
  def append(value: Char): Unit = chars.addSingle(value)

  def append(value: Byte): Unit = chars.addStringChars(value.toString)
  def append(value: Short): Unit = chars.addStringChars(value.toString)
  def append(value: Int): Unit = chars.addStringChars(value.toString)
  def append(value: Long): Unit = chars.addStringChars(value.toString)

  def appendAnyRef(any: AnyRef): Unit =
    if any ne null then chars.addStringChars(any.toString)
    else chars.addAllArrayElements(StringBuilder.nullChars)

  def appendAny(any: Any): Unit =
    if any != null then chars.addStringChars(any.toString)
    else chars.addAllArrayElements(StringBuilder.nullChars)

  def build(): String = new String(chars.buildArray())

  override def toString: String = build()

}
object StringBuilder {

  private val nullChars: Array[Char] = Array('n', 'u', 'l', 'l')
  private val trueChars: Array[Char] = Array('t', 'r', 'u', 'e')
  private val falseChars: Array[Char] = Array('f', 'a', 'l', 's', 'e')

}
