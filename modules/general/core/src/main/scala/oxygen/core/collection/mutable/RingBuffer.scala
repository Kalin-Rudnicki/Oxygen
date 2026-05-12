package oxygen.core.collection.mutable

import oxygen.core.syntax.option.*
import scala.reflect.ClassTag

final class RingBuffer[A: ClassTag](initialSize: Int) {

  ///////  ///////////////////////////////////////////////////////////////

  private var _used: Int = 0
  private var _headIdx: Int = 0
  private var _tailIdx: Int = 0
  private var _buffer: Array[A] = new Array[A](initialSize)

  ///////  ///////////////////////////////////////////////////////////////

  def used: Int = _used
  def allocated: Int = _buffer.length

  ///////  ///////////////////////////////////////////////////////////////

  private inline def growIfNeeded(addSize: Int): Unit = {
    val newUsed: Long = _used + addSize
    if newUsed > _buffer.length then {
      var longNewAllocated: Long = _buffer.length
      while longNewAllocated < newUsed do
        longNewAllocated = longNewAllocated * 2

      if longNewAllocated > Int.MaxValue then
        throw new RuntimeException(s"RingBuffer size overflow ($longNewAllocated > ${Int.MaxValue})")

      val newAllocated: Int = longNewAllocated.toInt
      val newBuffer: Array[A] = new Array[A](newAllocated)
      if _used > 0 then {
        if _tailIdx >= _headIdx then {
          System.arraycopy(_buffer, _headIdx, newBuffer, 0, _used)
        } else {
          val preWrapSize: Int = _buffer.length - _headIdx
          System.arraycopy(_buffer, _headIdx, newBuffer, 0, preWrapSize)
          System.arraycopy(_buffer, 0, newBuffer, preWrapSize, _used - preWrapSize)
        }
      }
      _headIdx = 0
      _tailIdx = _used - 1
      _buffer = newBuffer
    }
  }

  private inline def prependInternal(value: A): Unit = {
    if _used > 0 then
      _headIdx = _headIdx - 1

    if _headIdx < 0 then
      _headIdx = _buffer.length - 1

    _used = _used + 1
    _buffer(_headIdx) = value
  }

  private inline def appendInternal(value: A): Unit = {
    if _used > 0 then
      _tailIdx = _tailIdx + 1

    if _tailIdx >= _buffer.length then
      _tailIdx = 0

    _used = _used + 1
    _buffer(_tailIdx) = value
  }

  ///////  ///////////////////////////////////////////////////////////////

  /**
    * Add to the start of the buffer.
    */
  def prepend(value: A): Unit = {
    growIfNeeded(1)
    prependInternal(value)
  }

  /**
    * Add to the end of the buffer.
    */
  def append(value: A): Unit = {
    growIfNeeded(1)
    appendInternal(value)
  }

  /**
    * Add all to the end of the buffer.
    */
  def appendAll(values: Iterable[A]): Unit = {
    growIfNeeded(values.size)
    values.foreach(appendInternal)
  }

  /**
    * Take from the start of the buffer.
    */
  def popHead(): Option[A] =
    _used match {
      case 0 =>
        None
      case 1 =>
        val value: A = _buffer(_headIdx)
        _buffer(_headIdx) = null.asInstanceOf[A]

        _headIdx = 0
        _tailIdx = 0
        _used = 0

        value.some
      case _ =>
        val value: A = _buffer(_headIdx)
        _buffer(_headIdx) = null.asInstanceOf[A]

        _headIdx = _headIdx + 1
        _used = _used - 1
        if _headIdx >= _buffer.length then
          _headIdx = 0

        value.some
    }

  /**
    * Take from the end of the buffer.
    */
  def popTail(): Option[A] =
    _used match {
      case 0 =>
        None
      case 1 =>
        val value: A = _buffer(_tailIdx)
        _buffer(_tailIdx) = null.asInstanceOf[A]

        _headIdx = 0
        _tailIdx = 0
        _used = 0

        value.some
      case _ =>
        val value: A = _buffer(_tailIdx)
        _buffer(_tailIdx) = null.asInstanceOf[A]

        _tailIdx = _tailIdx - 1
        _used = _used - 1
        if _tailIdx < 0 then
          _tailIdx = _buffer.length - 1

        value.some
    }

  override def toString: String = {
    val sb = oxygen.core.StringBuilder.emptyThreadUnsafe

    sb.append("RingBuffer[used: ")
    sb.append(_used)
    sb.append(", allocated: ")
    sb.append(_buffer.length)
    sb.append(", head: ")
    sb.append(_headIdx)
    sb.append(", tail: ")
    sb.append(_tailIdx)
    sb.append("](")

    var idx: Int = 0
    while idx < _buffer.length do {
      if idx > 0 then
        sb.append(", ")
      sb.appendAny(_buffer(idx))
      idx = idx + 1
    }
    sb.append(")")

    sb.toString
  }

}
object RingBuffer {

  def empty[A: ClassTag](initialSize: Int): RingBuffer[A] = new RingBuffer[A](initialSize)
  def empty[A: ClassTag]: RingBuffer[A] = new RingBuffer[A](32)

}
