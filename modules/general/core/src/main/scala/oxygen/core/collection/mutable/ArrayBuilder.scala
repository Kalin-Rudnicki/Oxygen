package oxygen.core.collection.mutable

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

final class ArrayBuilder[A] private (private val threadUnsafe: Boolean)(using private val ct: ClassTag[A]) {

  def addStringChars(string: String)(using ev: A =:= Char): Unit = {
    val string0: String = if string ne null then string else "null"
    val strLen: Int = string0.length
    if strLen == 0 then ()
    else if threadUnsafe then addStringCharsInternal(string0, strLen)
    else this.synchronized { addStringCharsInternal(string0, strLen) }
  }

  def addAllArrayElements(elementsToAdd: Array[A]): Unit =
    if elementsToAdd eq null then ()
    else {
      val newElemsLength: Int = elementsToAdd.length
      if newElemsLength == 0 then ()
      else if threadUnsafe then addAllArrayElementsInternal(elementsToAdd, newElemsLength) //
      else this.synchronized { addAllArrayElementsInternal(elementsToAdd, newElemsLength) }
    }

  def addAllIterable(elementsToAdd: Iterable[A]): Unit =
    if elementsToAdd eq null then ()
    else {
      val knownSize: Int = elementsToAdd.knownSize
      if knownSize == 0 then ()
      else if knownSize == -1 then addAllIterator(elementsToAdd.iterator)
      else
        if threadUnsafe then addAllIterableInternal(elementsToAdd, knownSize)
        else this.synchronized { addAllIterableInternal(elementsToAdd, knownSize) }
    }

  def addAllIterator(elementsToAdd: Iterator[A]): Unit =
    if elementsToAdd eq null then ()
    else if threadUnsafe then addAllIteratorInternal(elementsToAdd)
    else this.synchronized { addAllIteratorInternal(elementsToAdd) }

  def addSingle(element: A): Unit =
    if threadUnsafe then addSingleInternal(element)
    else this.synchronized { addSingleInternal(element) }

  def addAllBuilder(that: ArrayBuilder[A]): Unit =
    if that eq null then ()
    else {
      val thatSnapshot: ArrayBuilder.Snapshot[A] = that.safeSnapshot()
      if threadUnsafe then addAllBuilderInternal(thatSnapshot)
      else this.synchronized { addAllBuilderInternal(thatSnapshot) }
    }

  def <<(array: Array[A]): ArrayBuilder[A] = {
    this.addAllArrayElements(array)
    this
  }

  def iterator(): Iterator[A] = snapshot().iterator

  def snapshot(): Seq[A] & ArrayBuilder.Report =
    if threadUnsafe then snapshotInternal()
    else this.synchronized { snapshotInternal() }

  private def safeSnapshot(): ArrayBuilder.Snapshot[A] =
    if threadUnsafe then snapshotInternal()
    else this.synchronized { snapshotInternal() }

  def buildArray(): Array[A] =
    safeSnapshot().arrayOfA

  def isEmpty(): Boolean =
    if threadUnsafe then _currentUsed == 0 && _overflowUsed == 0
    else this.synchronized { _currentUsed == 0 && _overflowUsed == 0 }
  def nonEmpty(): Boolean =
    if threadUnsafe then _currentUsed > 0 || _overflowUsed > 0
    else this.synchronized { _currentUsed > 0 || _overflowUsed > 0 }
  def size(): Int = {
    val total: Long =
      if threadUnsafe then _overflowTotalUsedElems + _currentUsed
      else this.synchronized { _overflowTotalUsedElems + _currentUsed }
    if total > jdk.internal.util.ArraysSupport.SOFT_MAX_ARRAY_LENGTH then
      throw new RuntimeException(s"Unable to build array of length $total, maxArraySize=${jdk.internal.util.ArraysSupport.SOFT_MAX_ARRAY_LENGTH}")
    total.toInt
  }

  def withCommit(f: => Boolean): Unit =
    if threadUnsafe then withCommitInternal(f)
    else this.synchronized { withCommitInternal(f) }

  private def withCommitInternal(f: => Boolean): Unit = {
    val initial: ArrayBuilder.Snapshot[A] = this.snapshotInternal()
    val doCommit: Boolean = f
    if !doCommit then {
      this._overflowUsed = initial.overflowUsed
      this._overflowAllocatedElems = initial.overflowAllocatedElems
      this._overflowTotalUsedElems = initial.overflowTotalUsedElems
      this._currentUsed = initial.currentUsed
      this._currentAvailable = initial.currentAvailable
    }
  }

  def showInternalState(): String = {
    val sb = new scala.collection.mutable.StringBuilder()

    sb.append("ArrayBuilder[")
    sb.append(ct.runtimeClass.getName)
    sb.append(s"]:\n  overflow ( size = $_overflowUsed, used = $_overflowTotalUsedElems, total = $_overflowAllocatedElems ):")

    var _tmpIdx: Int = 0
    while _tmpIdx < _overflowUsed do {
      val node = _overflowBuffer(_tmpIdx)
      sb.append("\n    - ( allocated = ")
      sb.append(node.array.length)
      sb.append(", used = ")
      sb.append(node.used)
      sb.append(" ): \n      ")
      node.array.addString(sb, "[", ", ", "]")
      _tmpIdx = _tmpIdx + 1
    }

    sb.append(s"\n  current:\n    ( allocated = ${if _currentArray eq null then "<N/A>" else _currentArray.length}, used = ")
    sb.append(_currentUsed)
    sb.append(", available = ")
    sb.append(_currentAvailable)
    sb.append("):\n    ")
    if _currentArray eq null then sb.append("<empty-buffer>")
    else _currentArray.addString(sb, "[", ", ", "]")

    sb.result()
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /*
      overflow buffer elements:
        - ["A", "B", "C", null] # 3 / 4
        - ["D", "E", null x 6] # 2 / 8
        - ["F", "G", "H", "I", "J", "K", "L", "M", "N", "O"] # 10 / 10
        - null

      _overflowTotalUsedElems = 3 + 2 + 10 = 15
      _overflowAllocatedElems = 4 + 8 + 10 = 22
      _numOverflowElems = 3
   */
  private var _overflowTotalUsedElems: Long = 0
  private var _overflowAllocatedElems: Long = 0
  private var _overflowUsed: Int = 0
  private var _overflowBuffer: Array[ArrayBuilder.PotentiallyPartialArray[A]] = new Array[ArrayBuilder.PotentiallyPartialArray[A]](32)

  private def overflowAppend(array: Array[A], used: Int): Unit = {
    if _overflowUsed >= _overflowBuffer.length then {
      val newOverflowBuffer: Array[ArrayBuilder.PotentiallyPartialArray[A]] = new Array[ArrayBuilder.PotentiallyPartialArray[A]](array.length + 32)
      System.arraycopy(_overflowBuffer, 0, newOverflowBuffer, 0, _overflowBuffer.length)
      _overflowBuffer = newOverflowBuffer
    }
    _overflowBuffer(_overflowUsed) = ArrayBuilder.PotentiallyPartialArray[A](array, used)
    _overflowTotalUsedElems = _overflowTotalUsedElems + used
    _overflowAllocatedElems = _overflowAllocatedElems + array.length
    _overflowUsed = _overflowUsed + 1
  }

  private var _currentArray: Array[A] = null
  private var _currentUsed: Int = 0
  private var _currentAvailable: Int = 32

  private inline def newSize(inline base: Int): Int = {
    val newSizeLong: Long = base.toLong * 2
    if newSizeLong <= jdk.internal.util.ArraysSupport.SOFT_MAX_ARRAY_LENGTH then newSizeLong.toInt
    else jdk.internal.util.ArraysSupport.SOFT_MAX_ARRAY_LENGTH
  }

  private inline def addStringCharsNoOverflow(inline string: String, inline newElemsLength: Int, inline newAvailable: Int): Unit = {
    val castCurrentArray: Array[Char] = _currentArray.asInstanceOf[Array[Char]]
    string.getChars(0, newElemsLength, castCurrentArray, _currentUsed)
    _currentUsed = _currentUsed + newElemsLength
    _currentAvailable = newAvailable
  }

  private inline def addAllArrayElementsNoOverflow(inline elementsToAdd: Array[A], inline newElemsLength: Int, inline newAvailable: Int): Unit = {
    System.arraycopy(elementsToAdd, 0, _currentArray, _currentUsed, newElemsLength)
    _currentUsed = _currentUsed + newElemsLength
    _currentAvailable = newAvailable
  }

  private inline def addAllIterableElementsNoOverflow(inline elementsToAdd: Iterable[A], inline newElemsLength: Int, inline newAvailable: Int): Unit = {
    elementsToAdd.copyToArray(_currentArray, _currentUsed, newElemsLength)
    _currentUsed = _currentUsed + newElemsLength
    _currentAvailable = newAvailable
  }

  private inline def addStringCharsInternal(inline string: String, inline newElemsLength: Int): Unit = {
    val newAvailable: Int = _currentAvailable - newElemsLength
    if _currentArray eq null then
      if newElemsLength >= (_currentAvailable >> 1) then {
        // at this point, you have not even allocated [[currentArray]]
        // the first thing you tried to add already took up more than half your buffer
        // just add it to overflow and increase size
        overflowAppend(string.toCharArray.asInstanceOf[Array[A]], newElemsLength)
        _currentAvailable = newSize(_currentAvailable.max(newElemsLength))
      } else {
        _currentArray = new Array[A](_currentAvailable)
        addStringCharsNoOverflow(string, newElemsLength, newAvailable)
      }
    else
      if newAvailable >= 0 then {
        addStringCharsNoOverflow(string, newElemsLength, newAvailable)
      } else {
        overflowAppend(_currentArray, _currentUsed)
        overflowAppend(string.toCharArray.asInstanceOf[Array[A]], newElemsLength)
        _currentAvailable = newSize(_currentArray.length.max(newElemsLength))
        _currentArray = null
        _currentUsed = 0
      }
  }

  private inline def addAllArrayElementsInternal(inline elementsToAdd: Array[A], inline newElemsLength: Int): Unit = {
    val newAvailable: Int = _currentAvailable - newElemsLength
    if _currentArray eq null then
      if newElemsLength >= (_currentAvailable >> 1) then {
        // at this point, you have not even allocated [[currentArray]]
        // the first thing you tried to add already took up more than half your buffer
        // just add it to overflow and increase size
        overflowAppend(elementsToAdd, newElemsLength)
        _currentAvailable = newSize(_currentAvailable.max(newElemsLength))
      } else {
        _currentArray = new Array[A](_currentAvailable)
        addAllIterableElementsNoOverflow(elementsToAdd, newElemsLength, newAvailable)
      }
    else
      if newAvailable >= 0 then addAllArrayElementsNoOverflow(elementsToAdd, newElemsLength, newAvailable)
      else {
        overflowAppend(_currentArray, _currentUsed)
        overflowAppend(elementsToAdd, newElemsLength)
        _currentAvailable = newSize(_currentArray.length.max(newElemsLength))
        _currentArray = null
        _currentUsed = 0
      }
  }

  private inline def addAllIterableInternal(inline elementsToAdd: Iterable[A], inline newElemsLength: Int): Unit = {
    val newAvailable: Int = _currentAvailable - newElemsLength
    if _currentArray eq null then
      if newElemsLength >= (_currentAvailable >> 1) then {
        // at this point, you have not even allocated [[currentArray]]
        // the first thing you tried to add already took up more than half your buffer
        // just add it to overflow and increase size
        val array: Array[A] = elementsToAdd.toArray[A]
        overflowAppend(array, array.length)
      } else {
        _currentArray = new Array[A](_currentAvailable)
        addAllIterableElementsNoOverflow(elementsToAdd, newElemsLength, newAvailable)
      }
    else
      if newAvailable >= 0 then addAllIterableElementsNoOverflow(elementsToAdd, newElemsLength, newAvailable)
      else {
        overflowAppend(_currentArray, _currentUsed)
        overflowAppend(elementsToAdd.toArray[A], newElemsLength)
        _currentAvailable = newSize(_currentArray.length.max(newElemsLength))
        _currentArray = null
        _currentUsed = 0
      }
  }

  private inline def addAllIteratorInternal(inline elementsToAdd: Iterator[A]): Unit =
    while elementsToAdd.hasNext do {
      val next: A = elementsToAdd.next()
      addSingleInternal(next)
    }

  private inline def addSingleInternal(inline element: A): Unit =
    if _currentAvailable > 0 then {
      if _currentArray eq null then
        _currentArray = new Array[A](_currentAvailable)

      _currentArray(_currentUsed) = element
      _currentAvailable = _currentAvailable - 1
      _currentUsed = _currentUsed + 1
    } else {
      overflowAppend(_currentArray, _currentUsed)
      _currentAvailable = newSize(_currentUsed)
      _currentArray = new Array[A](_currentAvailable)
      _currentArray(0) = element
      _currentAvailable = _currentAvailable - 1
      _currentUsed = 1
    }

  private def addAllBuilderInternal(that: ArrayBuilder.Snapshot[A]): Unit = {
    if _currentArray ne null then
      overflowAppend(_currentArray, _currentUsed)
    _currentArray = null
    _currentAvailable = _currentAvailable + _currentUsed
    _currentUsed = 0

    val extra: Int = if that.currentArray ne null then 1 else 0

    if this._overflowUsed + that.overflowUsed + extra > this._overflowBuffer.length then {
      val newBuffer: Array[ArrayBuilder.PotentiallyPartialArray[A]] = new Array[ArrayBuilder.PotentiallyPartialArray[A]](this._overflowBuffer.length + that.overflowBuffer.length + 32)
      if this._overflowUsed > 0 then
        System.arraycopy(this._overflowBuffer, 0, newBuffer, 0, this._overflowUsed)
    }

    if that.overflowUsed > 0 then {
      System.arraycopy(that.overflowBuffer, 0, this._overflowBuffer, this._overflowUsed, that.overflowUsed)
      this._overflowUsed = this._overflowUsed + 1
    }

    if that.currentArray ne null then {
      this._overflowBuffer(this._overflowUsed) = new ArrayBuilder.PotentiallyPartialArray[A](that.currentArray, that.currentUsed)
      this._overflowUsed = this._overflowUsed + 1
    }
  }

  private inline def snapshotInternal(): ArrayBuilder.Snapshot[A] =
    ArrayBuilder.Snapshot[A](this, _overflowBuffer, _overflowTotalUsedElems, _overflowAllocatedElems, _overflowUsed, _currentArray, _currentUsed, _currentAvailable)

  // TODO (KR) :
  /*
  private def snapshotFromSelfOrCopyInternal(expOverflowUsed: Int, expCurrentUsed: Int)(f: ArrayBuilder[A] => Unit): ArrayBuilder.Snapshot[A] = {
    val target: ArrayBuilder[A] =
      if this._overflowUsed == expOverflowUsed && this._currentUsed == expCurrentUsed then this
      else {
        val copied = new ArrayBuilder[A](threadUnsafe)
        copied.addAllBuilder(this)
        copied
      }

    f(target)
    target.safeSnapshot()
  }
   */

}
object ArrayBuilder {

  def empty[A: ClassTag]: ArrayBuilder[A] = new ArrayBuilder[A](true)
  def emptyThreadUnsafe[A: ClassTag]: ArrayBuilder[A] = new ArrayBuilder[A](true)
  def emptyThreadSafe[A: ClassTag]: ArrayBuilder[A] = new ArrayBuilder[A](false)

  private final case class PotentiallyPartialArray[A](array: Array[A], used: Int) extends Iterable[A] {

    override def iterator: Iterator[A] = new PartialArrayIterator[A](array, used)

    // TODO (KR) : known size

  }

  trait Report {
    val overflowTotalUsedElems: Long
    val overflowAllocatedElems: Long
    val overflowUsed: Int
    val currentUsed: Int
    val currentAvailable: Int
    lazy val totalSize: Int

    def isStillValid(): Boolean
    def underlyingChanged(): Boolean

    final def sameState(that: Report): Boolean =
      this.overflowUsed == that.overflowUsed && this.currentUsed == that.currentUsed

  }

  private final case class Snapshot[A](
      origin: ArrayBuilder[A],
      overflowBuffer: Array[PotentiallyPartialArray[A]],
      overflowTotalUsedElems: Long,
      overflowAllocatedElems: Long,
      overflowUsed: Int,
      currentArray: Array[A],
      currentUsed: Int,
      currentAvailable: Int,
  ) extends Seq[A], Report {

    private given aCt: ClassTag[A] = origin.ct

    lazy val totalSize: Int = {
      val total: Long = overflowTotalUsedElems + currentUsed
      if total > jdk.internal.util.ArraysSupport.SOFT_MAX_ARRAY_LENGTH then
        throw new RuntimeException(s"Unable to build array of length $total, maxArraySize=${jdk.internal.util.ArraysSupport.SOFT_MAX_ARRAY_LENGTH}")
      total.toInt
    }

    override def isStillValid(): Boolean =
      origin._overflowUsed == overflowUsed && origin._currentUsed == currentUsed

    override def underlyingChanged(): Boolean =
      origin._overflowUsed != overflowUsed || origin._currentUsed != currentUsed

    override def apply(i: Int): A = arrayOfA(i)

    override def productPrefix: String = super.productPrefix

    override protected def className: String = "ArrayBuilder.Snapshot"

    lazy val arrayOfA: Array[A] = {
      val output: Array[A] = new Array[A](totalSize)

      var _idx: Int = 0
      var _offset: Int = 0

      while _idx < overflowUsed do {
        val tmp = overflowBuffer(_idx)
        System.arraycopy(tmp.array, 0, output, _offset, tmp.used)
        _idx = _idx + 1
        _offset = _offset + tmp.used
      }

      if currentArray ne null then
        System.arraycopy(currentArray, 0, output, _offset, currentUsed)

      output
    }

    lazy val reversedArrayOfA: Array[A] = arrayOfA.reverse

    override def toArray[B >: A: ClassTag as bCt]: Array[B] =
      if bCt.runtimeClass.isAssignableFrom(aCt.runtimeClass) then
        arrayOfA.asInstanceOf[Array[B]]
      else {
        val output: Array[B] = new Array[B](totalSize)

        var _idx: Int = 0
        var _offset: Int = 0

        while _idx < overflowUsed do {
          val tmp = overflowBuffer(_idx)
          Array.copy(tmp.array, 0, output, _offset, tmp.used)
          _idx = _idx + 1
          _offset = _offset + 1
        }

        if currentArray ne null then
          Array.copy(currentArray, 0, output, _idx, currentUsed)

        output
      }

    override def knownSize: Int = totalSize

    override def length: Int = totalSize

    override def toVector: Vector[A] = toArraySeq.toVector

    def toArraySeq: ArraySeq[A] = ArraySeq.unsafeWrapArray(arrayOfA)

    override def toIndexedSeq: IndexedSeq[A] = toArraySeq

    override protected def reversed: Iterable[A] = ArraySeq.unsafeWrapArray(reversedArrayOfA)

    override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Int = {
      val toMove: Int = len min totalSize
      Array.copy(arrayOfA, 0, xs, start, toMove)
      toMove
    }

    override def iterator: Iterator[A] =
      new SnapshotIterator(
        overflow = overflowBuffer,
        overflowUsed = overflowUsed,
        currentArray = currentArray,
        currentArrayUsed = currentUsed,
      )

  }

  private final case class PartialArrayIterator[A](array: Array[A], used: Int) extends Iterator[A] {

    def this(ppa: PotentiallyPartialArray[A]) = this(ppa.array, ppa.used)

    private var _offset: Int = 0

    override def hasNext: Boolean = _offset < used

    override def next(): A = {
      val value: A = array(_offset)
      _offset = _offset + 1
      value
    }

    override def knownSize: Int = used - _offset

  }

  private final class SnapshotIterator[A](
      overflow: Array[PotentiallyPartialArray[A]],
      overflowUsed: Int,
      currentArray: Array[A],
      currentArrayUsed: Int,
  ) extends Iterator[A] {

    private var _usedCurrent: Boolean = false
    private var _offset: Int = 0
    private var _current: PartialArrayIterator[A] =
      if overflowUsed > 0 then new PartialArrayIterator[A](overflow(0))
      else if currentArray ne null then {
        _usedCurrent = true
        new PartialArrayIterator[A](currentArray, currentArrayUsed)
      } else null

    private def cycle(): Boolean = {
      _offset = _offset + 1
      if _offset < overflowUsed then {
        _current = new PartialArrayIterator(overflow(_offset))
        true
      } else if (currentArray ne null) && !_usedCurrent then {
        _usedCurrent = true
        _current = new PartialArrayIterator[A](currentArray, currentArrayUsed)
        true
      } else {
        _current = null
        false
      }
    }

    override def hasNext: Boolean =
      (_current ne null) && (_current.hasNext || cycle())

    override def next(): A =
      _current.next()

  }

}
