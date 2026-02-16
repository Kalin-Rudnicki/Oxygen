package oxygen.core.collection.mutable

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

final class ArrayBuilder[A] private (private val threadUnsafe: Boolean)(using private val ct: ClassTag[A]) {

  def addStringChars(string: String)(using ev: A =:= Char): Unit = {
    val string0: String = if string ne null then string else "null"
    val strLen: Int = string0.length
    if strLen == 0 then ()
    else if threadUnsafe then this.current.addStringChars(string0, strLen)
    else this.synchronized { this.current.addStringChars(string0, strLen) }
  }

  def addAllArrayElements(elementsToAdd: Array[A]): Unit =
    if threadUnsafe then this.current.append(elementsToAdd)
    else this.synchronized { this.current.append(elementsToAdd) }

  def addAllIterable(elementsToAdd: Iterable[A]): Unit =
    if elementsToAdd eq null then ()
    else {
      val knownSize: Int = elementsToAdd.knownSize
      if knownSize == 0 then ()
      else if knownSize == -1 then addAllIterator(elementsToAdd.iterator)
      else
        if threadUnsafe then this.current.addIterable(elementsToAdd, knownSize)
        else this.synchronized { this.current.addIterable(elementsToAdd, knownSize) }
    }

  def addAllIterator(elementsToAdd: Iterator[A]): Unit =
    if elementsToAdd eq null then ()
    else if threadUnsafe then this.current.addAllIteratorInternal(elementsToAdd)
    else this.synchronized { this.current.addAllIteratorInternal(elementsToAdd) }

  def addSingle(element: A): Unit =
    if threadUnsafe then this.current.appendOne(element)
    else this.synchronized { this.current.appendOne(element) }

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
    if threadUnsafe then current._used == 0 && overflow._totalUsedElems == 0
    else this.synchronized { current._used == 0 && overflow._totalUsedElems == 0 }
  def nonEmpty(): Boolean =
    if threadUnsafe then current._used > 0 || overflow._totalUsedElems > 0
    else this.synchronized { current._used > 0 || overflow._totalUsedElems > 0 }
  def size(): Int = {
    val total: Long =
      if threadUnsafe then overflow._totalUsedElems + current._used
      else this.synchronized { overflow._totalUsedElems + current._used }
    if total > jdk.internal.util.ArraysSupport.SOFT_MAX_ARRAY_LENGTH then
      throw new RuntimeException(s"Unable to build array of length $total, maxArraySize=${jdk.internal.util.ArraysSupport.SOFT_MAX_ARRAY_LENGTH}")
    total.toInt
  }

  def showInternalState(): String = {
    val sb = new scala.collection.mutable.StringBuilder()

    sb.append("ArrayBuilder[")
    sb.append(ct.runtimeClass.getName)
    sb.append(s"]:\n  overflow ( bufferSize = ${overflow._used}, total.used = ${overflow._totalUsedElems}, total.allocated = ${overflow._totalAllocatedElems} ):")

    var _tmpIdx: Int = 0
    while _tmpIdx < overflow._used do {
      val node = overflow._array(_tmpIdx)
      sb.append("\n    - ( allocated = ")
      sb.append(node.array.length)
      sb.append(", used = ")
      sb.append(node.used)
      sb.append(" ): \n      ")
      node.array.addString(sb, "[", ", ", "]")
      _tmpIdx = _tmpIdx + 1
    }

    sb.append(s"\n  current:\n    ( allocated = ${if current._array eq null then "<N/A>" else current._array.length}, used = ")
    sb.append(current._used)
    sb.append(", available = ")
    sb.append(current._available)
    sb.append("):\n    ")
    if current._array eq null then sb.append("<empty-buffer>")
    else current._array.addString(sb, "[", ", ", "]")

    sb.result()
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private val overflow: ArrayBuilder.OverflowBuffer[A] = ArrayBuilder.OverflowBuffer.empty[A](32)
  private val current: ArrayBuilder.CurrentBuffer[A] = ArrayBuilder.CurrentBuffer.empty[A](overflow, 32)

  private def addAllBuilderInternal(that: ArrayBuilder.Snapshot[A]): Unit = {
    this.current.addToOverflowIfNonEmpty()
    this.overflow.append(that.overflow)
    this.overflow.append(that.current)
  }

  private inline def snapshotInternal(): ArrayBuilder.Snapshot[A] =
    ArrayBuilder.Snapshot[A](this, ArrayBuilder.Snapshot.Overflow.from(overflow), ArrayBuilder.Snapshot.Current.from(current))

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

  private final class OverflowBuffer[A](
      var _array: Array[PotentiallyPartialArray[A]],
      var _used: Int,
      var _totalUsedElems: Long,
      var _totalAllocatedElems: Long,
  ) {

    private inline def ensureSize(newSize: Int): Unit =
      if newSize < _array.length then {
        val newArray: Array[PotentiallyPartialArray[A]] = new Array[PotentiallyPartialArray[A]](newSize + 32)
        System.arraycopy(this._array, 0, newArray, 0, _used)
        this._array = newArray
      }

    inline def append(that: Array[A]): Unit = {
      val newSize: Int = this._used + 1
      this.ensureSize(newSize)
      this._array(this._used) = PotentiallyPartialArray(that, that.length)
      this._used = newSize
      this._totalUsedElems = this._totalUsedElems + that.length
      this._totalAllocatedElems = this._totalAllocatedElems + that.length
    }

    inline def append(that: Snapshot.Current[A]): Unit =
      if that.used > 0 then {
        val newSize: Int = this._used + 1
        this.ensureSize(newSize)
        this._array(this._used) = PotentiallyPartialArray(that.array, that.used)
        this._used = newSize
        this._totalUsedElems = this._totalUsedElems + that.used
        this._totalAllocatedElems = this._totalAllocatedElems + that.array.length
      }

    inline def append(that: CurrentBuffer[A]): Unit =
      if that._used > 0 then {
        val newSize: Int = this._used + 1
        this.ensureSize(newSize)
        this._array(this._used) = PotentiallyPartialArray(that._array, that._used)
        this._used = newSize
        this._totalUsedElems = this._totalUsedElems + that._used
        this._totalAllocatedElems = this._totalAllocatedElems + that._array.length
      }

    inline def append(that: Snapshot.Overflow[A]): Unit =
      if that.used > 0 then {
        val newSize: Int = this._used + that.used
        this.ensureSize(newSize)
        System.arraycopy(that.array, 0, this._array, this._used, that.used)
        this._used = newSize
        this._totalUsedElems = this._totalUsedElems + that.totalUsedElems
        this._totalAllocatedElems = this._totalAllocatedElems + that.totalAllocatedElems
      }

    inline def append(that: OverflowBuffer[A]): Unit =
      if that._used > 0 then {
        val newSize: Int = this._used + that._used
        this.ensureSize(newSize)
        System.arraycopy(that._array, 0, this._array, this._used, that._used)
        this._used = newSize
        this._totalUsedElems = this._totalUsedElems + that._totalUsedElems
        this._totalAllocatedElems = this._totalAllocatedElems + that._totalAllocatedElems
      }

  }
  private object OverflowBuffer {

    def empty[A](size: Int): OverflowBuffer[A] = new OverflowBuffer[A](new Array[PotentiallyPartialArray[A]](size), 0, 0, 0)

  }

  private final class CurrentBuffer[A: ClassTag](
      overflow: OverflowBuffer[A],
      var _array: Array[A],
      var _used: Int,
      var _available: Int,
  ) {

    // TODO (KR) :
    /*
  private inline def newSize(inline base: Int): Int = {
    val newSizeLong: Long = base.toLong * 2
    if newSizeLong <= jdk.internal.util.ArraysSupport.SOFT_MAX_ARRAY_LENGTH then newSizeLong.toInt
    else jdk.internal.util.ArraysSupport.SOFT_MAX_ARRAY_LENGTH
  }
     */

    private inline def growTo(size: Int): Unit = {
      overflow.append(this)
      this._array = null
      this._used = 0
      this._available = size
    }
    private inline def doubleSize(): Unit =
      if this._array ne null then growTo(this._array.length * 2)
      else this._available = this._available * 2

    private inline def safeAddValues(values: Array[A]): Unit = {
      System.arraycopy(values, 0, this._array, this._used, values.length)
      this._used = this._used + values.length
      this._available = this._available - values.length
    }

    def append(values: Array[A]): Unit =
      if (values ne null) && values.length > 0 then {
        val combinedSize: Int = this._used + values.length
        if _array ne null then
          if combinedSize > this._array.length then {
            this.doubleSize()
            this.append(values)
          } else
            this.safeAddValues(values)
        else
          if values.length >= this._available / 2 then {
            this.overflow.append(values)
            this._available = this._available * 2
          } else {
            this._array = new Array[A](_available)
            this.safeAddValues(values)
          }
      }

    inline def addAllIteratorInternal(inline elementsToAdd: Iterator[A]): Unit =
      while elementsToAdd.hasNext do {
        val next: A = elementsToAdd.next()
        this.appendOne(next)
      }

    private inline def addAllIterableElementsNoOverflow(inline elementsToAdd: Iterable[A], inline newElemsLength: Int, inline newAvailable: Int): Unit = {
      elementsToAdd.copyToArray(this._array, this._used, newElemsLength)
      this._used = this._used + newElemsLength
      this._available = newAvailable
    }

    private inline def addStringCharsNoOverflow(inline string: String, inline newElemsLength: Int, inline newAvailable: Int): Unit = {
      val castCurrentArray: Array[Char] = this._array.asInstanceOf[Array[Char]]
      string.getChars(0, newElemsLength, castCurrentArray, this._used)
      this._used = this._used + newElemsLength
      this._available = newAvailable
    }

    inline def addStringChars(string: String, newElemsLength: Int): Unit = {
      val newAvailable: Int = this._available - newElemsLength
      if this._array eq null then
        if newElemsLength >= (this._available / 2) then {
          // at this point, you have not even allocated [[currentArray]]
          // the first thing you tried to add already took up more than half your buffer
          // just add it to overflow and increase size
          this.overflow.append(string.toCharArray.asInstanceOf[Array[A]])
          this._available = this._available * 2
        } else {
          this._array = new Array[A](this._available)
          addStringCharsNoOverflow(string, newElemsLength, newAvailable)
        }
      else
        if newAvailable >= 0 then {
          addStringCharsNoOverflow(string, newElemsLength, newAvailable)
        } else {
          this.doubleSize()
          this.overflow.append(string.toCharArray.asInstanceOf[Array[A]])
        }
    }

    inline def addIterable(elementsToAdd: Iterable[A], newElemsLength: Int): Unit = {
      val newAvailable: Int = _available - newElemsLength
      if this._array eq null then
        if newElemsLength >= (_available / 2) then {
          // at this point, you have not even allocated [[currentArray]]
          // the first thing you tried to add already took up more than half your buffer
          // just add it to overflow and increase size
          val array: Array[A] = elementsToAdd.toArray[A]
          this.overflow.append(array)
        } else {
          this._array = new Array[A](_available)
          this.addAllIterableElementsNoOverflow(elementsToAdd, newElemsLength, newAvailable)
        }
      else
        if newAvailable >= 0 then this.addAllIterableElementsNoOverflow(elementsToAdd, newElemsLength, newAvailable)
        else {
          this.doubleSize()
          this.overflow.append(elementsToAdd.toArray[A])
        }
    }

    inline def appendOne(value: A): Unit = {
      if this._available <= 0 then {
        if this._array eq null then
          throw new RuntimeException(s"WTF: ${this._used}, ${this._available}")

        this.doubleSize()
      }

      if this._array eq null then
        this._array = new Array[A](this._available)

      this._array(this._used) = value
      this._used = this._used + 1
      this._available = this._available - 1
    }

    inline def addToOverflowIfNonEmpty(): Unit =
      if this._used > 0 then
        this.growTo(this._array.length)

  }
  private object CurrentBuffer {

    def empty[A: ClassTag](overflow: OverflowBuffer[A], size: Int): CurrentBuffer[A] = new CurrentBuffer[A](overflow, null, 0, size)

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
      overflow: Snapshot.Overflow[A],
      current: Snapshot.Current[A],
  ) extends Seq[A], Report {

    val overflowBuffer: Array[PotentiallyPartialArray[A]] = overflow.array
    override val overflowUsed: Int = overflow.used
    override val overflowTotalUsedElems: Long = overflow.totalUsedElems
    override val overflowAllocatedElems: Long = overflow.totalAllocatedElems

    val currentArray: Array[A] = current.array
    override val currentUsed: Int = current.used
    override val currentAvailable: Int = current.available

    private given aCt: ClassTag[A] = origin.ct

    lazy val totalSize: Int = {
      val total: Long = overflowTotalUsedElems + currentUsed
      if total > jdk.internal.util.ArraysSupport.SOFT_MAX_ARRAY_LENGTH then
        throw new RuntimeException(s"Unable to build array of length $total, maxArraySize=${jdk.internal.util.ArraysSupport.SOFT_MAX_ARRAY_LENGTH}")
      total.toInt
    }

    override def isStillValid(): Boolean =
      origin.overflow._used == overflowUsed && origin.current._used == currentUsed

    override def underlyingChanged(): Boolean =
      origin.overflow._used != overflowUsed || origin.current._used != currentUsed

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
  private object Snapshot {

    final case class Overflow[A] private (
        array: Array[PotentiallyPartialArray[A]],
        used: Int,
        totalUsedElems: Long,
        totalAllocatedElems: Long,
    )
    object Overflow {

      def from[A](overflow: ArrayBuilder.OverflowBuffer[A]): Overflow[A] =
        Overflow(
          array = overflow._array,
          used = overflow._used,
          totalUsedElems = overflow._totalUsedElems,
          totalAllocatedElems = overflow._totalAllocatedElems,
        )

    }

    final case class Current[A] private (
        array: Array[A],
        used: Int,
        available: Int,
    )
    object Current {

      def from[A](current: ArrayBuilder.CurrentBuffer[A]): Current[A] =
        Current(
          array = current._array,
          used = current._used,
          available = current._available,
        )

    }

  }

  private final case class PartialArrayIterator[A](array: Array[A], used: Int) extends Iterator[A] {

    if used > 0 && used > array.length then
      throw new RuntimeException(s"len = ${array.length}, used = $used")

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
