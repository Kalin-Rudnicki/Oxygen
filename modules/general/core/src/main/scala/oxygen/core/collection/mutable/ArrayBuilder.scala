package oxygen.core.collection.mutable

import scala.reflect.ClassTag

/**
  * FIX-PRE-MERGE (KR) : Add description
  */
final class ArrayBuilder[A: ClassTag as ct] private (initialSize: Int, growthFactor: Double, private val threadUnsafe: Boolean) {

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

  def addAllBuilder(that: ArrayBuilder[A]): Unit = // FIX-PRE-MERGE (KR) :
    if that eq null then ()
    else if this eq that then {
      if this.threadUnsafe then ???
      else this.synchronized { ??? }
    } else
      if this.threadUnsafe then
        if that.threadUnsafe then ???
        else that.synchronized { ??? }
      else
        if that.threadUnsafe then this.synchronized { ??? } //
        else this.synchronized { that.synchronized { ??? } }

  def <<(array: Array[A]): ArrayBuilder[A] = {
    this.addAllArrayElements(array)
    this
  }

  def iterator(): Iterator[A] = snapshot().iterator

  def snapshot(): Iterable[A] =
    if threadUnsafe then snapshotInternal()
    else this.synchronized { snapshotInternal() }

  def buildArray(): Array[A] =
    if threadUnsafe then buildArrayInternal()
    else this.synchronized { buildArrayInternal() }

  def showInternalState(): String = {
    val sb = new scala.collection.mutable.StringBuilder()

    sb.append("ArrayBuilder[")
    sb.append(ct.runtimeClass.getName)
    sb.append(s"]:\n  overflow ( size = $_numOverflowElems, used = $_overflowUsedLength, total = $_overflowAllocatedLength ):")

    overflow.iterator().foreach { node =>
      sb.append("\n    - ( allocated = ")
      sb.append(node.array.length)
      sb.append(", used = ")
      sb.append(node.used)
      sb.append(" ): \n      ")
      node.array.addString(sb, "[", ", ", "]")
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

  private var _overflowUsedLength: Long = 0
  private var _overflowAllocatedLength: Long = 0
  private var _numOverflowElems: Int = 0
  private val overflow: LinkedList[ArrayBuilder.PotentiallyPartialArray[A]] = LinkedList.empty[ArrayBuilder.PotentiallyPartialArray[A]](!threadUnsafe)
  private def overflowAppend(array: Array[A], used: Int): Unit = {
    overflow.append(ArrayBuilder.PotentiallyPartialArray[A](array, used))
    _overflowUsedLength = _overflowUsedLength + used
    _overflowAllocatedLength = _overflowAllocatedLength + array.length
    _numOverflowElems = _numOverflowElems + 1
  }

  private var _currentArray: Array[A] = null
  private var _currentUsed: Int = 0
  private var _currentAvailable: Int = initialSize

  private inline def newSize(inline base: Int): Int = {
    val newSizeDouble: Double = base.toLong * growthFactor
    if newSizeDouble <= Int.MaxValue then newSizeDouble.toInt
    else Int.MaxValue
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

  private inline def nodeIterableInternal(): Iterable[ArrayBuilder.PotentiallyPartialArray[A]] =
    if _currentArray eq null then overflow.snapshot()
    else overflow.snapshot() ++ Iterable.single(ArrayBuilder.PotentiallyPartialArray[A](_currentArray, _currentUsed))

  private inline def snapshotInternal(): Iterable[A] =
    nodeIterableInternal().flatMap(_.iterator)

  private inline def buildArrayInternal(): Array[A] = {
    val finalSize: Long = _overflowUsedLength + _currentUsed
    if finalSize > Int.MaxValue then
      throw new RuntimeException(s"Unable to build array of length $finalSize, maxArraySize=Int.MaxValue=${Int.MaxValue}")
    val output: Array[A] = new Array[A](finalSize.toInt)

    var offset: Int = 0

    val nodeIter: Iterator[ArrayBuilder.PotentiallyPartialArray[A]] = nodeIterableInternal().iterator
    while nodeIter.hasNext do {
      val node = nodeIter.next()
      System.arraycopy(node.array, 0, output, offset, node.used)
      offset = offset + node.used
    }

    output
  }

}
object ArrayBuilder {

  def empty[A: ClassTag]: ArrayBuilder[A] = emptyThreadUnsafe[A]
  def emptyThreadUnsafe[A: ClassTag]: ArrayBuilder[A] = new ArrayBuilder[A](32, 2, true)
  def emptyThreadSafe[A: ClassTag]: ArrayBuilder[A] = new ArrayBuilder[A](32, 2, false)

  def empty[A: ClassTag](initialSize: Int): ArrayBuilder[A] = emptyThreadUnsafe[A](initialSize)
  def emptyThreadUnsafe[A: ClassTag](initialSize: Int): ArrayBuilder[A] = new ArrayBuilder[A](initialSize, 2, true)
  def emptyThreadSafe[A: ClassTag](initialSize: Int): ArrayBuilder[A] = new ArrayBuilder[A](initialSize, 2, false)

  def make[A: ClassTag](initialSize: Int, growthFactor: Double, threadSafe: Boolean): ArrayBuilder[A] = new ArrayBuilder[A](initialSize.max(8), growthFactor.max(1), !threadSafe)

  private final case class PotentiallyPartialArray[A](array: Array[A], used: Int) extends Iterable[A] {

    override def iterator: Iterator[A] = new PotentiallyPartialArrayIterator(array, used)

    // TODO (KR) : known size

  }

  private final class PotentiallyPartialArrayIterator[A](array: Array[A], used: Int) extends Iterator[A] {

    private var _offset: Int = 0

    override def hasNext: Boolean = _offset < used

    override def next(): A = {
      val value: A = array(_offset)
      _offset = _offset + 1
      value
    }

    // TODO (KR) : known size

  }

}
