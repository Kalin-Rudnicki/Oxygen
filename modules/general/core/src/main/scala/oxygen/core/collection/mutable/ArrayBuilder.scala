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
    var tmpNode: ArrayBuilder.OverflowNode[A] = overflowHead

    sb.append("ArrayBuilder[")
    sb.append(ct.runtimeClass.getName)
    sb.append(s"]:\n  overflow (total = $overflowTotalLength):")

    while tmpNode != null do {
      sb.append("\n    - ( size = ")
      sb.append(tmpNode.value.length)
      sb.append(", used = ")
      sb.append(tmpNode.used)
      sb.append(" ): \n      ")
      sb.append(tmpNode.value.mkString("[", ", ", "]"))
      tmpNode = tmpNode.next
    }

    sb.append("\n  current:\n    ( used = ")
    sb.append(currentUsed)
    sb.append(", available = ")
    sb.append(currentAvailable)
    sb.append("):\n    ")
    if currentArray eq null then sb.append("<empty-buffer>")
    else sb.append(currentArray.mkString("[", ", ", "]"))

    sb.result()
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private var overflowTotalLength: Long = 0
  private val overflow: LinkedList[ArrayBuilder.PotentiallyPartialArray[A]] = LinkedList.empty[ArrayBuilder.PotentiallyPartialArray[A]](!threadUnsafe)
  private def overflowAppend(array: Array[A], used: Int): Unit = {
    overflow.append(ArrayBuilder.PotentiallyPartialArray[A](array, used))
    overflowTotalLength = overflowTotalLength + used
  }

  private var currentArray: Array[A] = null
  private var currentUsed: Int = 0
  private var currentAvailable: Int = initialSize

  private inline def newSize(inline base: Int): Int = {
    val newSizeDouble: Double = base.toLong * growthFactor
    if newSizeDouble <= Int.MaxValue then newSizeDouble.toInt
    else Int.MaxValue
  }

  private inline def addAllArrayElementsNoOverflow(inline elementsToAdd: Array[A], inline newElemsLength: Int, inline newAvailable: Int): Unit = {
    System.arraycopy(elementsToAdd, 0, currentArray, currentUsed, newElemsLength)
    currentUsed = currentUsed + newElemsLength
    currentAvailable = newAvailable
  }

  private inline def addAllIterableElementsNoOverflow(inline elementsToAdd: Iterable[A], inline newElemsLength: Int, inline newAvailable: Int): Unit = {
    elementsToAdd.copyToArray(currentArray, currentUsed, newElemsLength)
    currentUsed = currentUsed + newElemsLength
    currentAvailable = newAvailable
  }

  private inline def addAllArrayElementsInternal(inline elementsToAdd: Array[A], inline newElemsLength: Int): Unit = {
    val newAvailable: Int = currentAvailable - newElemsLength
    if currentArray eq null then
      if newElemsLength >= (currentAvailable >> 1) then {
        // at this point, you have not even allocated [[currentArray]]
        // the first thing you tried to add already took up more than half your buffer
        // just add it to overflow and increase size
        overflowAppend(elementsToAdd, newElemsLength)
        currentAvailable = newSize(currentAvailable.max(newElemsLength))
      } else {
        currentArray = new Array[A](currentAvailable)
        addAllIterableElementsNoOverflow(elementsToAdd, newElemsLength, newAvailable)
      }
    else
      if newAvailable >= 0 then addAllArrayElementsNoOverflow(elementsToAdd, newElemsLength, newAvailable)
      else {
        overflowAppend(currentArray, currentUsed)
        overflowAppend(elementsToAdd, newElemsLength)
        currentAvailable = newSize(currentArray.length.max(newElemsLength))
        currentArray = null
        currentUsed = 0
      }
  }

  private inline def addAllIterableInternal(inline elementsToAdd: Iterable[A], inline newElemsLength: Int): Unit = {
    val newAvailable: Int = currentAvailable - newElemsLength
    if currentArray eq null then
      if newElemsLength >= (currentAvailable >> 1) then {
        // at this point, you have not even allocated [[currentArray]]
        // the first thing you tried to add already took up more than half your buffer
        // just add it to overflow and increase size
        val array: Array[A] = elementsToAdd.toArray[A]
        overflowAppend(array, array.length)
      } else {
        currentArray = new Array[A](currentAvailable)
        elementsToAdd.copyToArray[A](currentAvailable, 0, newElemsLength)
        currentAvailable = currentAvailable - newElemsLength
      }
    else
      if newAvailable >= 0 then addAllArrayElementsNoOverflow(elementsToAdd, newElemsLength, newAvailable)
      else {
        overflowAppend(currentArray, currentUsed)
        overflowAppend(elementsToAdd, newElemsLength)
        currentAvailable = newSize(currentArray.length.max(newElemsLength))
        currentArray = null
        currentUsed = 0
      }
  }

  private inline def addAllIteratorInternal(inline elementsToAdd: Iterator[A]): Unit =
    while elementsToAdd.hasNext do {
      val next: A = elementsToAdd.next()
      addSingleInternal(next)
    }

  private inline def addSingleInternal(inline element: A): Unit =
    if currentAvailable > 0 then // not possible :   currentArray == null && currentAvailable == 0
      if currentArray eq null then {
        currentArray = new Array[A](currentAvailable)
        currentArray(0) = element
        currentAvailable = currentAvailable - 1
        currentUsed = 1
      } else {
        overflowAppend(currentArray, currentUsed)
        currentArray(currentUsed) = element
        currentAvailable = currentAvailable - 1
        currentUsed = currentUsed + 1
      }
    else {
      currentArray(currentUsed) = element
      currentAvailable = currentAvailable - 1
      currentUsed = 1
    }

  private inline def nodeIterableInternal(): Iterable[ArrayBuilder.PotentiallyPartialArray[A]] =
    if currentArray eq null then overflow.snapshot()
    else overflow.snapshot() ++ Iterable.single(ArrayBuilder.PotentiallyPartialArray[A](currentArray, currentUsed))

  private inline def snapshotInternal(): Iterable[A] =
    nodeIterableInternal().flatMap(_.iterator)

  private inline def buildArrayInternal(): Array[A] = {
    val finalSize: Long = overflowTotalLength + currentUsed
    if finalSize > Int.MaxValue then
      throw new RuntimeException(s"Unable to build array of length $finalSize, maxArraySize=Int.MaxValue=${Int.MaxValue}")
    val output: Array[A] = new Array[A](finalSize.toInt)

    var offset: Int = 0

    val nodeIter: Iterator[ArrayBuilder.PotentiallyPartialArray[A]] = nodeIterableInternal()
    while nodeIter.hasNext do {
      val node = nodeIter.next()
      System.arraycopy(node.array, 0, output, offset, node.used)
      offset = offset + node.used
    }

    output
  }

}
object ArrayBuilder {

  def empty[A: ClassTag]: ArrayBuilder = emptyThreadUnsafe[A]
  def emptyThreadUnsafe[A: ClassTag]: ArrayBuilder = new ArrayBuilder[A](32, 2, true)
  def emptyThreadSafe[A: ClassTag]: ArrayBuilder = new ArrayBuilder[A](32, 2, false)

  def empty[A: ClassTag](initialSize: Int): ArrayBuilder = emptyThreadUnsafe[A](initialSize)
  def emptyThreadUnsafe[A: ClassTag](initialSize: Int): ArrayBuilder = new ArrayBuilder[A](initialSize, 2, true)
  def emptyThreadSafe[A: ClassTag](initialSize: Int): ArrayBuilder = new ArrayBuilder[A](initialSize, 2, false)

  def make[A: ClassTag](initialSize: Int, growthFactor: Double, threadSafe: Boolean): ArrayBuilder = new ArrayBuilder[A](initialSize.max(8), growthFactor.max(1), !threadSafe)

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
