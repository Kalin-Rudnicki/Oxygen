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
  private var overflowHead: ArrayBuilder.OverflowNode[A] = null
  private var overflowTail: ArrayBuilder.OverflowNode[A] = null
  private def overflowAppend(array: Array[A], used: Int): Unit = {
    val newNode = new ArrayBuilder.OverflowNode[A](array, used)
    if overflowTail eq null then {
      overflowHead = newNode
      overflowTail = newNode
    } else {
      overflowTail.next = newNode
      overflowTail = newNode
    }
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

  private inline def snapshotInternal(): ArrayBuilder.Snapshot[A] =
    ArrayBuilder.Snapshot[A](overflowHead, overflowTail, if currentArray eq null then null else new ArrayBuilder.OverflowNode[A](currentArray, currentUsed))

  private inline def buildArrayInternal(): Array[A] = {
    val finalSize: Long = overflowTotalLength + currentUsed
    if finalSize > Int.MaxValue then
      throw new RuntimeException(s"Unable to build array of length $finalSize, maxArraySize=Int.MaxValue=${Int.MaxValue}")
    val output: Array[A] = new Array[A](finalSize.toInt)

    var offset: Int = 0

    inline def addToOutput(inline part: Array[A], inline partSize: Int): Unit = {
      System.arraycopy(part, 0, output, offset, partSize)
      offset = offset + partSize
    }

    var tmpNode: ArrayBuilder.OverflowNode[A] = overflowHead
    while tmpNode != null do {
      addToOutput(tmpNode.value, tmpNode.used)
      tmpNode = tmpNode.next
    }

    if currentArray != null then
      addToOutput(currentArray, currentUsed)

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

  private final class OverflowNode[A](val value: Array[A], val used: Int) {
    var next: OverflowNode[A] = null
  }

  private final class OverflowNodeIterator[A](head: OverflowNode[A], tail: OverflowNode[A]) extends Iterator[OverflowNode[A]] {

    private var node: OverflowNode[A] = head

    override def hasNext: Boolean = node != null

    override def next(): OverflowNode[A] = {
      val value: OverflowNode[A] = node

      if value eq tail then
        node = null
      else
        node = node.next

      value
    }

  }

  private final class OverflowNodeElementIterator[A](node: OverflowNode[A]) extends Iterator[A] {

    private var offset: Int = 0

    override def hasNext: Boolean = offset < node.used

    override def next(): A = {
      val value: A = node.value(offset)
      offset = offset + 1
      value
    }

  }

  /**
    * Immutable snapshot of an [[ArrayBuilder]].
    * Because of the way this is stored, things can technically be added to the array within [[current]],
    * but because it has take a snapshot of its [[ArrayBuilder.currentUsed]], it doesn't matter.
    */
  private final case class Snapshot[A](overflowHead: OverflowNode[A], overflowTail: OverflowNode[A], current: OverflowNode[A]) extends Iterable[A] {

    def nodeIterator: Iterator[OverflowNode[A]] = {
      val overflowIter: Iterator[OverflowNode[A]] = new OverflowNodeIterator[A](overflowHead, overflowTail)
      if current eq null then overflowIter
      else overflowIter ++ Iterator.single(current)
    }

    def iterator: Iterator[A] = nodeIterator.flatMap(new OverflowNodeElementIterator[A](_))

  }

  private final class ArrayBuilderIterator[A](snapshot: Snapshot[A]) extends Iterator[A] {

    var consumingOverflow: Boolean = snapshot.overflowHead != null
    var node: OverflowNode[A] = if consumingOverflow then snapshot.overflowHead else snapshot.current
    var offset: Int = 0

    override def hasNext: Boolean = node != null

    override def next(): A = {
      val value: A = node.value(offset)

      offset = offset + 1
      if offset >= node.used then {
        if consumingOverflow then
          if node.next eq snapshot.overflowTail then {
            node = snapshot.current
            consumingOverflow = false
          } else
            node = node.next
        else
          node = null
      }

      value
    }

  }

}
