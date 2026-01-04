package oxygen.core.collection.mutable

/**
  * Supports appending and prepending.
  */
final class LinkedList[A] private (threadUnsafe: Boolean) {

  private var _size: Int = 0
  private var _head: LinkedList.Node[A] = null
  private var _tail: LinkedList.Node[A] = null

  def unsafeHeadNode: LinkedList.Node[A] = _head
  def unsafeTailNode: LinkedList.Node[A] = _tail
  def headNode: Option[LinkedList.Node[A]] = Option(_head)
  def tailNode: Option[LinkedList.Node[A]] = Option(_tail)

  def prepend(value: A): Unit =
    if threadUnsafe then prependInternal(value)
    else this.synchronized { prependInternal(value) }

  def append(value: A): Unit =
    if threadUnsafe then appendInternal(value)
    else this.synchronized { appendInternal(value) }

  // [A, B, C].prependAllInReverse([D, E, F]) -> [D, E, F, A, B, C]
  def prependAll(values: Seq[A]): Unit =
    if threadUnsafe then {
      val it = values.reverseIterator
      while it.hasNext do prepend(it.next())
    } else
      this.synchronized {
        val it = values.reverseIterator
        while it.hasNext do prepend(it.next())
      }

  // [A, B, C].prependAllInReverse([D, E, F]) -> [F, E, D, A, B, C]
  def prependAllInReverse(values: IterableOnce[A]): Unit =
    if threadUnsafe then {
      val it = values.iterator
      while it.hasNext do prepend(it.next())
    } else
      this.synchronized {
        val it = values.iterator
        while it.hasNext do prepend(it.next())
      }

  def appendAll(values: IterableOnce[A]): Unit =
    if threadUnsafe then {
      val it = values.iterator
      while it.hasNext do append(it.next())
    } else
      this.synchronized {
        val it = values.iterator
        while it.hasNext do append(it.next())
      }

  def iterator(): Iterator[A] = snapshot().iterator

  def snapshot(): Iterable[A] =
    if threadUnsafe then LinkedList.Snapshot[A](false, _head, _tail)
    else this.synchronized { LinkedList.Snapshot[A](false, _head, _tail) }

  private inline def prependInternal(inline value: A): Unit = {
    val node: LinkedList.Node[A] = new LinkedList.Node[A](value)
    if _head eq null then {
      _head = node
      _tail = node
      _size = 1
    } else {
      node._next = _head
      _head._prev = node
      _head = node
    }
  }

  private inline def appendInternal(inline value: A): Unit = {
    val node: LinkedList.Node[A] = new LinkedList.Node[A](value)
    if _head eq null then {
      _head = node
      _tail = node
      _size = 1
    } else {
      _tail._next = node
      node._prev = _tail
      _tail = node
    }
  }

}
object LinkedList {

  final class Node[A] private[LinkedList] (val value: A) {

    private[LinkedList] var _prev: Node[A] = null
    private[LinkedList] var _next: Node[A] = null

  }

  private final case class Snapshot[A](isReversed: Boolean, head: Node[A], tail: Node[A]) extends Iterable[A] {

    override def iterator: Iterator[A] =
      if isReversed then ReversedSnapshotIterator(this)
      else SnapshotIterator(this)

    override protected def reversed: Iterable[A] = Snapshot(!isReversed, head, tail)

  }

  private final case class SnapshotIterator[A](snapshot: Snapshot[A]) extends Iterator[A] {

    private var _node: Node[A] = snapshot.head

    override def hasNext: Boolean = _node != null

    override def next(): A = {
      val value: A = _node.value

      if _node eq snapshot.tail then
        _node = null
      else
        _node = _node._next

      value
    }

  }

  private final case class ReversedSnapshotIterator[A](snapshot: Snapshot[A]) extends Iterator[A] {

    private var _node: Node[A] = snapshot.tail

    override def hasNext: Boolean = _node != null

    override def next(): A = {
      val value: A = _node.value

      if _node eq snapshot.head then
        _node = null
      else
        _node = _node._prev

      value
    }

  }

}
