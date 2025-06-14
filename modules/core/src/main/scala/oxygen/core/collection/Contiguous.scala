package oxygen.core.collection

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * The goal of this class is to be as close of a wrapper around [[Array]] as [[IArray]] is, without involving [[ClassTag]].
  * [[Array]]/[[IArray]] use [[ClassTag]] in order to create more efficiently sized arrays (bytes take up less space than longs, or objects).
  * In order to do this, the [[Array]]/[[IArray]] types are LITTERED with [[ClassTag]]s.
  * It is the belief of this class that:
  *   1. This hindrance causes [[Array]]/[[IArray]] to be used less than they should.
  *   2. The value provided by space-saving in the case of primitives is MINIMAL.
  *      (Especially since it would usually push the programmer to use a type which is even less efficient).
  */
final class Contiguous[+A] private[collection] (private val array: Array[A @uncheckedVariance]) {
  import Contiguous.typedAnyTag

  val length: Int = array.length
  val lift: Int => Option[A] = array.lift

  def ++[A2 >: A](that: Contiguous[A2]): Contiguous[A2] = {
    val newArray: Array[A2] = new Array[A2](Math.addExact(this.length, that.length))
    System.arraycopy(this.array, 0, newArray, 0, this.length)
    System.arraycopy(that.array, 0, newArray, this.length, that.length)
    new Contiguous[A2](newArray)
  }

  def :+[A2 >: A](that: A2): Contiguous[A2] = new Contiguous[A2](array :+ that)
  def +:[A2 >: A](that: A2): Contiguous[A2] = new Contiguous[A2](that +: array)

  inline def apply(index: Int): A = array(index)
  def at(index: Int): A = array(index)
  inline def get(index: Int): Option[A] = lift(index)

  inline def head: A = array.head
  inline def headOption: Option[A] = array.headOption
  inline def last: A = array.last
  inline def lastOption: Option[A] = array.lastOption

  def map[B](f: A => B): Contiguous[B] =
    new Contiguous[B](array.map(f))

  def flatten[B](using ev: A <:< Contiguous[B]): Contiguous[B] = {
    val mapped: Contiguous[Contiguous[B]] = this.asInstanceOf[Contiguous[Contiguous[B]]]

    var counter: Int = 0
    mapped.foreach { c => counter = Math.addExact(counter, c.length) }

    val newArray: Array[B] = new Array[B](counter)

    counter = 0

    mapped.foreach { c =>
      System.arraycopy(c.array, 0, newArray, counter, c.length)
      counter = counter + c.length
    }

    new Contiguous[B](newArray)
  }
  def flattenIterable[B](using ev: A <:< IterableOnce[B]): Contiguous[B] = {
    val builder: mutable.Builder[B, Contiguous[B]] = Contiguous.newBuilder[B]
    foreach { a => builder.addAll(ev(a)) }
    builder.result()
  }

  def flatMap[B](f: A => Contiguous[B]): Contiguous[B] =
    map(f).flatten
  def flatMapIterable[B](f: A => IterableOnce[B]): Contiguous[B] = {
    val builder: mutable.Builder[B, Contiguous[B]] = Contiguous.newBuilder[B]
    foreach { a => builder.addAll(f(a)) }
    builder.result()
  }

  inline def foreach[U](f: A => U): Unit = array.foreach(f)

  inline def foreachFrom[U](i: Int)(f: A => U): Unit = {
    var idx: Int = i
    while (idx < length) {
      f(array(idx))
      idx = idx + 1
    }
  }

  def partition(f: A => Boolean): (Contiguous[A], Contiguous[A]) = {
    val isTrue: Contiguous.Builder[A] = Contiguous.newBuilder[A]
    val isFalse: Contiguous.Builder[A] = Contiguous.newBuilder[A]
    isTrue.sizeHint(length)
    isFalse.sizeHint(length)
    foreach { c => if (f(c)) isTrue.addOne(c) else isFalse.addOne(c) }
    (isTrue.result(), isFalse.result())
  }
  def partitionMap[A1, A2](f: A => Either[A1, A2]): (Contiguous[A1], Contiguous[A2]) = {
    val isLeft: Contiguous.Builder[A1] = Contiguous.newBuilder[A1]
    val isRight: Contiguous.Builder[A2] = Contiguous.newBuilder[A2]
    isLeft.sizeHint(length)
    isRight.sizeHint(length)
    foreach { c =>
      f(c) match {
        case Right(value) => isRight.addOne(value)
        case Left(value)  => isLeft.addOne(value)
      }
    }
    (isLeft.result(), isRight.result())
  }

  def groupBy[B](key: A => B): Map[B, Contiguous[A]] =
    groupMap(key)(identity)

  def groupMap[B, C](key: A => B)(value: A => C): Map[B, Contiguous[C]] = {
    val map: mutable.Map[B, Contiguous.Builder[C]] = mutable.Map.empty

    this.foreach { a =>
      val k = key(a)
      val b =
        map.get(k) match {
          case Some(builder) => builder
          case None          => val builder = Contiguous.newBuilder[C]; map.update(k, builder); builder
        }
      b.addOne(value(a))
    }

    map.iterator.map { case (k, vs) => (k, vs.result()) }.toMap
  }

  def sorted[B >: A: Ordering]: Contiguous[A] = new Contiguous[A](array.sorted)
  def sortBy[B: Ordering](f: A => B): Contiguous[A] = new Contiguous[A](array.sortBy(f))

  inline def min[B >: A: Ordering]: A = array.min
  inline def minBy[B: Ordering](f: A => B): A = array.minBy(f)
  inline def minOption[B >: A: Ordering]: Option[A] = array.minOption
  inline def minByOption[B: Ordering](f: A => B): Option[A] = array.minByOption(f)

  inline def max[B >: A: Ordering]: A = array.max
  inline def maxBy[B: Ordering](f: A => B): A = array.maxBy(f)
  inline def maxOption[B >: A: Ordering]: Option[A] = array.maxOption
  inline def maxByOption[B: Ordering](f: A => B): Option[A] = array.maxByOption(f)

  inline def sum[B >: A](using numeric: Numeric[B]): B = array.sum[B]

  def filter(f: A => Boolean): Contiguous[A] = new Contiguous[A](array.filter(f))
  def filterNot(f: A => Boolean): Contiguous[A] = new Contiguous[A](array.filterNot(f))
  def collect[B](f: PartialFunction[A, B]): Contiguous[B] = new Contiguous[B](array.collect(f))
  def collectFirst[B](f: PartialFunction[A, B]): Option[B] = array.collectFirst(f)
  def distinct: Contiguous[A] = new Contiguous[A](array.distinct)
  def distinctBy[B](f: A => B): Contiguous[A] = new Contiguous[A](array.distinctBy(f))

  inline def foldLeft[B](z: B)(op: (B, A) => B): B = array.foldLeft(z)(op)
  inline def foldRight[B](z: B)(op: (A, B) => B): B = array.foldRight(z)(op)

  inline def isEmpty: Boolean = array.isEmpty
  inline def nonEmpty: Boolean = array.nonEmpty

  inline def mkString: String = array.mkString
  inline def mkString(sep: String): String = array.mkString(sep)
  inline def mkString(start: String, sep: String, end: String): String = array.mkString(start, sep, end)

  inline def iterator: Iterator[A] = array.iterator
  inline def toSeq: Seq[A] = array.toSeq
  inline def toIndexedSeq: IndexedSeq[A] = array.toIndexedSeq
  inline def toList: List[A] = array.toList
  inline def toVector: Vector[A] = array.toVector
  inline def toArray: Array[A @uncheckedVariance] = array.clone()
  inline def toArraySeq: ArraySeq[A] = ArraySeq.unsafeWrapArray(array)
  inline def toIArray: IArray[A] = IArray.unsafeFromArray(array)
  inline def toSet[B >: A]: Set[B] = array.toSet
  inline def toMap[K, V](implicit ev: A <:< (K, V)): Map[K, V] = array.toMap
  def toIterable: Iterable[A] =
    new Iterable[A] {
      override def iterator: Iterator[A] = array.iterator
    }

  def zipWithIndexFrom(startIdx: Int): Contiguous[(A, Int)] = {
    val builder = Contiguous.newBuilder[(A, Int)]
    val iter = this.iterator
    var idx: Int = startIdx
    while (iter.hasNext) {
      builder.addOne((iter.next(), idx))
      idx += 1
    }
    builder.result()
  }
  def zipWithIndex: Contiguous[(A, Int)] = zipWithIndexFrom(0)

  def zipUsing[B, C](that: Contiguous[B])(leftOnly: A => C, rightOnly: B => C, both: (A, B) => C): Contiguous[C] = {
    var idx: Int = 0

    val minLength = this.length min that.length
    val newArray: Array[C] = new Array[C](this.length max that.length)

    while (idx < minLength) {
      newArray(idx) = both(this(idx), that(idx))
      idx = idx + 1
    }
    while (idx < this.length) {
      newArray(idx) = leftOnly(this(idx))
      idx = idx + 1
    }
    while (idx < that.length) {
      newArray(idx) = rightOnly(that(idx))
      idx = idx + 1
    }

    new Contiguous[C](newArray)
  }

  /**
    * Gets an [[Array]] out of this [[Contiguous]], ensuring that the resulting array has correctly sized elements, instead of using [[Any]] for all types.
    */
  def toTypedArray[B >: A](using classTag: ClassTag[B]): Array[B] = {
    val arr: Array[B] = new Array[B](length)(using classTag)
    Array.copy(array, 0, arr, 0, length)
    arr
  }

  /**
    * Gets an [[Array]] out of this [[Contiguous]], ensuring that the resulting array has correctly sized elements, instead of using [[Any]] for all types.
    */
  def toTypedIArrayArray[B >: A](using classTag: ClassTag[B]): IArray[B] =
    IArray.unsafeFromArray(toTypedArray[B](using classTag))

  override def toString: String =
    mkString("Contiguous(", ", ", ")")

  override def equals(that: Any): Boolean =
    that.asInstanceOf[Matchable] match {
      case that: Contiguous[?] =>

        if (this.length != that.length)
          return false

        var i: Int = 0

        while (i < this.length) {
          if (this.array(i) == that.array(i)) i = i + 1
          else return false
        }

        true
      case _ =>
        false
    }

  def addTo[G[_], B >: A](builder: mutable.Builder[B, G[B]]): Unit = builder.addAll(array)

  private[collection] inline def getRawArray: Array[A @uncheckedVariance] = array

}
object Contiguous {

  lazy val Empty: Contiguous[Nothing] = Contiguous()

  def empty[A]: Contiguous[A] = Empty

  def single[A](value: A): Contiguous[A] = {
    val array: Array[A] = new Array[A](1)
    array(0) = value
    new Contiguous[A](array)
  }

  def apply[A](values: A*): Contiguous[A] =
    new Contiguous[A](Array(values*))

  def fill[A](n: Int)(elem: => A): Contiguous[A] =
    new Contiguous[A](Array.fill[A](n)(elem))

  def fromArray[A](array: Array[A]): Contiguous[A] = {
    val newArray: Array[A] = new Array[A](array.length)
    Array.copy(array, 0, newArray, 0, array.length)
    new Contiguous[A](newArray)
  }

  def fromIArray[A](array: IArray[A]): Contiguous[A] =
    fromArray(array.asInstanceOf[Array[A]])

  def from[A](it: IterableOnce[A]): Contiguous[A] =
    new Contiguous[A](Array.from[A](it))

  def unsafeMakeExposingArray[A](length: Int): (Contiguous[A], Array[A]) = {
    val array: Array[A] = new Array[A](length)
    (new Contiguous[A](array), array)
  }

  type Builder[A] = mutable.Builder[A, Contiguous[A]]

  def newBuilder[A]: Builder[A] =
    Array.newBuilder[A].mapResult(new Contiguous[A](_))

  def unapplySeq[A](contiguous: Contiguous[A]): Option[Seq[A]] =
    Some(contiguous.toSeq)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Tag
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private val anyTag: ClassTag[Any] = ClassTag.Any
  given typedAnyTag: [A] => ClassTag[A] = anyTag.asInstanceOf[ClassTag[A]]

}
