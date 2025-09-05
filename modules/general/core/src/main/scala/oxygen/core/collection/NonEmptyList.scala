package oxygen.core.collection

import oxygen.core.Ior
import oxygen.core.syntax.groupBy.*
import oxygen.core.syntax.option.*
import oxygen.core.typeclass.SeqRead
import scala.collection.mutable
import scala.quoted.*
import scala.reflect.ClassTag

final case class NonEmptyList[+A](head: A, tail: List[A]) extends PartialFunction[Int, A] {

  val toList: List[A] = head :: tail

  private inline def unsafeTransformList[B](inline f: List[A] => List[B]): NonEmptyList[B] = NonEmptyList.unsafeFromList(f(toList))

  // single builders
  def ::[A2 >: A](value: A2): NonEmptyList[A2] = NonEmptyList(value, toList)
  def +:[A2 >: A](value: A2): NonEmptyList[A2] = NonEmptyList(value, toList)
  def :+[A2 >: A](value: A2): NonEmptyList[A2] = NonEmptyList(head, tail :+ value)
  def prepended[B >: A](value: B): NonEmptyList[B] = NonEmptyList(value, toList)
  def appended[B >: A](value: B): NonEmptyList[B] = NonEmptyList(head, tail :+ value)

  // multi builders
  def :::[A2 >: A](that: NonEmptyList[A2]): NonEmptyList[A2] = NonEmptyList(that.head, that.tail ::: this.toList)
  def ++[A2 >: A](that: NonEmptyList[A2]): NonEmptyList[A2] = NonEmptyList(this.head, this.tail ::: that.toList)
  def ++[F[_], A2 >: A](that: F[A2])(using seqOps: SeqRead[F]): NonEmptyList[A2] = NonEmptyList.unsafeConcat(this.iterator, seqOps.newIterator(that))
  def :++[A2 >: A](that: NonEmptyList[A2]): NonEmptyList[A2] = NonEmptyList(this.head, this.tail ::: that.toList)
  def :++[F[_], A2 >: A](that: F[A2])(using seqOps: SeqRead[F]): NonEmptyList[A2] = NonEmptyList.unsafeConcat(this.iterator, seqOps.newIterator(that))
  def ++:[A2 >: A](that: NonEmptyList[A2]): NonEmptyList[A2] = NonEmptyList(that.head, that.tail ::: this.toList)
  def ++:[F[_], A2 >: A](that: F[A2])(using seqOps: SeqRead[F]): NonEmptyList[A2] = NonEmptyList.unsafeConcat(seqOps.newIterator(that), this.iterator)
  def prependedAll[F[_], B >: A](prefix: F[B])(using seqOps: SeqRead[F]): NonEmptyList[B] = NonEmptyList.unsafeConcat(seqOps.newIterator(prefix), this.iterator)
  def appendedAll[F[_], B >: A](suffix: F[B])(using seqOps: SeqRead[F]): NonEmptyList[B] = NonEmptyList.unsafeConcat(this.iterator, seqOps.newIterator(suffix))

  def iterator: Iterator[A] = toList.iterator

  def size: Int = toList.length
  def length: Int = toList.length

  def last: A = toList.last

  def forall(p: A => Boolean): Boolean = toList.forall(p)
  def exists(p: A => Boolean): Boolean = toList.exists(p)
  def contains[A1 >: A](elem: A1): Boolean = toList.contains(elem)

  def find(p: A => Boolean): Option[A] = toList.find(p)
  def findLast(p: A => Boolean): Option[A] = toList.findLast(p)

  def groupBy[K](f: A => K): Map[K, NonEmptyList[A]] = toList.groupByNE(f)
  def groupMap[K, B](key: A => K)(f: A => B): Map[K, NonEmptyList[B]] = toList.groupMapNE(key)(f)
  def groupMapReduce[K, B](key: A => K)(f: A => B)(reduce: (B, B) => B): Map[K, B] = toList.groupMapReduce(key)(f)(reduce)

  def partition(p: A => Boolean): Ior[NonEmptyList[A], NonEmptyList[A]] =
    this.toList.partition(p) match {
      case (lHead :: lTail, rHead :: rTail) => Ior.Both(NonEmptyList(lHead, lTail), NonEmptyList(rHead, rTail))
      case (lHead :: lTail, Nil)            => Ior.Left(NonEmptyList(lHead, lTail))
      case (Nil, rHead :: rTail)            => Ior.Right(NonEmptyList(rHead, rTail))
      case (Nil, Nil)                       => throw new RuntimeException("not possible...")
    }

  def partitionMap[T, F](p: A => Either[T, F]): Ior[NonEmptyList[T], NonEmptyList[F]] =
    this.toList.partitionMap(p) match {
      case (lHead :: lTail, rHead :: rTail) => Ior.Both(NonEmptyList(lHead, lTail), NonEmptyList(rHead, rTail))
      case (lHead :: lTail, Nil)            => Ior.Left(NonEmptyList(lHead, lTail))
      case (Nil, rHead :: rTail)            => Ior.Right(NonEmptyList(rHead, rTail))
      case (Nil, Nil)                       => throw new RuntimeException("not possible...")
    }

  def distinct: NonEmptyList[A] = unsafeTransformList(_.distinct)
  def distinctBy[B](f: A => B): NonEmptyList[A] = unsafeTransformList(_.distinctBy(f))

  def sorted[B >: A](implicit ord: Ordering[B]): NonEmptyList[A] = unsafeTransformList(_.sorted[B])
  def sortBy[B](f: A => B)(implicit ord: Ordering[B]): NonEmptyList[A] = unsafeTransformList(_.sortBy(f))
  def sortWith(lt: (A, A) => Boolean): NonEmptyList[A] = unsafeTransformList(_.sortWith(lt))

  def map[B](f: A => B): NonEmptyList[B] = NonEmptyList(f(head), tail.map(f))
  def flatMap[B](f: A => NonEmptyList[B]): NonEmptyList[B] = unsafeTransformList(_.flatMap(f(_).toList))
  def flatten[B](implicit ev: A <:< NonEmptyList[B]): NonEmptyList[B] = flatMap(ev(_))

  def reverse: NonEmptyList[A] = unsafeTransformList(_.reverse)

  def zip[B](that: NonEmptyList[B]): NonEmptyList[(A, B)] = unsafeTransformList(_.zip(that.toList))
  def zipWithIndex: NonEmptyList[(A, Int)] = unsafeTransformList(_.zipWithIndex)

  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1 = toList.fold(z)(op)
  def foldLeft[A1 >: A](z: A1)(op: (A1, A1) => A1): A1 = toList.foldLeft(z)(op)
  def foldRight[B](z: B)(op: (A, B) => B): B = toList.foldRight(z)(op)

  def reduce[B >: A](op: (B, B) => B): B = toList.reduce(op)
  def reduceLeft[B >: A](op: (B, A) => B): B = toList.reduceLeft(op)
  def reduceRight[B >: A](op: (A, B) => B): B = toList.reduceRight(op)

  def sum[B >: A](implicit num: Numeric[B]): B = toList.sum
  def product[B >: A](implicit num: Numeric[B]): B = toList.product

  def min[B >: A](implicit ord: Ordering[B]): A = toList.min
  def max[B >: A](implicit ord: Ordering[B]): A = toList.max
  def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A = toList.maxBy(f)
  def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A = toList.minBy(f)

  def toIndexedSeq: IndexedSeq[A] = toList.toIndexedSeq
  def toVector: Vector[A] = toList.toVector
  def toMap[K, V](implicit ev: A <:< (K, V)): Map[K, V] = toList.toMap
  def toSet[B >: A]: Set[B] = toList.toSet
  def toArray[B >: A: ClassTag]: Array[B] = toList.toArray

  def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = toList.collectFirst(pf)

  def mkString: String = toList.mkString
  def mkString(sep: String): String = toList.mkString(sep)
  def mkString(start: String, sep: String, end: String): String = toList.mkString(start, sep, end)

  // =====|  |=====

  override def apply(n: Int): A = toList.apply(n)

  override def isDefinedAt(x: Int): Boolean = toList.isDefinedAt(x)

  override def hashCode(): Int = toList.hashCode()

  override def toString: String = s"NonEmptyList(${toList.mkString(", ")})"

}
object NonEmptyList {

  // =====|  |=====

  inline def one[A](elem0: A): NonEmptyList[A] = NonEmptyList(elem0, Nil)
  inline def of[A](inline elem0: A, inline elemN: A*): NonEmptyList[A] = ${ ofImpl('elem0, 'elemN) }

  inline def fill[A](size: Int)(elem: A): NonEmptyList[A] = ${ fillImpl('size, 'elem) }
  def unsafeFill[A](size: Int)(elem: A): NonEmptyList[A] = NonEmptyList.unsafeFromList(List.fill(size)(elem))

  private def ofImpl[A: Type](elem0: Expr[A], elemN: Expr[Seq[A]])(using quotes: Quotes): Expr[NonEmptyList[A]] =
    elemN match {
      case Varargs(elemN: Seq[Expr[A]]) =>
        val nilExpr: Expr[List[A]] = '{ Nil }
        val listExpr: Expr[List[A]] = elemN.foldRight(nilExpr) { (elem, acc) => '{ ::(${ elem }, ${ acc }) } }
        '{ NonEmptyList(${ elem0 }, ${ listExpr }) }
      case _ =>
        '{ NonEmptyList(${ elem0 }, ${ elemN }.toList) }
    }

  private def fillImpl[A: Type](sizeExpr: Expr[Int], elemExpr: Expr[A])(using quotes: Quotes): Expr[NonEmptyList[A]] = {
    import quotes.reflect.*
    val size = sizeExpr.valueOrAbort
    if (size < 1) report.errorAndAbort("NonEmptyList.fill requires size >= 1")
    else if (size == 1) '{ NonEmptyList(${ elemExpr }, Nil) }
    else '{ NonEmptyList(${ elemExpr }, List.fill(${ Expr(size - 1) })(${ elemExpr })) }
  }

  inline def fromList[A](list: List[A]): Option[NonEmptyList[A]] = list match
    case head :: tail => NonEmptyList(head, tail).some
    case Nil          => None

  inline def unsafeFromList[A](list: List[A]): NonEmptyList[A] = list match
    case head :: tail => NonEmptyList(head, tail)
    case Nil          => throw new NoSuchElementException("Can not create an empty NonEmptyList")

  def unsafeNewBuilder[A]: mutable.Builder[A, NonEmptyList[A]] =
    List.newBuilder[A].mapResult(NonEmptyList.unsafeFromList)

  private def unsafeConcat[A](a: Iterator[A], b: Iterator[A]): NonEmptyList[A] = {
    val builder = unsafeNewBuilder[A]
    (a.knownSize, b.knownSize) match {
      case (-1, _)        => ()
      case (_, -1)        => ()
      case (aSize, bSize) => builder.sizeHint(aSize + bSize)
    }
    builder.addAll(a)
    builder.addAll(b)
    builder.result()
  }

  def unapply[A](list: List[A]): Option[NonEmptyList[A]] = NonEmptyList.fromList(list)
  def unapply[A](list: NonEmptyList[A]): Some[NonEmptyList[A]] = Some(list)

}
