package oxygen.core.collection

import oxygen.core.collection.Contiguous.typedAnyTag
import scala.annotation.unchecked.uncheckedVariance

/**
  * Not meant to be accessed at all.
  * Strictly for the purpose of building a [[Contiguous]].
  */
sealed trait Growable[+A] {

  val size: Int

  protected def write(offset: Int, array: Array[A @uncheckedVariance]): Unit

  def toContiguous: Contiguous[A]

  final def ++[B >: A](that: Growable[B]): Growable[B] = Growable.concat(this, that)

  final def :+[B >: A](that: B): Growable[B] = Growable.concat(this, Growable.Single(that))
  final def :++[B >: A](that: Growable[B]): Growable[B] = Growable.concat(this, that)
  final def :++[B >: A](that: Contiguous[B]): Growable[B] = Growable.concat(this, Growable.many(that))
  final def :++[B >: A](that: IterableOnce[B]): Growable[B] = Growable.concat(this, Growable.many(that))

  final def +:[B >: A](that: B): Growable[B] = Growable.concat(Growable.Single(that), this)
  final def ++:[B >: A](that: Growable[B]): Growable[B] = Growable.concat(that, this)
  final def ++:[B >: A](that: Contiguous[B]): Growable[B] = Growable.concat(Growable.many(that), this)
  final def ++:[B >: A](that: IterableOnce[B]): Growable[B] = Growable.concat(Growable.many(that), this)

}
object Growable {

  private case object Empty extends Growable[Nothing] {
    override val size: Int = 0
    override protected def write(offset: Int, array: Array[Nothing]): Unit =
      ()
    override def toContiguous: Contiguous[Nothing] =
      Contiguous.Empty
  }

  private final case class Single[+A](value: A) extends Growable[A] {
    override val size: Int = 1
    override protected def write(offset: Int, array: Array[A @uncheckedVariance]): Unit =
      array(offset) = value
    override def toContiguous: Contiguous[A] =
      Contiguous(value)
  }

  private final case class Many[+A](values: Contiguous[A]) extends Growable[A] {
    override val size: Int = values.length
    override protected def write(offset: Int, array: Array[A @uncheckedVariance]): Unit =
      System.arraycopy(values.getRawArray, 0, array, offset, values.length)
    override def toContiguous: Contiguous[A] =
      values
  }

  private final case class Concat[+A](a: Growable[A], b: Growable[A]) extends Growable[A] {
    override val size: Int = Math.addExact(a.size, b.size)
    override protected def write(offset: Int, array: Array[A @uncheckedVariance]): Unit = {
      a.write(offset, array)
      b.write(offset + a.size, array)
    }
    override def toContiguous: Contiguous[A] = {
      val array: Array[A] = new Array[A](size)
      write(0, array)
      new Contiguous[A](array)
    }
  }

  def empty[A]: Growable[A] = Empty
  def single[A](value: A): Growable[A] = Single(value)
  def many[A](values: IterableOnce[A]): Growable[A] = many(Contiguous.from(values))

  def many[A](values: Contiguous[A]): Growable[A] =
    values.length match {
      case 0 => Empty
      case 1 => Single(values(0))
      case _ => Many(values)
    }

  def concat[A](a: Growable[A], b: Growable[A]): Growable[A] =
    if (b eq Empty) a
    else if (a eq Empty) b
    else Concat(a, b)

}
