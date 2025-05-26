package oxygen.core.collection

import oxygen.core.typeclass.SeqOps
import scala.annotation.targetName
import scala.collection.mutable

/**
  * For the purpose of building sequences.
  * Allows you to do as much mapping, flat-mapping, and concatenation as you would like,
  * without having to re-build the sequences every time.
  * @see [[Growable.to]]
  */
sealed trait Growable[+A] {

  val knownSize: Int

  def foreach(f: A => Unit): Unit

  final def toContiguous: Contiguous[A] = this.to[Contiguous]

  final def ++[B >: A](that: Growable[B]): Growable[B] = Growable.Concat(this, that)

  final def :+[B >: A](that: B): Growable[B] = Growable.Concat(this, Growable.Single(that))
  final def :++[B >: A](that: Growable[B]): Growable[B] = Growable.Concat(this, that)

  final def +:[B >: A](that: B): Growable[B] = Growable.Concat(Growable.Single(that), this)
  final def ++:[B >: A](that: Growable[B]): Growable[B] = Growable.Concat(that, this)

  final def map[B](f: A => B): Growable[B] = Growable.Map(this, f)
  final def flatMap[B](f: A => Growable[B]): Growable[B] = Growable.FlatMap(this, f)
  final def filter(f: A => Boolean): Growable[A] = Growable.Filter(this, f)
  final def filterNot(f: A => Boolean): Growable[A] = filter(a => !f(a))

  final def flatten[B](using ev: A <:< Growable[B]): Growable[B] = this.flatMap(ev(_))

  @targetName("flatMapOption")
  final def flatMap[B](f: A => Option[B]): Growable[B] = Growable.Collect(this, f)
  final def collect[B](f: PartialFunction[A, B]): Growable[B] = Growable.Collect(this, f.lift)

  final def distinct: Growable[A] = Growable.DistinctBy(this, identity)
  final def distinctBy[B](f: A => B): Growable[A] = Growable.DistinctBy(this, f)

}
object Growable {

  case object Empty extends Growable[Nothing] {
    override val knownSize: Int = 0
    override def foreach(f: Nothing => Unit): Unit =
      ()
  }

  final case class Single[A](value: A) extends Growable[A] {
    override val knownSize: Int = 1
    override def foreach(f: A => Unit): Unit =
      f(value)
  }

  final case class Many[S[_], A](values: S[A], seqOps: SeqOps[S]) extends Growable[A] {
    override val knownSize: Int = seqOps.knownSize(values)
    override def foreach(f: A => Unit): Unit =
      seqOps.newIterator(values).foreach(f)
  }

  final case class Concat[A](a: Growable[A], b: Growable[A]) extends Growable[A] {
    override val knownSize: Int = -1
    override def foreach(f: A => Unit): Unit = {
      a.foreach(f)
      b.foreach(f)
    }
  }

  final case class Map[A, B](a: Growable[A], ab: A => B) extends Growable[B] {
    override val knownSize: Int = a.knownSize
    override def foreach(f: B => Unit): Unit =
      a.foreach { a => f(ab(a)) }
  }

  final case class FlatMap[A, B](a: Growable[A], ab: A => Growable[B]) extends Growable[B] {
    override val knownSize: Int = -1
    override def foreach(f: B => Unit): Unit =
      a.foreach { ab(_).foreach(f) }
  }

  final case class Filter[A](a: Growable[A], filter: A => Boolean) extends Growable[A] {
    override val knownSize: Int = -1
    override def foreach(f: A => Unit): Unit =
      a.foreach { a => if (filter(a)) f(a) }
  }

  final case class Collect[A, B](a: Growable[A], ab: A => Option[B]) extends Growable[B] {
    override val knownSize: Int = -1
    override def foreach(f: B => Unit): Unit =
      a.foreach { ab(_).foreach(f) }
  }

  final case class Fill[A](size: Int, fill: Int => A) extends Growable[A] {
    override val knownSize: Int = size
    override def foreach(f: A => Unit): Unit = {
      var c: Int = 0
      while (c < size) {
        f(fill(c))
        c += 1
      }
    }
  }

  final case class DistinctBy[A, B](a: Growable[A], ab: A => B) extends Growable[A] {
    override val knownSize: Int = -1
    override def foreach(f: A => Unit): Unit = {
      val seen: mutable.Set[B] = mutable.Set()
      a.foreach { a =>
        if (seen.add(ab(a)))
          f(a)
      }
    }

  }

  extension [A](self: Growable[A])
    def to[S[_]](using seqOps: SeqOps[S]): S[A] = {
      val builder = seqOps.newBuilder[A]
      builder.sizeHint(self.knownSize)
      self.foreach(builder.addOne)
      builder.result()
    }

  def empty[A]: Growable[A] = Empty
  def single[A](value: A): Growable[A] = Single(value)
  def many[S[_], A](values: S[A])(using seqOps: SeqOps[S]): Growable[A] = Many(values, seqOps)
  def apply[A](values: A*): Growable[A] = Growable.many(values)

  def option[A](value: Option[A]): Growable[A] = value match
    case Some(value) => Single(value)
    case None        => Empty

  def fill[A](n: Int)(elem: => A): Growable[A] = Growable.Fill(n, _ => elem)
  def fillWithIndex[A](n: Int)(elem: Int => A): Growable[A] = Growable.Fill(n, elem)

}
