package oxygen.core.typeclass

import oxygen.core.syntax.option.*
import scala.collection.immutable.ArraySeq
import scala.reflect.TypeTest

trait PartialLens[A, B] {

  def getOption(parent: A): Option[B]
  def replaceOption(parent: A)(child: B): Option[A]
  def modifyOption(parent: A)(child: B => B): Option[A]
  def flatModifyOption(parent: A)(child: B => Option[B]): Option[A]

  def apply(parent: A): PartialLens.Applied[A, B] = PartialLens.Applied.Inst(this, parent)

  final def >>>[C](that: PartialLens[B, C]): PartialLens[A, C] = (this, that) match
    case (self: Lens[A, B], that: Lens[B, C]) => Lens.AndThen(self, that)
    case _                                    => PartialLens.AndThen(this, that)

}
object PartialLens {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def subtype[A, B <: A](using typeTest: TypeTest[A, B]): PartialLens[A, B] = SubtypeLens(typeTest)

  def seqOf[F[_]: AtIndex as atIdx, A](idx: Int): PartialLens[F[A], A] = atIdx.make[A](idx)
  def seq[A](idx: Int): PartialLens[Seq[A], A] = SeqLens(idx)
  def arraySeq[A](idx: Int): PartialLens[ArraySeq[A], A] = ArraySeqLens(idx)
  def list[A](idx: Int): PartialLens[List[A], A] = ListLens(idx)
  def indexedSeq[A](idx: Int): PartialLens[IndexedSeq[A], A] = IndexedSeqLens(idx)
  def vector[A](idx: Int): PartialLens[Vector[A], A] = VectorLens(idx)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Applied[A, B] {

    protected val lens: PartialLens[A, B]
    protected val parent: A

    final def getOption: Option[B] = lens.getOption(parent)
    final def replaceOption(child: B): Option[A] = lens.replaceOption(parent)(child)
    final def modifyOption(child: B => B): Option[A] = lens.modifyOption(parent)(child)
    final def flatModifyOption(child: B => Option[B]): Option[A] = lens.flatModifyOption(parent)(child)

  }
  object Applied {
    final case class Inst[A, B](lens: PartialLens[A, B], parent: A) extends Applied[A, B]
  }

  final case class AndThen[A, B, C](a: PartialLens[A, B], b: PartialLens[B, C]) extends PartialLens[A, C] {
    override def getOption(parent: A): Option[C] = a.getOption(parent).flatMap(b.getOption)
    override def replaceOption(parent: A)(child: C): Option[A] = a.flatModifyOption(parent)(b.replaceOption(_)(child))
    override def modifyOption(parent: A)(child: C => C): Option[A] = a.flatModifyOption(parent)(b.modifyOption(_)(child))
    override def flatModifyOption(parent: A)(child: C => Option[C]): Option[A] = a.flatModifyOption(parent)(b.flatModifyOption(_)(child))
  }

  final case class SubtypeLens[A, B <: A](typeTest: TypeTest[A, B]) extends PartialLens[A, B] {
    override def getOption(parent: A): Option[B] = typeTest.unapply(parent)
    override def replaceOption(parent: A)(child: B): Option[A] = child.some
    override def modifyOption(parent: A)(child: B => B): Option[A] = typeTest.unapply(parent).map(child)
    override def flatModifyOption(parent: A)(child: B => Option[B]): Option[A] = typeTest.unapply(parent).flatMap(child)
  }

  /////// Seqs ///////////////////////////////////////////////////////////////

  final case class AtIndex[F[_]](make: [a] => Int => PartialLens[F[a], a])
  object AtIndex {
    given seq: AtIndex[Seq] = AtIndex { [a] => (idx: Int) => SeqLens(idx) }
    given arraySeq: AtIndex[ArraySeq] = AtIndex { [a] => (idx: Int) => ArraySeqLens(idx) }
    given list: AtIndex[List] = AtIndex { [a] => (idx: Int) => ListLens(idx) }
    given indexedSeq: AtIndex[IndexedSeq] = AtIndex { [a] => (idx: Int) => IndexedSeqLens(idx) }
    given vector: AtIndex[Vector] = AtIndex { [a] => (idx: Int) => VectorLens(idx) }
  }

  abstract class IterableLens[F[_], A](_get: F[A] => Option[A], _update: (F[A], A) => F[A], _updateOpt: (F[A], A) => Option[F[A]]) extends PartialLens[F[A], A] {
    override def getOption(parent: F[A]): Option[A] = _get(parent)
    override def replaceOption(parent: F[A])(child: A): Option[F[A]] = _updateOpt(parent, child)
    override def modifyOption(parent: F[A])(child: A => A): Option[F[A]] = _get(parent).map(b => _update(parent, child(b)))
    override def flatModifyOption(parent: F[A])(child: A => Option[A]): Option[F[A]] = _get(parent).flatMap(child(_).map(_update(parent, _)))
  }

  final case class SeqLens[A](idx: Int) extends IterableLens[Seq, A](_.lift(idx), _.updated(idx, _), (s, a) => Option.when(s.size > idx)(s.updated(idx, a)))
  final case class ArraySeqLens[A](idx: Int) extends IterableLens[ArraySeq, A](_.lift(idx), _.updated(idx, _), (s, a) => Option.when(s.size > idx)(s.updated(idx, a)))
  final case class ListLens[A](idx: Int) extends IterableLens[List, A](_.lift(idx), _.updated(idx, _), (s, a) => Option.when(s.size > idx)(s.updated(idx, a)))
  final case class IndexedSeqLens[A](idx: Int) extends IterableLens[IndexedSeq, A](_.lift(idx), _.updated(idx, _), (s, a) => Option.when(s.size > idx)(s.updated(idx, a)))
  final case class VectorLens[A](idx: Int) extends IterableLens[Vector, A](_.lift(idx), _.updated(idx, _), (s, a) => Option.when(s.size > idx)(s.updated(idx, a)))

}
