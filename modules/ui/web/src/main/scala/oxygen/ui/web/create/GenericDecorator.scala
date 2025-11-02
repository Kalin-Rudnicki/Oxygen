package oxygen.ui.web.create

import oxygen.predef.core.*

sealed trait GenericDecorator[A] {

  def show: String
  def accNames: Growable[String]
  final lazy val names: ArraySeq[String] = accNames.toArraySeq

  def decorate(in: A): A
  final def apply(in: A): A = decorate(in)

  final def >>(that: GenericDecorator[A]): GenericDecorator[A] = (this, that) match
    case (_: GenericDecorator.Empty[A], b: GenericDecorator.NonEmpty[A])    => b
    case (a: GenericDecorator.NonEmpty[A], b: GenericDecorator.NonEmpty[A]) => GenericDecorator.AndThen(a, b)
    case (a: GenericDecorator.NonEmpty[A], _: GenericDecorator.Empty[A])    => a
    case (a: GenericDecorator.Empty[A], _: GenericDecorator.Empty[A])       => a

  final def <<(that: GenericDecorator[A]): GenericDecorator[A] = that >> this

}
object GenericDecorator {

  def empty[A]: GenericDecorator[A] = new Empty[A]

  def apply[A](name: String)(mod: A => A): GenericDecorator[A] = Simple(name, mod)

  def all[S[_]: SeqRead, A](decorators: S[GenericDecorator[A]]): GenericDecorator[A] =
    decorators.newIterator.foldLeft(empty[A])(_ >> _)

  def all[A](decorators: GenericDecorator[A]*): GenericDecorator[A] =
    all(decorators)

  final class Empty[A] private[GenericDecorator] () extends GenericDecorator[A] {
    override val show: String = "empty"
    override val accNames: Growable[String] = Growable.empty
    override def decorate(in: A): A = in
  }

  sealed trait NonEmpty[A] extends GenericDecorator[A]

  final case class Simple[A] private[GenericDecorator] (name: String, mod: A => A) extends GenericDecorator.NonEmpty[A] {
    override val show: String = name
    override val accNames: Growable[String] = Growable.single(name)
    override def decorate(in: A): A = mod(in)
  }

  final case class AndThen[A] private[GenericDecorator] (a: GenericDecorator.NonEmpty[A], b: GenericDecorator.NonEmpty[A]) extends GenericDecorator.NonEmpty[A] {
    override lazy val show: String = s"${a.show} >> ${b.show}"
    override lazy val accNames: Growable[String] = a.accNames ++ b.accNames
    override def decorate(in: A): A = b.decorate(a.decorate(in))
  }

}
