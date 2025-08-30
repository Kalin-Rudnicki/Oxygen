package oxygen.core.typeclass

import oxygen.core.syntax.option.*

trait Lens[A, B] extends PartialLens[A, B] {

  def get(parent: A): B
  def replace(parent: A)(child: B): A
  def modify(parent: A)(child: B => B): A

  override def apply(parent: A): Lens.Applied[A, B] = Lens.Applied.Inst(this, parent)

  override final def getOption(parent: A): Option[B] = get(parent).some
  override final def replaceOption(parent: A)(child: B): Option[A] = replace(parent)(child).some
  override final def modifyOption(parent: A)(child: B => B): Option[A] = modify(parent)(child).some
  override final def flatModifyOption(parent: A)(child: B => Option[B]): Option[A] = child(get(parent)).map(replace(parent))

  final def >>>[C](that: Lens[B, C]): Lens[A, C] = Lens.AndThen(this, that)

}
object Lens {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def unsafe[A, B](underlying: PartialLens[A, B]): Lens[A, B] = UnsafePartial(underlying)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Applied[A, B] extends PartialLens.Applied[A, B] {

    override protected val lens: Lens[A, B]

    final def get(parent: A): B = lens.get(parent)
    final def replace(parent: A)(child: B): A = lens.replace(parent)(child)
    final def modify(parent: A)(child: B => B): A = lens.modify(parent)(child)

  }
  object Applied {
    final case class Inst[A, B](lens: Lens[A, B], parent: A) extends Applied[A, B]
  }

  final case class AndThen[A, B, C](a: Lens[A, B], b: Lens[B, C]) extends Lens[A, C] {
    override def get(parent: A): C = b.get(a.get(parent))
    override def replace(parent: A)(child: C): A = a.modify(parent)(b.replace(_)(child))
    override def modify(parent: A)(child: C => C): A = a.modify(parent)(b.modify(_)(child))
  }

  final case class UnsafePartial[A, B](underlying: PartialLens[A, B]) extends Lens[A, B] {
    override def get(parent: A): B =
      underlying.getOption(parent).getOrElse(throw new RuntimeException(s"Lens.UnsafePartial.get returned None for: $parent"))
    override def replace(parent: A)(child: B): A =
      underlying.replaceOption(parent)(child).getOrElse(throw new RuntimeException(s"Lens.UnsafePartial.replace returned None for: $parent"))
    override def modify(parent: A)(child: B => B): A =
      underlying.modifyOption(parent)(child).getOrElse(throw new RuntimeException(s"Lens.UnsafePartial.modify returned None for: $parent"))

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Macros
  //////////////////////////////////////////////////////////////////////////////////////////////////////

}
