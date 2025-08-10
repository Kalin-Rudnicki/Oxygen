package oxygen.core

sealed trait Ior[+L, +R] {

  type SelfT[+L2, +R2] <: Ior[L2, R2]

  def rightMap[R2](rf: R => R2): SelfT[L, R2]
  def leftMap[L2](lf: L => L2): SelfT[L2, R]
  def bimap[L2, R2](lf: L => L2, rf: R => R2): SelfT[L2, R2]
  def map[R2](rf: R => R2): SelfT[L, R2]

}
object Ior {

  sealed trait Distinct[+L, +R] extends Ior[L, R] {

    override type SelfT[+L2, +R2] <: Ior.Distinct[L2, R2]

  }

  final case class Left[+L](left: L) extends Ior.Distinct[L, Nothing] {

    override type SelfT[+L2, +R2] = Left[L2]

    override def rightMap[R2](rf: Nothing => R2): Left[L] = this
    override def leftMap[L2](lf: L => L2): Left[L2] = Left(lf(left))
    override def bimap[L2, R2](lf: L => L2, rf: Nothing => R2): Left[L2] = Left(lf(left))
    override def map[R2](rf: Nothing => R2): Left[L] = this

  }

  final case class Right[+R](right: R) extends Ior.Distinct[Nothing, R] {

    override type SelfT[+L2, +R2] = Right[R2]

    override def rightMap[R2](rf: R => R2): Right[R2] = Right(rf(right))
    override def leftMap[L2](lf: Nothing => L2): Right[R] = this
    override def bimap[L2, R2](lf: Nothing => L2, rf: R => R2): Right[R2] = Right(rf(right))
    override def map[R2](rf: R => R2): Right[R2] = Right(rf(right))

  }

  final case class Both[+L, +R](left: L, right: R) extends Ior[L, R] {

    override type SelfT[+L2, +R2] = Both[L2, R2]

    override def rightMap[R2](rf: R => R2): Both[L, R2] = Both(left, rf(right))
    override def leftMap[L2](lf: L => L2): Both[L2, R] = Both(lf(left), right)
    override def bimap[L2, R2](lf: L => L2, rf: R => R2): Both[L2, R2] = Both(lf(left), rf(right))
    override def map[R2](rf: R => R2): Both[L, R2] = Both(left, rf(right))

  }

}
