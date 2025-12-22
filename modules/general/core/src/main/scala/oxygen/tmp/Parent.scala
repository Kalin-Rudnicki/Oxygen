package oxygen.tmp

import oxygen.tmp as PKG

sealed trait Parent[A] {
  def unwrapped: PKG.Case1[A] | PKG.Case2[A]
}
object Parent {

  trait Case1[A] extends Parent[A] { self: PKG.Case1[A] =>
    override final def unwrapped: PKG.Case1[A] = self
  }

  trait Case2[A] extends Parent[A] { self: PKG.Case2[A] =>
    override final def unwrapped: PKG.Case2[A] = self
  }

}
