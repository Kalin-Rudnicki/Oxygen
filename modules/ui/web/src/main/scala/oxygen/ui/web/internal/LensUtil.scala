package oxygen.ui.web.internal

import monocle.{Lens, Setter}
import monocle.macros.GenLens
import oxygen.predef.core.*

object LensUtil {

  inline def genLens[A, B](inline f: A => B): Lens[A, B] =
    GenLens.apply[A].apply(f).asInstanceOf[Lens[A, B]]

  def arraySeq[A](idx: Int): Lens[ArraySeq[A], A] =
    Lens[ArraySeq[A], A](_(idx)) { value => seq => seq.updated(idx, value) }

  def setBoth[A, B](lens1: Setter[A, B], lens2: Setter[A, B]): Setter[A, B] =
    new Setter[A, B] {

      override def modify(f: B => B): A => A =
        a => lens2.modify(f)(lens1.modify(f)(a))

      override def replace(b: B): A => A =
        a => lens2.replace(b)(lens1.replace(b)(a))

    }

}
