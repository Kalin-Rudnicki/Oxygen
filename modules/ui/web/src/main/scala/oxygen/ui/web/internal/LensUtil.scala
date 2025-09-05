package oxygen.ui.web.internal

import monocle.Lens
import monocle.macros.GenLens
import oxygen.predef.core.*

object LensUtil {

  inline def genLens[A, B](inline f: A => B): Lens[A, B] =
    GenLens.apply[A].apply(f).asInstanceOf[Lens[A, B]]

  def arraySeq[A](idx: Int): Lens[ArraySeq[A], A] =
    Lens[ArraySeq[A], A](_(idx)) { value => seq => seq.updated(idx, value) }

}
