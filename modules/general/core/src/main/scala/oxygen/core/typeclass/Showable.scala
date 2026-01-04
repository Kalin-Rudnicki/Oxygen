package oxygen.core.typeclass

import oxygen.core.LazyString

trait Showable {

  def show: LazyString

  override final def toString: String = show.toString

}
