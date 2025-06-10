package oxygen.quoted

import scala.quoted.*

trait Model {

  val quotes: Quotes
  val unwrap: Any

  def maybePos: Option[Position] = None

  override final def hashCode(): Int = unwrap.hashCode()

  override final def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: Model => this.unwrap == that.unwrap
    case _           => false

  override final def toString: String = unwrap.toString

}
