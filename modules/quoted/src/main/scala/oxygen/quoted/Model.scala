package oxygen.quoted

trait Model {

  val unwrap: Any

  override final def hashCode(): Int = unwrap.hashCode()

  override final def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: Model => this.unwrap == that.unwrap
    case _           => false

  override final def toString: String = unwrap.toString

}
