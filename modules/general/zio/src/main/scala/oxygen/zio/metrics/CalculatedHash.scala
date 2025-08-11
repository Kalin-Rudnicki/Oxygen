package oxygen.zio.metrics

/**
  * This computes 3 hashes:
  * 1. hash(value)
  * 2. hash(value.toString)
  * 3. hash(s"$productPrefix(${value.toString})")
  *
  * If all of those are equal, this trait assumes that the underlying values are equal without fully comparing them.
  */
trait CalculatedHash { self: Product =>

  private val fieldSeq: Seq[(String, Any)] = productElementNames.zip(productIterator).toSeq

  private val valueToString: String = fieldSeq.map { case (k, v) => s"$k = $v" }.mkString(", ")
  private val myToString: String = s"$productPrefix($valueToString)"

  private val valueHash: Int = fieldSeq.hashCode()
  private val valueToStringHash = valueToString.hashCode
  private val myToStringHash = myToString.hashCode

  override final def hashCode(): Int = valueHash

  override final def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: CalculatedHash => this.valueHash == that.valueHash && this.valueToStringHash == that.valueToStringHash && this.myToStringHash == that.myToStringHash
    case _                    => false

  override final def toString: String = myToString

}
