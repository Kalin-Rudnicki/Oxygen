package oxygen.quoted.error

final case class UnknownCase(
    typeName: String,
    value: Any,
) extends Throwable {
  override def getMessage: String = s"Error while trying to pattern match on '$typeName' : value = $value"
  override def toString: String = getMessage
}
