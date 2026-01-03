package oxygen.core

// TODO (KR) : remove - replaced by new LazyString / Error
final case class ThrowableRepr(
    typeTag: TypeTag[?],
    message: Option[String],
    cause: Option[ThrowableRepr],
) extends Throwable {

  def toThrowable: Throwable = this

  def simpleMessage: String = message.getOrElse(typeTag.prefixAll)

  def simpleMessageWithCause: String = cause match
    case Some(cause) => s"$simpleMessage\n    Cause: ${cause.simpleMessageWithCause.replaceAll("\n", "\n    ")}"
    case None        => simpleMessage

  override def getMessage: String = message.orNull

  override def getCause: Throwable = cause.orNull

  override def toString: String = message match
    case Some(message) => s"${typeTag.prefixAll} : $message"
    case None          => typeTag.prefixAll

}
object ThrowableRepr {

  def fromThrowable(throwable: Throwable): ThrowableRepr =
    throwable match {
      case throwable: ThrowableRepr => throwable
      case _                        =>
        ThrowableRepr(
          TypeTag.fromClass(throwable.getClass),
          Option(throwable.getMessage),
          Option(throwable.getCause).map(fromThrowable),
        )
    }

}
