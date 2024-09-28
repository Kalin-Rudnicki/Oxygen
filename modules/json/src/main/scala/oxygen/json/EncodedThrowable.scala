package oxygen.json

import oxygen.json.instances.*
import oxygen.predef.core.*
import zio.json.JsonCodec

final case class EncodedThrowable(
    typeTag: TypeTag[?],
    message: Option[String],
    cause: Option[EncodedThrowable],
) extends Throwable derives JsonCodec {

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
object EncodedThrowable {

  def fromThrowable(throwable: Throwable): EncodedThrowable =
    throwable match {
      case throwable: EncodedThrowable => throwable
      case _ =>
        EncodedThrowable(
          TypeTag.fromClass(throwable.getClass),
          Option(throwable.getMessage),
          Option(throwable.getCause).map(fromThrowable),
        )
    }

}
