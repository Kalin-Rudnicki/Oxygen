package oxygen.socket

sealed trait Message
object Message {
  case object Close extends Message
  case object HeartBeat extends Message
  final case class Str(message: String) extends Message
}
