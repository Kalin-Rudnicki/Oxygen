package oxygen.pulsar.model

import org.apache.pulsar.client.api.{Message, MessageId}
import oxygen.events.{Event, EventCodec}
import oxygen.predef.core.*
import scala.jdk.CollectionConverters.*
import zio.*

final class PulsarConsumerEvent[+K, +V](
    val source: PulsarTopic,
    val key: K,
    val value: V,
    lazyHeaders: Lazy[Map[String, String]],
) extends Event[PulsarTopic, K, V] {
  override def headers: Map[String, String] = lazyHeaders.value
}
object PulsarConsumerEvent {

  def parseMessage[K, V](message: Message[Array[Byte]])(using codec: EventCodec[K, V]): Task[PulsarConsumerEvent[K, V]] = {
    val messageId: MessageId = message.getMessageId
    for {
      topic <- PulsarTopic.parseTask(message.getMessageId, message.getTopicName)
      key <- codec.decodeKey(message.getKeyBytes) match
        case Right(key)  => ZIO.succeed(key)
        case Left(error) => ZIO.fail(Error(s"Error decoding message key [messageId=$messageId]: $error"))
      value <- codec.decodeValue(message.getValue) match
        case Right(value) => ZIO.succeed(value)
        case Left(error)  => ZIO.fail(Error(s"Error decoding message value [messageId=$messageId]: $error"))
    } yield PulsarConsumerEvent(
      source = topic,
      key = key,
      value = value,
      lazyHeaders = Lazy { message.getProperties.asScala.toMap },
    )
  }

}
