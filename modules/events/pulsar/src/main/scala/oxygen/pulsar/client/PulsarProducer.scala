package oxygen.pulsar.client

import org.apache.pulsar.client.api.{MessageId, Producer as RawPulsarProducer}
import oxygen.events.*
import oxygen.pulsar.model.*
import oxygen.pulsar.syntax.*
import zio.*

final class PulsarProducer[K, V](
    producer: RawPulsarProducer[Array[Byte]],
    codec: EventCodec[K, V],
) extends EventProducer[Event[Unit, K, V]] {

  def send(
      key: K,
      value: V,
      headers: Map[String, String],
  ): Task[MessageId] =
    ZIO.fromCompletableFuture {
      producer
        .newMessage()
        .keyBytes(codec.encodeKey(key))
        // TODO (KR) : support separate ordering key? if so, will need to add logic to any place that does key partitioning.
        .value(codec.encodeValue(value))
        .addHeaders(headers)
        .sendAsync()
    }.tap { messageId =>
      ZIO.logTrace(s"Pulsar producer published message [messageId=$messageId, topic=${producer.getTopic}]")
    }

  override def produce(value: Event[Unit, K, V]): UIO[Unit] =
    send(value.key, value.value, value.headers).orDie.unit

}
object PulsarProducer {

  final case class Config(
      topic: PulsarTopic,
      producerName: Option[String],
      sendTimeout: Option[Duration],
      blockIfQueueFull: Boolean,
      maxPendingMessages: Int,
      batchingEnabled: Boolean,
      batchingMaxPublishDelay: Option[Duration],
      batchingMaxBytes: Option[Int],
      accessMode: ProducerAccessMode,
      properties: Map[String, String],
  )

  def layer[K: Tag, V: Tag](using codec: EventCodec[K, V]): RLayer[PulsarClient & PulsarProducer.Config, PulsarProducer[K, V]] =
    ZLayer.scoped {
      for {
        client <- ZIO.service[PulsarClient]
        config <- ZIO.service[PulsarProducer.Config]
        producer <- client.rawProducer(config)
      } yield PulsarProducer(producer, codec)
    }

}
