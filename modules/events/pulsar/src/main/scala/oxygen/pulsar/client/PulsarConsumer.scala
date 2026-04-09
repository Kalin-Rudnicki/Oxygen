package oxygen.pulsar.client

import oxygen.events.*
import oxygen.pulsar.model.*
import zio.*
import zio.stream.*

final class PulsarConsumer[K, V](
    client: PulsarClient,
    config: PulsarConsumer.Config,
    codec: EventCodec[K, V],
) extends EventConsumer[PulsarConsumerEvent[K, V]] {

  override def stream: EventStream[Any, Throwable, PulsarConsumerEvent[K, V]] =
    EventStream(
      "Pulsar Consumer",
      true,
      ZStream.scoped { client.rawConsumer(config) }.flatMap { consumer =>
        ZStream
          .repeatZIOChunk {
            ZIO
              .fromCompletableFuture { consumer.batchReceiveAsync() }
              .map { messages => Chunk.fromJavaIterable(messages) }
          }
          .mapZIOChunked { msg =>
            ZIO.logTrace(s"Pulsar consumer received message [messageId=${msg.getMessageId}, topic=${msg.getTopicName}]") *>
              PulsarConsumerEvent.parseMessage(msg)(using codec).map { msg2 =>
                Committable(
                  msg2,
                  ZIO.fromCompletableFuture { consumer.acknowledgeAsync(msg.getMessageId) }.orDie.unit,
                )
              }
          }
      },
    )

}
object PulsarConsumer {

  final case class Config(
      topic: PulsarTopic,
      subscriptionName: String,
      subscriptionType: ConsumerSubscriptionType,
      consumerName: Option[String],
      receiverQueueSize: Int,
      maxTotalReceiverQueueSizeAcrossPartitions: Option[Int],
      ackTimeout: Option[Duration],
      negativeAckRedeliveryDelay: Option[Duration],
      subscriptionInitialPosition: SubscriptionInitialPosition,
      replicateSubscriptionState: Boolean,
      // batchReceivePolicy: Option[BatchReceivePolicy],
      properties: Map[String, String],
  )

  def layer[K: Tag, V: Tag](using codec: EventCodec[K, V]): RLayer[PulsarClient & PulsarConsumer.Config, PulsarConsumer[K, V]] =
    ZLayer {
      for {
        client <- ZIO.service[PulsarClient]
        config <- ZIO.service[PulsarConsumer.Config]
      } yield PulsarConsumer(client, config, codec)
    }

  def stringLayer[K: Tag, V: Tag](using codec: EventCodec[K, V]): RLayer[PulsarClient & PulsarConsumer.Config, EventConsumer[Event[String, K, V]]] =
    PulsarConsumer.layer[K, V].project(_.map(Event.StringKey(_)))

}
