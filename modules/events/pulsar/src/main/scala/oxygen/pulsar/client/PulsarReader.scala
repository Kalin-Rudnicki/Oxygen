package oxygen.pulsar.client

import oxygen.events.*
import oxygen.pulsar.model.*
import zio.*
import zio.stream.*

final class PulsarReader[K, V](
    client: PulsarClient,
    config: PulsarReader.Config,
    codec: EventCodec[K, V],
) extends EventConsumer[PulsarConsumerEvent[K, V]] {

  override def stream: EventStream[Any, Throwable, PulsarConsumerEvent[K, V]] =
    EventStream(
      "Pulsar Reader",
      true,
      ZStream.scoped { client.rawReader(config) }.flatMap { reader =>
        ZStream
          .repeatZIO {
            ZIO.fromCompletableFuture { reader.readNextAsync() }
          }
          .buffer(128)
          .mapZIOChunked { msg =>
            ZIO.logTrace(s"Pulsar reader received message [messageId=${msg.getMessageId}, topic=${msg.getTopicName}]") *>
              PulsarConsumerEvent.parseMessage(msg)(using codec).map { msg2 =>
                Committable(
                  msg2,
                  ZIO.unit, // nothing to "commit" in a reader
                )
              }
          }
      },
    )

}
object PulsarReader {

  final case class Config(
      topic: PulsarTopic,
      subscriptionInitialPosition: SubscriptionInitialPosition,
      readerName: Option[String],
      receiverQueueSize: Int,
      readCompacted: Boolean,
      subscriptionRolePrefix: Option[String],
  )

  def layer[K: Tag, V: Tag](using codec: EventCodec[K, V]): RLayer[PulsarClient & PulsarReader.Config, PulsarReader[K, V]] =
    ZLayer {
      for {
        client <- ZIO.service[PulsarClient]
        config <- ZIO.service[PulsarReader.Config]
      } yield PulsarReader(client, config, codec)
    }

  def stringLayer[K: Tag, V: Tag](using codec: EventCodec[K, V]): RLayer[PulsarClient & PulsarReader.Config, EventConsumer[Event[String, K, V]]] =
    PulsarReader.layer[K, V].project(_.map(Event.StringKey(_)))

}
