package oxygen.pulsar.client

import java.util.concurrent.TimeUnit
import org.apache.pulsar.client.api.{Consumer as RawPulsarConsumer, Producer as RawPulsarProducer, PulsarClient as RawPulsarClient, Reader as RawPulsarReader, SizeUnit}
import org.apache.pulsar.client.impl.ClientBuilderImpl
import oxygen.pulsar.syntax.*
import zio.*

final class PulsarClient(client: RawPulsarClient) {

  def rawProducer(config: PulsarProducer.Config): RIO[Scope, RawPulsarProducer[Array[Byte]]] =
    for {
      producer <- ZIO.fromCompletableFuture {
        client
          .newProducer()
          .topic(config.topic.topicUrl)
          .configOpt(config.producerName) { name => _.producerName(name) }
          .configOpt(config.sendTimeout) { timeout => _.sendTimeout(timeout.toSeconds.toInt, TimeUnit.SECONDS) }
          .configOpt(config.batchingMaxPublishDelay) { delay => _.batchingMaxPublishDelay(delay.toSeconds.toInt, TimeUnit.SECONDS) }
          .blockIfQueueFull(config.blockIfQueueFull)
          .maxPendingMessages(config.maxPendingMessages)
          .enableBatching(config.batchingEnabled)
          .accessMode(config.accessMode.toPulsar)
          .addHeaders(config.properties)
          .createAsync()
      }
      _ <- ZIO.addFinalizer { ZIO.fromCompletableFuture { producer.closeAsync() }.orDie }
    } yield producer

  def rawConsumer(config: PulsarConsumer.Config): RIO[Scope, RawPulsarConsumer[Array[Byte]]] =
    for {
      consumer <- ZIO.fromCompletableFuture {
        client
          .newConsumer()
          .topic(config.topic.topicUrl)
          .subscriptionName(config.subscriptionName)
          .subscriptionType(config.subscriptionType.toPulsar)
          .configOpt(config.consumerName) { name => _.consumerName(name) }
          .receiverQueueSize(config.receiverQueueSize)
          .configOpt(config.maxTotalReceiverQueueSizeAcrossPartitions) { pars => _.maxTotalReceiverQueueSizeAcrossPartitions(pars) }
          .configOpt(config.ackTimeout) { t => _.ackTimeout(t.toSeconds.toInt, TimeUnit.SECONDS) }
          .configOpt(config.negativeAckRedeliveryDelay) { d => _.negativeAckRedeliveryDelay(d.toSeconds.toInt, TimeUnit.SECONDS) }
          .subscriptionInitialPosition(config.subscriptionInitialPosition.toPulsar)
          .replicateSubscriptionState(config.replicateSubscriptionState)
          .addHeaders(config.properties)
          .subscribeAsync()
      }
      _ <- ZIO.addFinalizer { ZIO.fromCompletableFuture { consumer.closeAsync() }.orDie }
    } yield consumer

  def rawReader(config: PulsarReader.Config): RIO[Scope, RawPulsarReader[Array[Byte]]] =
    for {
      consumer <- ZIO.fromCompletableFuture {
        client
          .newReader()
          .topic(config.topic.topicUrl)
          .subscriptionInitialPosition(config.subscriptionInitialPosition)
          .configOpt(config.readerName) { name => _.readerName(name) }
          .receiverQueueSize(config.receiverQueueSize)
          .readCompacted(config.readCompacted)
          .configOpt(config.subscriptionRolePrefix) { prefix => _.subscriptionRolePrefix(prefix) }
          .createAsync()
      }
      _ <- ZIO.addFinalizer { ZIO.fromCompletableFuture { consumer.closeAsync() }.orDie }
    } yield consumer

}
object PulsarClient {

  final case class Config(
      host: String,
      port: Int,
      tls: Option[Config.Tls],
      ioThreads: Option[Int],
      listenerThreads: Option[Int],
      connectionsPerBroker: Option[Int],
      memoryLimit: Option[(Long, SizeUnit)],
      // TODO (KR) : auth
  ) {

    def protocolPrefix: String = if tls.nonEmpty then "pulsar+ssl://" else "pulsar://"

    def serviceUrl: String = s"$protocolPrefix$host:$port"

  }
  object Config {

    final case class Tls(
        trustCertsFilePath: Option[String],
        allowInsecureConnection: Option[Boolean],
    )

  }

  val layer: RLayer[PulsarClient.Config, PulsarClient] =
    ZLayer.scoped {
      for {
        config <- ZIO.service[PulsarClient.Config]
        rawClient <- ZIO.attemptBlocking {
          ClientBuilderImpl()
            .serviceUrl(config.serviceUrl)
            .configOpt(config.tls) { tls =>
              _
                .tlsTrustCertsFilePath(tls.trustCertsFilePath.orNull)
                .allowTlsInsecureConnection(tls.allowInsecureConnection.getOrElse(false))
            }
            .configOpt(config.ioThreads) { threads => _.ioThreads(threads) }
            .configOpt(config.listenerThreads) { threads => _.listenerThreads(threads) }
            .connectionsPerBroker(config.connectionsPerBroker.getOrElse(1))
            .configOpt(config.memoryLimit) { case (value, unit) => _.memoryLimit(value, unit) }
            .build()
        }
        _ <- ZIO.addFinalizer { ZIO.fromCompletableFuture { rawClient.closeAsync() }.orDie }
      } yield PulsarClient(rawClient)
    }

}
