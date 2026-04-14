package oxygen.events.pulsar

import org.apache.pulsar.client.impl.{ProducerBuilderImpl, ProducerImpl, PulsarClientImpl}
import oxygen.events.*
import oxygen.events.pulsar.syntax.*
import oxygen.schema.{JsonSchema, PlainTextSchema}
import zio.*

final case class PulsarEventProducer[K, V] private (
    pulsarProducer: ProducerImpl[String],
    keySchema: PlainTextSchema[K],
    valueSchema: JsonSchema[V],
) extends EventProducer[Event[Unit, K, V]] {

  override def produce(value: Event[Unit, K, V]): UIO[Unit] =
    ZIO.fromCompletableFuture {
      pulsarProducer
        .newMessage()
        .key(keySchema.encode(value.key))
        .value(valueSchema.encode(value.value))
        .addHeaders(value.headers)
        .sendAsync()
    }.orDie.unit

}
object PulsarEventProducer {

  def layer[K: {Tag, PlainTextSchema as keySchema}, V: {Tag, JsonSchema as valueSchema}]: RLayer[PulsarSource, EventProducer[Event[Unit, K, V]]] =
    ???,

}
