package oxygen.events

import java.nio.charset.StandardCharsets
import oxygen.predef.core.*
import oxygen.schema.{JsonSchema, PlainTextSchema}
import scala.annotation.nowarn

// TODO (KR) : should there be a PulsarEventCodec which can control the way the producer/consumer/reader is made?
//           : currently, every value looks like untyped bytes as far as pulsar is concerned.
//           : this wrapper is probably only needed if `oxygen-events-pulsar` decides to implement avro.
trait EventCodec[K, V] {

  def encodeKey(key: K): Array[Byte]
  def decodeKey(key: Array[Byte]): Either[String, K]

  def encodeValue(value: V): Array[Byte]
  def decodeValue(value: Array[Byte]): Either[String, V]

  ///////  ///////////////////////////////////////////////////////////////

  // Compiler being annoying and warning about unused typeTag, but it's needed for child transform.
  @nowarn def transformKey[K2: TypeTag](to: K => K2, from: K2 => K): EventCodec[K2, V] = EventCodec.TransformKey(this, to, from)
  @nowarn def transformOrFailKey[K2: TypeTag](to: K => Either[String, K2], from: K2 => K): EventCodec[K2, V] = EventCodec.TransformOrFailKey(this, to, from)
  @nowarn def transformValue[V2: TypeTag](to: V => V2, from: V2 => V): EventCodec[K, V2] = EventCodec.TransformValue(this, to, from)
  @nowarn def transformOrFailValue[V2: TypeTag](to: V => Either[String, V2], from: V2 => V): EventCodec[K, V2] = EventCodec.TransformOrFailValue(this, to, from)

}
object EventCodec {

  def plainKeyPlainSchema[K: PlainTextSchema as keySchema, V: PlainTextSchema as valueSchema]: EventCodec[K, V] = EventCodec.PlainKeyPlainSchema(keySchema, valueSchema)
  def plainKeyJsonSchema[K: PlainTextSchema as keySchema, V: JsonSchema as valueSchema]: EventCodec[K, V] = EventCodec.PlainKeyJsonSchema(keySchema, valueSchema)

  given givenPlainKeyJsonSchema: [K: PlainTextSchema as keySchema, V: JsonSchema as valueSchema] => EventCodec[K, V] = EventCodec.PlainKeyJsonSchema(keySchema, valueSchema)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Base
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class PlainKeyPlainSchema[K, V](keySchema: PlainTextSchema[K], valueSchema: PlainTextSchema[V]) extends EventCodec[K, V] {

    override def encodeKey(key: K): Array[Byte] = keySchema.encode(key).getBytes(StandardCharsets.UTF_8)
    override def decodeKey(key: Array[Byte]): Either[String, K] = keySchema.decode(new String(key, StandardCharsets.UTF_8))
    override def encodeValue(value: V): Array[Byte] = valueSchema.encode(value).getBytes(StandardCharsets.UTF_8)
    override def decodeValue(value: Array[Byte]): Either[String, V] = valueSchema.decode(new String(value, StandardCharsets.UTF_8))

    override def transformKey[K2: TypeTag](to: K => K2, from: K2 => K): EventCodec[K2, V] = PlainKeyPlainSchema(keySchema.transform(to, from), valueSchema)
    override def transformOrFailKey[K2: TypeTag](to: K => Either[String, K2], from: K2 => K): EventCodec[K2, V] = PlainKeyPlainSchema(keySchema.transformOrFail(to, from), valueSchema)
    override def transformValue[V2: TypeTag](to: V => V2, from: V2 => V): EventCodec[K, V2] = PlainKeyPlainSchema(keySchema, valueSchema.transform(to, from))
    override def transformOrFailValue[V2: TypeTag](to: V => Either[String, V2], from: V2 => V): EventCodec[K, V2] = PlainKeyPlainSchema(keySchema, valueSchema.transformOrFail(to, from))

  }

  final case class PlainKeyJsonSchema[K, V](keySchema: PlainTextSchema[K], valueSchema: JsonSchema[V]) extends EventCodec[K, V] {

    override def encodeKey(key: K): Array[Byte] = keySchema.encode(key).getBytes(StandardCharsets.UTF_8)
    override def decodeKey(key: Array[Byte]): Either[String, K] = keySchema.decode(new String(key, StandardCharsets.UTF_8))
    override def encodeValue(value: V): Array[Byte] = valueSchema.encode(value).getBytes(StandardCharsets.UTF_8)
    override def decodeValue(value: Array[Byte]): Either[String, V] = valueSchema.decode(new String(value, StandardCharsets.UTF_8))

    override def transformKey[K2: TypeTag](to: K => K2, from: K2 => K): EventCodec[K2, V] = PlainKeyJsonSchema(keySchema.transform(to, from), valueSchema)
    override def transformOrFailKey[K2: TypeTag](to: K => Either[String, K2], from: K2 => K): EventCodec[K2, V] = PlainKeyJsonSchema(keySchema.transformOrFail(to, from), valueSchema)
    override def transformValue[V2: TypeTag](to: V => V2, from: V2 => V): EventCodec[K, V2] = PlainKeyJsonSchema(keySchema, valueSchema.transform(to, from))
    override def transformOrFailValue[V2: TypeTag](to: V => Either[String, V2], from: V2 => V): EventCodec[K, V2] = PlainKeyJsonSchema(keySchema, valueSchema.transformOrFail(to, from))

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Transforms
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class TransformKey[K1, K2, V](underlying: EventCodec[K1, V], to: K1 => K2, from: K2 => K1) extends EventCodec[K2, V] {
    override def encodeKey(key: K2): Array[Byte] = underlying.encodeKey(from(key))
    override def decodeKey(key: Array[Byte]): Either[String, K2] = underlying.decodeKey(key).map(to)
    override def encodeValue(value: V): Array[Byte] = underlying.encodeValue(value)
    override def decodeValue(value: Array[Byte]): Either[String, V] = underlying.decodeValue(value)
  }
  final case class TransformOrFailKey[K1, K2, V](underlying: EventCodec[K1, V], to: K1 => Either[String, K2], from: K2 => K1) extends EventCodec[K2, V] {
    override def encodeKey(key: K2): Array[Byte] = underlying.encodeKey(from(key))
    override def decodeKey(key: Array[Byte]): Either[String, K2] = underlying.decodeKey(key).flatMap(to)
    override def encodeValue(value: V): Array[Byte] = underlying.encodeValue(value)
    override def decodeValue(value: Array[Byte]): Either[String, V] = underlying.decodeValue(value)
  }

  final case class TransformValue[K, V1, V2](underlying: EventCodec[K, V1], to: V1 => V2, from: V2 => V1) extends EventCodec[K, V2] {
    override def encodeKey(key: K): Array[Byte] = underlying.encodeKey(key)
    override def decodeKey(key: Array[Byte]): Either[String, K] = underlying.decodeKey(key)
    override def encodeValue(value: V2): Array[Byte] = underlying.encodeValue(from(value))
    override def decodeValue(value: Array[Byte]): Either[String, V2] = underlying.decodeValue(value).map(to)
  }
  final case class TransformOrFailValue[K, V1, V2](underlying: EventCodec[K, V1], to: V1 => Either[String, V2], from: V2 => V1) extends EventCodec[K, V2] {
    override def encodeKey(key: K): Array[Byte] = underlying.encodeKey(key)
    override def decodeKey(key: Array[Byte]): Either[String, K] = underlying.decodeKey(key)
    override def encodeValue(value: V2): Array[Byte] = underlying.encodeValue(from(value))
    override def decodeValue(value: Array[Byte]): Either[String, V2] = underlying.decodeValue(value).flatMap(to)
  }

}
