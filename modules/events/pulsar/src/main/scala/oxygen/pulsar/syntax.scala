package oxygen.pulsar

import org.apache.pulsar.client.api.{ConsumerBuilder, MessageId, ProducerBuilder, ReaderBuilder, TypedMessageBuilder}
import oxygen.pulsar
import oxygen.pulsar.model.SubscriptionInitialPosition

private[pulsar] object syntax {

  extension [A](self: TypedMessageBuilder[A])
    def addHeaders(headers: Map[String, String]): TypedMessageBuilder[A] =
      if headers.isEmpty then self
      else headers.iterator.foldLeft(self) { case (self, (k, v)) => self.property(k, v) }

  extension [A](self: ProducerBuilder[A])
    def addHeaders(headers: Map[String, String]): ProducerBuilder[A] =
      if headers.isEmpty then self
      else headers.iterator.foldLeft(self) { case (self, (k, v)) => self.property(k, v) }

  extension [A](self: ConsumerBuilder[A])
    def addHeaders(headers: Map[String, String]): ConsumerBuilder[A] =
      if headers.isEmpty then self
      else headers.iterator.foldLeft(self) { case (self, (k, v)) => self.property(k, v) }

  extension [A](self: ReaderBuilder[A])
    def subscriptionInitialPosition(pos: SubscriptionInitialPosition): ReaderBuilder[A] = pos match
      case pulsar.model.SubscriptionInitialPosition.Earliest => self.startMessageIdInclusive().startMessageId(MessageId.earliest)
      case pulsar.model.SubscriptionInitialPosition.Latest   => self.startMessageId(MessageId.latest)

  extension [A](self: A)
    def configOpt[B](opt: Option[B])(f: B => A => A): A =
      opt match
        case Some(opt) => f(opt)(self)
        case None      => self

}
