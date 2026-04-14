package oxygen.events.pulsar

import org.apache.pulsar.client.api.TypedMessageBuilder

object syntax {

  extension [A](self: TypedMessageBuilder[A])
    def addHeaders(headers: Map[String, String]): TypedMessageBuilder[A] =
      if headers.isEmpty then self
      else headers.iterator.foldLeft(self) { case (self, (k, v)) => self.property(k, v) }

}
