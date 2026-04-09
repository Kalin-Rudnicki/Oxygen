package oxygen.pulsar.model

import org.apache.pulsar.client.api as P
import oxygen.predef.core.*

enum SubscriptionInitialPosition derives StrictEnum {

  case Earliest
  case Latest

  final def toPulsar: org.apache.pulsar.client.api.SubscriptionInitialPosition = this match
    case Earliest => P.SubscriptionInitialPosition.Earliest
    case Latest   => P.SubscriptionInitialPosition.Latest

}
