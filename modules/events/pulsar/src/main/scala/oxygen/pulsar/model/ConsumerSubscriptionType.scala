package oxygen.pulsar.model

import org.apache.pulsar.client.api as P
import oxygen.predef.core.*

enum ConsumerSubscriptionType derives StrictEnum {

  /**
    * **Exclusive** subscription.
    *
    * Only **one** consumer can be attached to this subscription at a time.
    * That consumer receives **all** messages in strict order.
    *
    * If another consumer tries to attach → it fails.
    *
    * Use when:
    * - You need strict total ordering.
    * - Single-threaded / single-consumer processing (e.g. stateful workflows).
    * - You want fast failure if a duplicate consumer appears.
    */
  case Exclusive

  /**
    * **Shared** subscription (also known as round-robin).
    *
    * Multiple consumers can attach to the same subscription.
    * Messages are distributed across consumers (roughly round-robin).
    *
    * **No ordering guarantee** across consumers.
    *
    * Use when:
    * - High throughput / horizontal scaling is needed.
    * - You have stateless consumers that can process messages in parallel.
    * - Classic competing consumers pattern.
    */
  case Shared

  /**
    * **Failover** subscription.
    *
    * Multiple consumers can attach, but only the **master** (highest priority)
    * consumer receives messages.
    *
    * If the master fails or disconnects, the next consumer is promoted automatically.
    *
    * Messages are delivered in strict order to the active master.
    *
    * Use when:
    * - You want high availability with automatic failover.
    * - You need ordering, but also redundancy (hot standby consumers).
    */
  case Failover

  /**
    * **Key_Shared** subscription.
    *
    * Multiple consumers can attach.
    * Messages with the **same key** are always delivered to the **same consumer**
    * and in the **order they were published** (per-key ordering).
    *
    * Different keys can go to different consumers → good parallelism.
    *
    * Use when:
    * - You need per-entity ordering (e.g. per user, per orderId, per device).
    * - You want both ordering **and** horizontal scalability.
    */
  case KeyShared

  final def toPulsar: P.SubscriptionType = this match
    case ConsumerSubscriptionType.Exclusive => P.SubscriptionType.Exclusive
    case ConsumerSubscriptionType.Shared    => P.SubscriptionType.Shared
    case ConsumerSubscriptionType.Failover  => P.SubscriptionType.Failover
    case ConsumerSubscriptionType.KeyShared => P.SubscriptionType.Key_Shared

}
