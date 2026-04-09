package oxygen.pulsar.model

import org.apache.pulsar.client.api as P
import oxygen.core.typeclass.StrictEnum

enum ProducerAccessMode derives StrictEnum {

  /**
    * Default mode.
    * Multiple producers can publish to the same topic simultaneously.
    * No exclusivity enforced — fully shared access.
    *
    * Use when: You have many independent producers (normal pub-sub behavior).
    */
  case Shared

  /**
    * Exclusive access mode.
    * Only one producer is allowed at a time.
    *
    * If another producer tries to connect while one already exists:
    * → Producer creation fails immediately with an exception.
    *
    * Use when: You want strict single-producer semantics and fast failure
    * if a duplicate appears.
    */
  case Exclusive

  /**
    * Exclusive access with active fencing.
    * Only one producer is allowed.
    *
    * If a new producer connects:
    * → It immediately "fences" (invalidates / kicks out) any existing producer.
    *
    * This is stronger than plain Exclusive: the old producer gets an error
    * and can no longer write.
    *
    * Use when: Leader failover scenarios where the new leader must forcefully
    * take over (e.g. after detecting the old leader is dead).
    */
  case ExclusiveWithFencing

  /**
    * Waiting exclusive access.
    * Only one producer is allowed.
    *
    * If another producer is already connected:
    * → This producer creation is **pending** (it waits) until the current
    * exclusive producer disconnects.
    *
    * Once the old producer closes, this one is promoted to exclusive.
    *
    * Use when: You want clean hand-off / leader election without failing fast.
    * Common in HA setups where producers are expected to shut down gracefully.
    */
  case WaitForExclusive

  final def toPulsar: P.ProducerAccessMode = this match
    case ProducerAccessMode.Shared               => P.ProducerAccessMode.Shared
    case ProducerAccessMode.Exclusive            => P.ProducerAccessMode.Exclusive
    case ProducerAccessMode.ExclusiveWithFencing => P.ProducerAccessMode.ExclusiveWithFencing
    case ProducerAccessMode.WaitForExclusive     => P.ProducerAccessMode.WaitForExclusive

}
