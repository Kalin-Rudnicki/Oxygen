package oxygen.json

import oxygen.predef.core.*

final case class OrderedMap[K, +V](elements: ArraySeq[(K, V)]) {

  lazy val toMap: Map[K, V] = elements.toMap

  def update[V2 >: V](k: K, v: V2): OrderedMap[K, V2] =
    elements.indexWhere(_._1 == k) match
      case -1  => OrderedMap(elements :+ (k, v))
      case idx => OrderedMap(elements.updated(idx, (k, v)))

  def ++[V2 >: V](that: OrderedMap[K, V2]): OrderedMap[K, V2] =
    that.elements.foldLeft(this: OrderedMap[K, V2]) { case (acc, (k, v)) => acc.update(k, v) }

}
object OrderedMap {

  def empty[K, V]: OrderedMap[K, V] = OrderedMap(ArraySeq.empty[(K, V)])

  def from[K, V](iter: Iterable[(K, V)]): OrderedMap[K, V] =
    iter.foldLeft(OrderedMap.empty[K, V]) { case (acc, (k, v)) => acc.update(k, v) }

}
