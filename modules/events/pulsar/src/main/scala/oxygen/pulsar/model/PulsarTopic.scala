package oxygen.pulsar.model

import org.apache.pulsar.client.api.MessageId
import oxygen.predef.core.*
import zio.*

final case class PulsarTopic(
    persistent: Boolean,
    tenant: String,
    namespace: String,
    topic: String,
    partition: Option[Int],
) {

  private def protocolPrefix: String = if persistent then "persistent://" else "non-persistent://"
  private def partitionSuffix: String = partition.fold("") { p => s"-partition-$p" }

  def topicUrl: String = s"$protocolPrefix$tenant/$namespace/$topic$partitionSuffix"

  def fullyQualified: String = s"$protocolPrefix$tenant/$namespace/$topic$partitionSuffix"

  def ignorePartition: PulsarTopic = copy(partition = None)

  def topicAndPartition: String = partition match
    case Some(partition) => s"$topic [partition #$partition]"
    case None            => topic

}
object PulsarTopic {

  given Ordering[PulsarTopic] =
    Ordering.by[PulsarTopic, String](_.tenant).orElseBy(_.namespace).orElseBy(_.topic).orElseBy(_.partition.getOrElse(0)).orElseBy(_.persistent)

  private object parseIsPersistent {
    def unapply(in: String): Option[Boolean] = in match
      case "persistent"     => true.some
      case "non-persistent" => false.some
      case _                => None
  }

  private object topicAndPartition {
    def unapply(in: String): Some[(String, Option[Int])] =
      in.split("-partition-").toList match {
        case base :: partNo :: Nil =>
          partNo.toIntOption match {
            case Some(partNo) => Some((base, partNo.some))
            case None         => Some((in, None))
          }
        case _ =>
          Some((in, None))
      }
  }

  private val topicReg = "^([a-z\\-]+)://([^/]+)/([^/]+)/([^/]+)$".r
  def parse(topicUrl: String): Option[PulsarTopic] = topicUrl match
    case topicReg(parseIsPersistent(persistent), tenant, namespace, topicAndPartition(topic, partition)) => PulsarTopic(persistent, tenant, namespace, topic, partition).some
    case _                                                                                               => None

  def parseTask(topicUrl: String): Task[PulsarTopic] = parse(topicUrl) match
    case Some(topic) => ZIO.succeed(topic)
    case None        => ZIO.fail(Error(s"Invalid topic url [url=$topicUrl]"))
  def parseTask(messageId: MessageId, topicUrl: String): Task[PulsarTopic] = parse(topicUrl) match
    case Some(topic) => ZIO.succeed(topic)
    case None        => ZIO.fail(Error(s"Invalid topic url [messageId=$messageId, url=$topicUrl]"))

}
