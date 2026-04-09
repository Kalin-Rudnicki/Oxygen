package oxygen.pulsar.model

import oxygen.predef.core.*
import zio.*

final case class PulsarNamespace(
    tenant: String,
    namespace: String,
) {

  def fullyQualified: String = s"$tenant/$namespace"

}
object PulsarNamespace {

  given Ordering[PulsarNamespace] =
    Ordering.by[PulsarNamespace, String](_.tenant).orElseBy(_.namespace)

  private val namespaceReg = "^([^/]+)/([^/]+)$".r
  def parse(fqNamespace: String): Option[PulsarNamespace] = fqNamespace match
    case namespaceReg(namespace, topic) => PulsarNamespace(namespace, topic).some
    case _                              => None

  def parseTask(fqNamespace: String): Task[PulsarNamespace] = parse(fqNamespace) match
    case Some(namespace) => ZIO.succeed(namespace)
    case None            => ZIO.fail(Error(s"Invalid topic url [fqNamespace=$fqNamespace]"))

}
