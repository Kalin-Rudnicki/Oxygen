package oxygen.pulsar.client

import java.util.concurrent.{CompletionException, ExecutionException}
import org.apache.pulsar.client.admin.PulsarAdmin as RawPulsarAdminClient
import org.apache.pulsar.client.admin.PulsarAdminException
import org.apache.pulsar.common.policies.data.TenantInfo
import oxygen.predef.core.*
import oxygen.pulsar.model.*
import oxygen.pulsar.syntax.*
import scala.jdk.CollectionConverters.*
import zio.*

final class PulsarAdminClient(client: RawPulsarAdminClient) { self =>

  // TODO (KR) : add more helpers

  object cluster {

    def list: Task[ArraySeq[String]] =
      for {
        _ <- ZIO.logTrace("Listing pulsar clusters")
        java <- ZIO.fromCompletableFuture { client.clusters().getClustersAsync }
        scala = ArraySeq.from(java.asScala)
      } yield scala

    def getSingle: Task[String] =
      list.flatMap {
        case clusters if clusters.length == 1 => ZIO.succeed { clusters(0) }
        case clusters                         => ZIO.fail(Error(s"Expected single cluster, but got: [${clusters.mkString(", ")}]"))
      }

  }

  object tenant {

    def list: Task[ArraySeq[PulsarTenant]] =
      for {
        _ <- ZIO.logTrace("Listing pulsar tenants")
        java <- ZIO.fromCompletableFuture { client.tenants().getTenantsAsync }
        scala = ArraySeq.from(java.asScala)
        wrapped = scala.map(PulsarTenant(_))
      } yield wrapped

    def create(cluster: String, tenant: PulsarTenant): Task[Unit] =
      ZIO.fromCompletableFuture { client.tenants().createTenantAsync(tenant.tenant, TenantInfo.builder().allowedClusters(java.util.Set.of(cluster)).build()) }.unit

    def create(tenant: PulsarTenant): Task[Unit] =
      ZIO.logInfo(s"Creating pulsar tenant [${tenant.tenant}]") *>
        self.cluster.getSingle.flatMap(self.tenant.create(_, tenant))

    def createIfDNE(tenant: PulsarTenant): Task[Unit] =
      list.flatMap {
        case tenants if tenants.contains(tenant) => ZIO.logInfo(s"Pulsar tenant already exists [${tenant.tenant}]")
        case _                                   => succeedIfAlreadyExists(s"Pulsar tenant already exists [${tenant.tenant}]")(create(tenant))
      }

  }

  object namespace {

    def list: Task[ArraySeq[PulsarNamespace]] =
      listFlat(self.tenant.list, self.namespace.list(_))

    def list(tenant: PulsarTenant): Task[ArraySeq[PulsarNamespace]] =
      for {
        _ <- ZIO.logTrace(s"Listing pulsar namespaces [tenant=${tenant.tenant}]")
        java <- ZIO.fromCompletableFuture { client.namespaces().getNamespacesAsync(tenant.tenant) }
        scala = ArraySeq.from(java.asScala)
        wrapped <- ZIO.foreach(scala)(PulsarNamespace.parseTask)
      } yield wrapped

    def create(namespace: PulsarNamespace): Task[Unit] =
      ZIO.logInfo(s"Creating pulsar namespace [${namespace.fullyQualified}]") *>
        ZIO.fromCompletableFuture { client.namespaces().createNamespaceAsync(namespace.fullyQualified) }.unit

    def createIfDNE(namespace: PulsarNamespace): Task[Unit] = {
      val tenant = PulsarTenant(namespace.tenant)
      self.tenant.createIfDNE(tenant) *>
        list(tenant).flatMap {
          case namespaces if namespaces.contains(namespace) => ZIO.logInfo(s"Pulsar namespace already exists [${namespace.fullyQualified}]")
          case _                                            => succeedIfAlreadyExists(s"Pulsar namespace already exists [${namespace.fullyQualified}]")(create(namespace))
        }
    }

  }

  object topic {

    def list: Task[ArraySeq[PulsarTopic]] =
      listFlat(self.namespace.list, self.topic.list(_))

    def list(tenant: PulsarTenant): Task[ArraySeq[PulsarTopic]] =
      listFlat(self.namespace.list(tenant), self.topic.list(_))

    def list(namespace: PulsarNamespace): Task[ArraySeq[PulsarTopic]] =
      for {
        _ <- ZIO.logTrace(s"Listing pulsar topics [namespace=${namespace.fullyQualified}]")
        java <- ZIO.fromCompletableFuture { client.topics().getListAsync(namespace.fullyQualified) }
        scala = ArraySeq.from(java.asScala)
        wrapped <- ZIO.foreach(scala)(PulsarTopic.parseTask)
      } yield wrapped

    def listIgnoringPartitions: Task[ArraySeq[PulsarTopic]] =
      list.map(_.map(_.ignorePartition).distinct)

    def listIgnoringPartitions(tenant: PulsarTenant): Task[ArraySeq[PulsarTopic]] =
      list(tenant).map(_.map(_.ignorePartition).distinct)

    def listIgnoringPartitions(namespace: PulsarNamespace): Task[ArraySeq[PulsarTopic]] =
      list(namespace).map(_.map(_.ignorePartition).distinct)

    def create(topic: PulsarTopic, partitions: Option[Int]): Task[Unit] = {
      val baseTopic = topic.ignorePartition
      partitions match {
        case Some(partitions) =>
          ZIO.logInfo(s"Creating partitioned pulsar topic [${topic.fullyQualified}] (partitions=$partitions)") *>
            ZIO.fromCompletableFuture { client.topics().createPartitionedTopicAsync(baseTopic.fullyQualified, partitions) }.unit
        case None =>
          ZIO.logInfo(s"Creating non-partitioned pulsar topic [${topic.fullyQualified}]") *>
            ZIO.fromCompletableFuture { client.topics().createNonPartitionedTopicAsync(baseTopic.fullyQualified) }.unit
      }
    }

    def createIfDNE(topic: PulsarTopic, partitions: Option[Int]): Task[Unit] = {
      val namespace = PulsarNamespace(topic.tenant, topic.namespace)
      val baseTopic = topic.ignorePartition
      for {
        _ <- self.namespace.createIfDNE(namespace)
        topics <- list(namespace)
        filtered = topics.filter(_.ignorePartition == baseTopic)
        numPartitions = filtered.flatMap(_.partition).length
        _ <-
          if filtered.isEmpty then succeedIfAlreadyExists(s"Pulsar topic already exists [${topic.fullyQualified}]")(create(topic, partitions))
          else if numPartitions != partitions.getOrElse(0) then ZIO.logWarning(s"Pulsar topic exists [${topic.fullyQualified}], but has mismatching partitions")
          else ZIO.logInfo(s"Pulsar topic already exists [${topic.fullyQualified}]")
      } yield ()
    }

  }

  /**
    * Runs a `create*` effect, treating an "already exists" failure as success.
    *
    * `createIfDNE` uses a check-then-act (list, then create) pattern which is not atomic: under
    * concurrent calls two fibers can both observe the resource as missing and both attempt to
    * create it, and the loser receives a 409 `PulsarAdminException` ("... already exists"). Since
    * the post-condition (the resource exists) still holds, this is treated as success.
    */
  private def succeedIfAlreadyExists(alreadyExistsMessage: String)(create: Task[Unit]): Task[Unit] =
    create.catchSome {
      case error if isAlreadyExists(error) => ZIO.logInfo(alreadyExistsMessage)
    }

  /**
    * Whether `error` represents an "already exists" / 409 conflict from a Pulsar `create*` call.
    *
    *   - A `PulsarAdminException` with HTTP status 409 is treated as already-exists.
    *   - `CompletionException` / `ExecutionException` (as surfaced by `ZIO.fromCompletableFuture`)
    *     are unwrapped and re-checked.
    *   - Otherwise fall back to a case-insensitive check for "already exists" in the message.
    */
  private def isAlreadyExists(error: Throwable): Boolean =
    error match {
      case e: PulsarAdminException if e.getStatusCode == 409 =>
        true
      case (_: CompletionException | _: ExecutionException) if error.getCause != null && (error.getCause ne error) =>
        isAlreadyExists(error.getCause)
      case _ =>
        Option(error.getMessage).exists(_.toLowerCase.contains("already exists"))
    }

  private def listFlat[A, B](a: Task[ArraySeq[A]], b: A => Task[ArraySeq[B]]): Task[ArraySeq[B]] =
    a.flatMap(ZIO.foreach(_)(b)).map(_.flatten)

}
object PulsarAdminClient {

  final case class Config(
      host: String,
      port: Int,
      tls: Option[Config.Tls],
      // TODO (KR) : auth
  ) {

    def protocolPrefix: String = if tls.nonEmpty then "https://" else "http://"

    def serviceUrl: String = s"$protocolPrefix$host:$port"

  }
  object Config {

    final case class Tls(
        trustCertsFilePath: Option[String],
        allowInsecureConnection: Option[Boolean],
    )

  }

  val layer: RLayer[PulsarAdminClient.Config, PulsarAdminClient] =
    ZLayer.scoped {
      for {
        config <- ZIO.service[PulsarAdminClient.Config]
        rawClient <- ZIO.attemptBlocking {
          RawPulsarAdminClient
            .builder()
            .serviceHttpUrl(config.serviceUrl)
            .configOpt(config.tls) { tls =>
              _
                .tlsTrustCertsFilePath(tls.trustCertsFilePath.orNull)
                .allowTlsInsecureConnection(tls.allowInsecureConnection.getOrElse(false))
            }
            .build()
        }
        _ <- ZIO.addFinalizer { ZIO.attemptBlocking { rawClient.close() }.orDie }
      } yield PulsarAdminClient(rawClient)
    }

}
