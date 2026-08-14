package oxygen.pulsar.client

import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.util.concurrent.{CompletableFuture, CompletionException}
import org.apache.pulsar.client.admin.{Namespaces, PulsarAdmin, PulsarAdminException, Tenants, Topics}
import oxygen.predef.test.*
import oxygen.pulsar.model.*
import scala.jdk.CollectionConverters.*

/**
  * Unit tests for [[PulsarAdminClient]] idempotency (OXY-146).
  *
  * `RawPulsarAdminClient` (`org.apache.pulsar.client.admin.PulsarAdmin`) is a large Java interface,
  * so it is faked with a `java.lang.reflect.Proxy` that dispatches by method name. The tests drive
  * the public `topic.createIfDNE`, arranging for the tenant + namespace to already exist and the
  * topic to be missing so that a `create` is always attempted, then vary the failure the fake
  * `create` returns.
  */
object PulsarAdminClientSpec extends OxygenSpecDefault {

  private val tenant: PulsarTenant = PulsarTenant("test-tenant")
  private val namespace: PulsarNamespace = PulsarNamespace("test-tenant", "test-ns")
  private val topic: PulsarTopic = PulsarTopic(persistent = true, "test-tenant", "test-ns", "test-topic", None)

  private def proxy[A](interface: Class[A])(handle: PartialFunction[(String, List[AnyRef]), AnyRef]): A = {
    val invocationHandler: InvocationHandler =
      (proxyInstance: Any, method: Method, args: Array[AnyRef]) => {
        val argList: List[AnyRef] = Option(args).fold(List.empty[AnyRef])(_.toList)
        method.getName match {
          case "toString" => s"${interface.getSimpleName}$$mock"
          case "hashCode" => Int.box(java.lang.System.identityHashCode(proxyInstance))
          case "equals"   => Boolean.box(proxyInstance.asInstanceOf[AnyRef] eq argList.headOption.orNull)
          case name =>
            handle.lift((name, argList)).getOrElse(throw new UnsupportedOperationException(s"${interface.getSimpleName}.$name"))
        }
      }
    interface.cast(Proxy.newProxyInstance(interface.getClassLoader, Array[Class[?]](interface), invocationHandler))
  }

  private def completed[A](value: A): CompletableFuture[A] = CompletableFuture.completedFuture(value)

  private def failed[A](error: Throwable): CompletableFuture[A] = {
    val cf = new CompletableFuture[A]()
    cf.completeExceptionally(error)
    cf
  }

  private def conflict409: PulsarAdminException =
    new PulsarAdminException.ConflictException(new RuntimeException("Topic already exists"), "Conflict", 409)

  /**
    * Builds a fake admin client where the tenant + namespace already exist. `topicList` controls
    * what the topic listing returns, and `onCreate` supplies the result of the (non-partitioned)
    * `create` call.
    */
  private def mockAdmin(
      topicList: List[String],
      onCreate: () => CompletableFuture[Void],
  ): PulsarAdmin = {
    val topics: Topics = proxy(classOf[Topics]) {
      case ("getListAsync", _)                    => completed(topicList.asJava)
      case ("createNonPartitionedTopicAsync", _)  => onCreate()
      case ("createPartitionedTopicAsync", _)     => onCreate()
    }
    val namespaces: Namespaces = proxy(classOf[Namespaces]) { case ("getNamespacesAsync", _) =>
      completed(List(namespace.fullyQualified).asJava)
    }
    val tenants: Tenants = proxy(classOf[Tenants]) { case ("getTenantsAsync", _) =>
      completed(List(tenant.tenant).asJava)
    }
    proxy(classOf[PulsarAdmin]) {
      case ("topics", _)     => topics
      case ("namespaces", _) => namespaces
      case ("tenants", _)    => tenants
    }
  }

  private def clientCreatingWith(onCreate: () => CompletableFuture[Void]): PulsarAdminClient =
    PulsarAdminClient(mockAdmin(Nil, onCreate))

  override def testSpec: TestSpec =
    suite("PulsarAdminClientSpec")(
      suite("topic.createIfDNE - idempotency")(
        test("succeeds when create returns a 409 PulsarAdminException (concurrent create race)") {
          val client = clientCreatingWith(() => failed(conflict409))
          for {
            exit <- client.topic.createIfDNE(topic, None).exit
          } yield assert(exit)(succeeds(anything))
        },
        test("succeeds when the 409 is wrapped in a CompletionException") {
          val client = clientCreatingWith(() => failed(new CompletionException(conflict409)))
          for {
            exit <- client.topic.createIfDNE(topic, None).exit
          } yield assert(exit)(succeeds(anything))
        },
        test("succeeds via case-insensitive message fallback when status is not 409") {
          val client = clientCreatingWith(() => failed(new PulsarAdminException("Topic already EXISTS")))
          for {
            exit <- client.topic.createIfDNE(topic, None).exit
          } yield assert(exit)(succeeds(anything))
        },
      ),
      suite("topic.createIfDNE - non-already-exists errors propagate")(
        test("fails when create returns an unrelated PulsarAdminException") {
          val client = clientCreatingWith(() => failed(new PulsarAdminException.NotAuthorizedException(new RuntimeException("nope"), "Forbidden", 403)))
          for {
            exit <- client.topic.createIfDNE(topic, None).exit
          } yield assert(exit)(fails(anything))
        },
        test("fails when create returns an unrelated RuntimeException") {
          val client = clientCreatingWith(() => failed(new RuntimeException("kaboom")))
          for {
            exit <- client.topic.createIfDNE(topic, None).exit
          } yield assert(exit)(fails(anything))
        },
      ),
      suite("topic.createIfDNE - preserved behavior")(
        test("logs a warning and succeeds (without creating) on partition mismatch") {
          val admin = mockAdmin(
            topicList = List(topic.ignorePartition.fullyQualified),
            onCreate = () => failed(new AssertionError("create must not be called when the topic already exists")),
          )
          for {
            exit <- PulsarAdminClient(admin).topic.createIfDNE(topic, Some(3)).exit
          } yield assert(exit)(succeeds(anything))
        },
      ),
    )

}
