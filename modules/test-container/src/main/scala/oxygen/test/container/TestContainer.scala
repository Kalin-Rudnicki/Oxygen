package oxygen.test.container

import java.util.UUID
import oxygen.predef.core.*
import zio.*

final case class TestContainer private (
    name: String,
    imageName: String,
    imageVersion: String,
    healthChecks: Growable[HealthCheck],
    params: Growable[TestContainer.Param],
) {

  def envVar(key: String, value: String): TestContainer = copy(params = this.params :+ TestContainer.Param.EnvVar(key, value))
  def envVar(key: String, value: Option[String]): TestContainer = value.fold(this)(this.envVar(key, _))

  def port(local: Int, container: Int): TestContainer = copy(params = this.params :+ TestContainer.Param.PortMapping(local, container))
  def port(local: Option[Int], container: Int): TestContainer = local.fold(this)(this.port(_, container))

  def label(key: String, value: String): TestContainer = copy(params = this.params :+ TestContainer.Param.Label(key, value))
  def label(key: String, value: Option[String]): TestContainer = value.fold(this)(this.label(key, _))

  def healthCheck(healthCheck: HealthCheck): TestContainer = copy(healthChecks = this.healthChecks :+ healthCheck)
  def healthCheck(name: String)(effect: Task[Boolean]): TestContainer = healthCheck(HealthCheck(name, effect))

}
object TestContainer {

  def make(name: String, imageName: String, imageVersion: String): TestContainer =
    new TestContainer(s"test-container--$name--${UUID.randomUUID}", imageName, imageVersion, Growable.empty, Growable.empty)

  enum Param {

    case EnvVar(key: String, value: String)
    case Label(key: String, value: String)
    case PortMapping(local: Int, container: Int)

    def toArgs: Growable[String] = this match
      case Param.EnvVar(key, value)            => Growable("-e", s"$key=$value")
      case Param.Label(key, value)             => Growable("-l", s"$key=$value")
      case Param.PortMapping(local, container) => Growable("-p", s"$local:$container")

  }

}
