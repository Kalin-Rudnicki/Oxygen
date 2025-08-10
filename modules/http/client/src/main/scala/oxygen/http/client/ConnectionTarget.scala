package oxygen.http.client

final case class ConnectionTarget(
    host: String,
    ssl: ConnectionTarget.SslConfig,
    port: Int,
)
object ConnectionTarget {

  enum SslConfig {
    case NoSsl
    case DefaultSsl
  }

}
