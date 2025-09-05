package oxygen.http.server

import zio.*

trait ErrorLevel[E] {
  def level(error: E): LogLevel
}
object ErrorLevel {

  given default: [E] => ErrorLevel[E] =
    _ => LogLevel.Error

}
