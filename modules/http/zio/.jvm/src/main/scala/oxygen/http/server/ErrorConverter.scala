package oxygen.http.server

trait ErrorConverter[A, B] {
  def convert(error: A): Either[Throwable, B]
}
