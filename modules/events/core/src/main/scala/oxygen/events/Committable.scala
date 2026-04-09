package oxygen.events

import zio.*

final case class Committable[+A](
    value: A,
    commit: UIO[Unit],
) {

  def as[B](v2: B): Committable[B] = Committable(v2, commit)

}
