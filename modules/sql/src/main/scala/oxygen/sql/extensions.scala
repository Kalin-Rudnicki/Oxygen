package oxygen.sql

import zio.*

extension [R, E, A](self: ZIO[R & Database, E, A])
  def usingDb(db: Database): ZIO[R, E, A] =
    db.use { self }
