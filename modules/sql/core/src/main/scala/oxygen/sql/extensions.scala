package oxygen.sql

import zio.*
import zio.stream.*

extension [R, E, A](self: ZIO[R & Database, E, A])
  def usingDb(db: Database): ZIO[R, E, A] =
    db.use { self }

extension [R, E, A](self: ZStream[R & Database, E, A])
  def usingDb(db: Database): ZStream[R, E, A] =
    db.use { self }
