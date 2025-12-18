package oxygen.storage

import oxygen.predef.core.*
import zio.*
import zio.stream.*

trait CRUDRepo[K, A] {

  // =====| Create |=====

  def insert(value: A): UIO[Unit]
  def insertAll[S[_]: SeqOps](values: S[A]): UIO[Unit]
  def insertAllStream[R, E](values: ZStream[R, E, A]): ZIO[R, E, Unit]

  final def insertAll(values: A*): UIO[Unit] = insertAll[Seq](values)

  // =====| Read |=====

  def selectAll[S[_]: SeqOps]: UIO[S[A]]
  def selectAllStream: UStream[A]

  def findByKey(key: K): UIO[Option[A]]
  def getByKeyOrDie(key: K): UIO[A]
  final def getByKeyOrFail[E](key: K, error: => E): IO[E, A] = findByKey(key).someOrFail(error)

  // =====| Update |=====

  // TODO (KR) : have a version which asserts exactly 1 row is updated
  def update(value: A): UIO[Unit]
  def updateAll[S[_]: SeqOps](values: S[A]): UIO[Unit]

  final def updateAll(values: A*): UIO[Unit] = updateAll[Seq](values)

  // =====| Delete |=====

  // TODO (KR) : have a version which asserts exactly 1 row is updated
  def delete(key: K): UIO[Unit]
  def deleteAll[S[_]: SeqOps](keys: S[K]): UIO[Unit]

  def truncate(using Unsafe): UIO[Unit]
  def truncateCascade(using Unsafe): UIO[Unit]

  final def deleteAll(keys: K*): UIO[Unit] = deleteAll[Seq](keys)

  // =====| Upsert |=====

  def upsert(value: A): UIO[Unit]
  def upsertAll[S[_]: SeqOps](values: S[A]): UIO[Unit]
  def upsertAllStream[R, E](values: ZStream[R, E, A]): ZIO[R, E, Unit]

  final def upsertAll(values: A*): UIO[Unit] = upsertAll[Seq](values)

}
