package oxygen.storage.inMemory

import oxygen.predef.core.*
import oxygen.storage.CRUDRepo
import oxygen.zio.instances.chunkSeqOps
import zio.*
import zio.stream.*

final class KeyedTable[A, K] private (
    tableName: String,
    keyFromValue: A => K,
    state: Ref[Map[K, A]],
) {

  extension (self: A) private def toKey: K = keyFromValue(self)

  val crud: CRUDRepo[A, K] = new CRUDRepo[A, K] {

    // =====| Create |=====

    final def insert(value: A): UIO[Unit] = {
      val k = value.toKey
      ZIO.die(ConstraintViolation(tableName, "primary_key", k)).whenZIODiscard(state.get.map(_.contains(k))) *>

    }

    final def insertAll[S[_]: SeqOps as seq](values: S[A]): UIO[Unit] =
      state.get.flatMap { initial =>
        ZIO.foreachDiscard(seq.toIterable(values))(insert).tapErrorCause { _ => state.set(initial) }
      }

    final def insertAllStream[R, E](values: ZStream[R, E, A]): ZIO[R, E, Unit] =
      values.rechunk(10_000).runForeachChunk(insertAll(_))

    // =====| Read |=====

    final def selectAll[S[_]: SeqOps]: UIO[S[A]] =
      state.get.map(_.values.transformTo[S])

    final def selectAllStream: UStream[A] =
      ZStream.fromZIO(selectAll[Chunk]).flatMap(ZStream.fromChunk(_))

    final def findByKey(key: K): UIO[Option[A]] =
      state.get.map(_.get(key))

    final def getByKeyOrDie(key: K): UIO[A] =
      ??? // FIX-PRE-MERGE (KR) :

    // =====| Update |=====

    final def update(value: A): UIO[Unit] =
      state.update(_.updatedWith(value.toKey)(_.map(_ => value)))

    final def updateAll[S[_]: SeqOps as seq](values: S[A]): UIO[Unit] =
      state.get.flatMap { initial =>
        ZIO.foreachDiscard(seq.toIterable(values))(update).tapErrorCause { _ => state.set(initial) }
      }

    // =====| Delete |=====

    final def delete(key: K): UIO[Unit] =
      state.update(_.removed(key))

    final def deleteAll[S[_]: SeqOps as seq](keys: S[K]): UIO[Unit] =
      state.update(_.removedAll(seq.toIterable(keys)))

    final def truncate(using Unsafe): UIO[Unit] =
      state.set(Map.empty)

    // what can be done here?
    final def truncateCascade(using Unsafe): UIO[Unit] =
      state.set(Map.empty)

    // =====| Upsert |=====

    final def upsert(value: A): UIO[Unit] =
      state.update(_.updated(value.toKey, value))

    final def upsertAll[S[_]: SeqOps as seq](values: S[A]): UIO[Unit] =
      ZIO.foreachDiscard(seq.toIterable(values))(upsert)

    final def upsertAllStream[R, E](values: ZStream[R, E, A]): ZIO[R, E, Unit] =
      values.rechunk(10_000).runForeachChunk(upsertAll(_))

  }

}
