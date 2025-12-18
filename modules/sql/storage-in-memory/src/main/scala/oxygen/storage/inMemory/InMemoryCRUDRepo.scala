package oxygen.storage.inMemory

import oxygen.predef.core.*
import oxygen.storage.CRUDRepo
import oxygen.zio.instances.chunkSeqOps
import zio.*
import zio.stream.*

trait InMemoryCRUDRepo[K, A] extends CRUDRepo[K, A] {

  protected val table: InMemoryTable[K, A]

  override def insert(value: A): UIO[Unit] = table.insert(value)

  override def insertAll[S[_]: SeqOps](values: S[A]): UIO[Unit] = table.insertAll(values)

  override def insertAllStream[R, E](values: ZStream[R, E, A]): ZIO[R, E, Unit] = values.foreach(table.insert) @@ table.rollbackAnyCause

  override def selectAll[S[_]: SeqOps]: UIO[S[A]] = table.values[S]

  override def selectAllStream: UStream[A] = ZStream.fromZIO { selectAll[Chunk] }.flatMap(ZStream.fromChunk)

  override def findByKey(key: K): UIO[Option[A]] = table.find(key)

  override def getByKeyOrDie(key: K): UIO[A] = table.get(key)

  override def update(value: A): UIO[Unit] = table.update(value)

  override def updateAll[S[_]: SeqOps](values: S[A]): UIO[Unit] = table.upsertAll(values)

  override def delete(key: K): UIO[Unit] = table.delete(key)

  override def deleteAll[S[_]: SeqOps](keys: S[K]): UIO[Unit] = table.deleteAll(keys)

  override def truncate(using Unsafe): UIO[Unit] = table.truncate

  override def truncateCascade(using Unsafe): UIO[Unit] = table.truncate

  override def upsert(value: A): UIO[Unit] = table.upsert(value)

  override def upsertAll[S[_]: SeqOps](values: S[A]): UIO[Unit] = table.upsertAll(values)

  override def upsertAllStream[R, E](values: ZStream[R, E, A]): ZIO[R, E, Unit] = values.foreach(table.upsert) @@ table.rollbackAnyCause

}
