package oxygen.sql.query

import oxygen.predef.core.*
import oxygen.sql.*
import oxygen.storage.*
import scala.annotation.targetName
import zio.*
import zio.stream.*

object PostgresCRUDRepo {

  trait Unmapped[K, A] extends CRUDRepo[K, A] {

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Abstract
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    val db: Database

    protected val companion: TableCompanion[A, K]

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Concrete
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    // =====| Create |=====

    override final def insert(value: A): UIO[Unit] =
      companion.insert.execute(value).unit.orDie.usingDb(db)

    override final def insertAll[S[_]: SeqOps](values: S[A]): UIO[Unit] =
      companion.insert.batched(values).unit.orDie.usingDb(db)

    override final def insertAllStream[R, E](values: ZStream[R, E, A]): ZIO[R, E, Unit] =
      companion.batchOptimizedInsert.insert.streamDbDie(values).unit.usingDb(db)

    // =====| Read |=====

    override final def selectAll[S[_]: SeqOps]: UIO[S[A]] =
      companion.selectAll.execute().to[S].orDie.usingDb(db)

    override final def selectAllStream: UStream[A] =
      companion.selectAll.execute().stream.orDie.usingDb(db)

    override final def findByKey(key: K): UIO[Option[A]] =
      companion.selectByPK.execute(key).option.orDie.usingDb(db)

    override final def getByKeyOrDie(key: K): UIO[A] =
      companion.selectByPK.execute(key).single.orDie.usingDb(db)

    // =====| Update |=====

    override final def update(value: A): UIO[Unit] =
      companion.update.execute(value).unit.orDie.usingDb(db)

    override final def updateAll[S[_]: SeqOps](values: S[A]): UIO[Unit] =
      companion.update.batched(values).unit.orDie.usingDb(db)

    // =====| Delete |=====

    override final def delete(key: K): UIO[Unit] =
      companion.deleteByPK.execute(key).unit.orDie.usingDb(db)

    override final def deleteAll[S[_]: SeqOps](keys: S[K]): UIO[Unit] =
      companion.deleteByPK.batched(keys).unit.orDie.usingDb(db)

    override final def truncate(using Unsafe): UIO[Unit] =
      companion.truncate.execute().unit.orDie.usingDb(db)

    override final def truncateCascade(using Unsafe): UIO[Unit] =
      companion.truncateCascade.execute().unit.orDie.usingDb(db)

    // =====| Upsert |=====

    override final def upsert(value: A): UIO[Unit] =
      companion.upsert.execute(value).unit.orDie.usingDb(db)

    override final def upsertAll[S[_]: SeqOps](values: S[A]): UIO[Unit] =
      companion.upsert.batched(values).unit.orDie.usingDb(db)

    override final def upsertAllStream[R, E](values: ZStream[R, E, A]): ZIO[R, E, Unit] =
      companion.batchOptimizedUpsert.insert.streamDbDie(values).unit.usingDb(db)

  }

  trait MapInfallible[K, A] extends CRUDRepo[K, A] {

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Abstract
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    val db: Database

    protected type DbA
    protected type DbK

    protected val companion: TableCompanion[DbA, DbK]

    protected def keyToDb(key: K): DbK
    protected def valueToDb(value: A): DbA

    protected def valueToDomain(value: DbA): A

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Concrete
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    extension (self: K) @targetName("ext_keyToDb") private def toDb: DbK = keyToDb(self)
    extension (self: A) @targetName("ext_valueToDb") private def toDb: DbA = valueToDb(self)
    extension (self: DbA) @targetName("ext_valueToDomain") private def toDomain: A = valueToDomain(self)

    // =====| Create |=====

    override final def insert(value: A): UIO[Unit] =
      companion.insert.execute(value.toDb).unit.orDie.usingDb(db)

    override final def insertAll[S[_]: SeqOps](values: S[A]): UIO[Unit] =
      companion.insert.contramap[A](_.toDb).batched(values).unit.orDie.usingDb(db)

    override final def insertAllStream[R, E](values: ZStream[R, E, A]): ZIO[R, E, Unit] =
      companion.batchOptimizedInsert.insert.streamDbDie(values.map(_.toDb)).unit.usingDb(db)

    // =====| Read |=====

    override final def selectAll[S[_]: SeqOps]: UIO[S[A]] =
      companion.selectAll.map(_.toDomain).execute().to[S].orDie.usingDb(db)

    override final def selectAllStream: UStream[A] =
      companion.selectAll.map(_.toDomain).execute().stream.orDie.usingDb(db)

    override final def findByKey(key: K): UIO[Option[A]] =
      companion.selectByPK.map(_.toDomain).execute(key.toDb).option.orDie.usingDb(db)

    override final def getByKeyOrDie(key: K): UIO[A] =
      companion.selectByPK.map(_.toDomain).execute(key.toDb).single.orDie.usingDb(db)

    // =====| Update |=====

    override final def update(value: A): UIO[Unit] =
      companion.update.execute(value.toDb).unit.orDie.usingDb(db)

    override final def updateAll[S[_]: SeqOps](values: S[A]): UIO[Unit] =
      companion.update.contramap[A](_.toDb).batched(values).unit.orDie.usingDb(db)

    // =====| Delete |=====

    override final def delete(key: K): UIO[Unit] =
      companion.deleteByPK.execute(key.toDb).unit.orDie.usingDb(db)

    override final def deleteAll[S[_]: SeqOps](keys: S[K]): UIO[Unit] =
      companion.deleteByPK.contramap[K](_.toDb).batched(keys).unit.orDie.usingDb(db)

    override final def truncate(using Unsafe): UIO[Unit] =
      companion.truncate.execute().unit.orDie.usingDb(db)

    override final def truncateCascade(using Unsafe): UIO[Unit] =
      companion.truncateCascade.execute().unit.orDie.usingDb(db)

    // =====| Upsert |=====

    override final def upsert(value: A): UIO[Unit] =
      companion.upsert.execute(value.toDb).unit.orDie.usingDb(db)

    override final def upsertAll[S[_]: SeqOps](values: S[A]): UIO[Unit] =
      companion.upsert.contramap[A](_.toDb).batched(values).unit.orDie.usingDb(db)

    override final def upsertAllStream[R, E](values: ZStream[R, E, A]): ZIO[R, E, Unit] =
      companion.batchOptimizedUpsert.insert.streamDbDie(values.map(_.toDb)).unit.usingDb(db)

  }

  trait MapFallible[K, A] extends CRUDRepo[K, A] {

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Abstract
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    val db: Database

    protected type DbK
    protected type DbA

    protected val companion: TableCompanion[DbA, DbK]

    protected def keyToDb(key: K): DbK
    protected def valueToDb(value: A): DbA

    protected def valueToDomain(value: DbA): Either[String, A]

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Concrete
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    extension (self: K) @targetName("ext_keyToDb") private def toDb: DbK = keyToDb(self)
    extension (self: A) @targetName("ext_valueToDb") private def toDb: DbA = valueToDb(self)
    extension (self: DbA) @targetName("ext_valueToDomain") private def toDomain: Either[String, A] = valueToDomain(self)

    // =====| Create |=====

    override final def insert(value: A): UIO[Unit] =
      companion.insert.execute(value.toDb).unit.orDie.usingDb(db)

    override final def insertAll[S[_]: SeqOps](values: S[A]): UIO[Unit] =
      companion.insert.contramap[A](_.toDb).batched(values).unit.orDie.usingDb(db)

    override final def insertAllStream[R, E](values: ZStream[R, E, A]): ZIO[R, E, Unit] =
      companion.batchOptimizedInsert.insert.streamDbDie(values.map(_.toDb)).unit.usingDb(db)

    // =====| Read |=====

    override final def selectAll[S[_]: SeqOps]: UIO[S[A]] =
      companion.selectAll.mapOrFail(_.toDomain).execute().to[S].orDie.usingDb(db)

    override final def selectAllStream: UStream[A] =
      companion.selectAll.mapOrFail(_.toDomain).execute().stream.orDie.usingDb(db)

    override final def findByKey(key: K): UIO[Option[A]] =
      companion.selectByPK.mapOrFail(_.toDomain).execute(key.toDb).option.orDie.usingDb(db)

    override final def getByKeyOrDie(key: K): UIO[A] =
      companion.selectByPK.mapOrFail(_.toDomain).execute(key.toDb).single.orDie.usingDb(db)

    // =====| Update |=====

    override final def update(value: A): UIO[Unit] =
      companion.update.execute(value.toDb).unit.orDie.usingDb(db)

    override final def updateAll[S[_]: SeqOps](values: S[A]): UIO[Unit] =
      companion.update.contramap[A](_.toDb).batched(values).unit.orDie.usingDb(db)

    // =====| Delete |=====

    override final def delete(key: K): UIO[Unit] =
      companion.deleteByPK.execute(key.toDb).unit.orDie.usingDb(db)

    override final def deleteAll[S[_]: SeqOps](keys: S[K]): UIO[Unit] =
      companion.deleteByPK.contramap[K](_.toDb).batched(keys).unit.orDie.usingDb(db)

    override final def truncate(using Unsafe): UIO[Unit] =
      companion.truncate.execute().unit.orDie.usingDb(db)

    override final def truncateCascade(using Unsafe): UIO[Unit] =
      companion.truncateCascade.execute().unit.orDie.usingDb(db)

    // =====| Upsert |=====

    override final def upsert(value: A): UIO[Unit] =
      companion.upsert.execute(value.toDb).unit.orDie.usingDb(db)

    override final def upsertAll[S[_]: SeqOps](values: S[A]): UIO[Unit] =
      companion.upsert.contramap[A](_.toDb).batched(values).unit.orDie.usingDb(db)

    override final def upsertAllStream[R, E](values: ZStream[R, E, A]): ZIO[R, E, Unit] =
      companion.batchOptimizedUpsert.insert.streamDbDie(values.map(_.toDb)).unit.usingDb(db)

  }

}
