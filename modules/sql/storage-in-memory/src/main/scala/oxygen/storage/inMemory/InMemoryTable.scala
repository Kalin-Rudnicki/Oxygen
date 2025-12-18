package oxygen.storage.inMemory

import oxygen.predef.core.*
import oxygen.zio.ZIOAspectPoly
import oxygen.zio.instances.chunkSeqOps
import zio.*

final class InMemoryTable[K, A] private (
    rootInstance: InMemoryTable.Instance[K, A],
) {

  private def name: String = rootInstance.name
  private def ref: Ref[Map[K, A]] = rootInstance.ref
  private def valueToKey: A => K = rootInstance.valueToKey
  def rollbackAnyCause: ZIOAspectPoly = rootInstance.rollbackAnyCause

  def getAll: UIO[Map[K, A]] = ref.get
  def setAll[S[_]: SeqRead](values: S[A]): UIO[Unit] = (ref.set(Map.empty) *> ZIO.foreachDiscard(values.transformTo[Chunk])(insert)) @@ rollbackAnyCause

  private def modifyEither(key: K)(f: Option[A] => Either[String, Option[A]]): UIO[Unit] =
    ref
      .modify[UIO[Unit]] { map =>
        val initialValue: Option[A] = map.get(key)
        (initialValue, f(initialValue)) match {
          case (Some(initialValue), Right(Some(newValue))) =>
            val initialKey = valueToKey(initialValue)
            val newKey = valueToKey(newValue)
            if initialKey == newKey then //
              (ZIO.unit, map.updated(key, newValue))
            else
              map.get(valueToKey(newValue)) match {
                case Some(conflictingValue) => (ZIO.dieMessage(s"PK violation in [table=$name] for [key=$key], [initial=$initialValue], [updated=$newValue], [conflict=$conflictingValue]"), map)
                case None                   => (ZIO.unit, map.removed(initialKey).updated(newKey, newValue))
              }
          case (Some(_), Right(None))        => (ZIO.unit, map.removed(key))
          case (None, Right(Some(newValue))) => (ZIO.unit, map.updated(key, newValue))
          case (None, Right(None))           => (ZIO.unit, map)
          case (_, Left(value))              => (ZIO.dieMessage(value), map)
        }
      }
      .flatten

  // =====|  |=====

  def keySet: UIO[Set[K]] = getAll.map(_.keySet)
  def contains(key: K): UIO[Boolean] = getAll.map(_.contains(key))

  def values[S[_]: SeqWrite]: UIO[S[A]] = getAll.map(_.values.transformTo[S])
  def filteredValues[S[_]: SeqWrite](filter: A => Boolean): UIO[S[A]] = getAll.map(_.values.filter(filter).transformTo[S])

  def find(key: K): UIO[Option[A]] = getAll.map(_.get(key))
  def get(key: K): UIO[A] = find(key).someOrElseZIO { ZIO.dieMessage(s"No value for [key=$key] in [table=$name]") }

  def insert(value: A): UIO[Unit] = {
    val key = valueToKey(value)
    modifyEither(key) {
      case Some(existing) => s"PK violation in [table=$name] for [key=$key], [existing=$existing], [new=$value]".asLeft
      case None           => value.some.asRight
    }
  }

  def insertAll[S[_]: SeqRead](values: S[A]): UIO[Unit] = ZIO.foreachDiscard(values.transformTo[Chunk])(insert) @@ rollbackAnyCause
  def insertAll(values: A*): UIO[Unit] = insertAll(values)

  def upsert(value: A): UIO[Unit] = {
    val key = valueToKey(value)
    modifyEither(key) { _ => value.some.asRight }
  }
  def upsertAll[S[_]: SeqRead](values: S[A]): UIO[Unit] = ZIO.foreachDiscard(values.transformTo[Chunk])(upsert) @@ rollbackAnyCause
  def upsertAll(values: A*): UIO[Unit] = upsertAll(values)

  def update(value: A): UIO[Unit] = {
    val key = valueToKey(value)
    modifyEither(key) {
      case Some(_) => value.some.asRight
      case None    => None.asRight
    }
  }

  def update(key: K)(f: A => A): UIO[Unit] = modifyEither(key)(_.map(f).asRight)
  def updateWhere(filter: A => Boolean)(f: A => A): UIO[Unit] =
    ref.get.flatMap { map => ZIO.foreachDiscard(map.values.filter(filter)) { a => update(valueToKey(a))(f) } } @@ rollbackAnyCause

  def delete(key: K): UIO[Unit] = modifyEither(key) { _ => None.asRight }
  def deleteAll[S[_]: SeqRead](keys: S[K]): UIO[Unit] = ZIO.foreachDiscard(keys.transformTo[Chunk])(delete) @@ rollbackAnyCause
  def deleteAll(keys: K*): UIO[Unit] = deleteAll(keys)
  def deleteWhere(filter: A => Boolean): UIO[Unit] = ref.update { _.filterNot { (_, a) => filter(a) } }

  def truncate: UIO[Unit] = ref.set(Map.empty)

}
object InMemoryTable {

  def makeEmpty[K, A](name: String, valueToKey: A => K): UIO[InMemoryTable[K, A]] =
    for {
      ref <- Ref.make(Map.empty[K, A])
    } yield InMemoryTable(Instance(name, ref, valueToKey))

  // TODO (KR) : use this to implement transactions, have a rootInstance, and map of transaction to Instance
  //           : have some sort of global FiberRef that keeps track of transactions/rollbacks
  //           : Transactions.get.register(commit = UIO[Unit], rollback = UIO[Unit])
  //           : how to handle nested `Atomically { ... }`
  private final case class Instance[K, A](
      name: String,
      ref: Ref[Map[K, A]],
      valueToKey: A => K,
  ) {

    val rollbackAnyCause: ZIOAspectPoly =
      new ZIOAspectPoly.Impl {
        def apply[R, E, B](effect: ZIO[R, E, B])(using trace: Trace): ZIO[R, E, B] =
          ref.get.flatMap { initial => effect.tapErrorCause { _ => ref.set(initial) } }
      }

  }

}
