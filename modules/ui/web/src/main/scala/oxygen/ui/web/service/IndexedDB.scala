package oxygen.ui.web.service

import org.scalajs.dom.{window, IDBDatabase}
import scala.scalajs.js
import zio.*

/**
  * W12-T03 / W12-T04: IndexedDB open + version upgrade helper (minimal).
  * Full query API left to apps; this is the shared open/migrate surface.
  *
  * Returns a ZIO-friendly [[Database]] wrapper — never a raw DOM type.
  */
object IndexedDB {
  // TODO (KR) : this one probably still needs a lot of work...

  final case class StoreSpec(name: String, keyPath: String = "id", autoIncrement: Boolean = false)

  final case class Migration(
      version: Int,
      stores: Seq[StoreSpec],
  )

  /**
    * Thin ZIO service around an open IDB database.
    * Expand with typed transactions as product needs grow.
    */
  final class Database private[IndexedDB] (private val underlying: IDBDatabase) {

    def name: String = underlying.name
    def version: Double = underlying.version

    def close: UIO[Unit] =
      ZIO.succeed(underlying.close())

    /** Escape hatch for advanced callers; prefer adding typed ops on this class. */
    def unsafeRaw: IDBDatabase = underlying

  }

  def isAvailable: Boolean =
    window.indexedDB.toOption.isDefined

  def isAvailableZIO: UIO[Boolean] =
    ZIO.succeed(isAvailable)

  /**
    * Open DB at `targetVersion`, creating object stores listed in migrations for versions
    * greater than the existing DB version (onupgradeneeded).
    */
  def open(
      name: String,
      targetVersion: Int,
      migrations: Seq[Migration],
  ): Task[Database] =
    ZIO.async { cb =>
      window.indexedDB.toOption match {
        case None =>
          cb(ZIO.fail(new RuntimeException("IndexedDB not available")))
        case Some(idb) =>
          val req = idb.open(name, targetVersion)
          req.onerror = { _ =>
            cb(ZIO.fail(new RuntimeException(s"IDB open failed: ${req.error}")))
          }
          req.onupgradeneeded = { ev =>
            val db = req.result.asInstanceOf[IDBDatabase]
            val oldV = ev.asInstanceOf[js.Dynamic].oldVersion.asInstanceOf[Double].toInt
            migrations
              .filter(_.version > oldV)
              .sortBy(_.version)
              .foreach { m =>
                m.stores.foreach { s =>
                  if !db.objectStoreNames.contains(s.name) then {
                    val opts = js.Dynamic.literal(keyPath = s.keyPath, autoIncrement = s.autoIncrement)
                    db.createObjectStore(s.name, opts.asInstanceOf[org.scalajs.dom.IDBCreateObjectStoreOptions])
                  }
                }
              }
          }
          req.onsuccess = { _ =>
            cb(ZIO.succeed(new Database(req.result)))
          }
      }
    }

  /** Pure: max migration version (for tests / callers). */
  def maxVersion(migrations: Seq[Migration]): Int =
    if migrations.isEmpty then 1 else migrations.map(_.version).max

}
