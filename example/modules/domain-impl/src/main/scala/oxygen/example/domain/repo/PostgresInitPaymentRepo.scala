package oxygen.example.domain.repo

import oxygen.example.conversion.dbToDomain.*
import oxygen.example.conversion.domainToDb.*
import oxygen.example.core.model.*
import oxygen.example.db.model.*
import oxygen.example.domain.model.payment.*
import oxygen.sql.*
import oxygen.sql.query.{PostgresCRUDRepo, TableCompanion}
import zio.*

final case class PostgresInitPaymentRepo(db: Database) extends InitPaymentRepo, PostgresCRUDRepo.MapInfallible[InitPaymentMethodId, InitPaymentMethod] {

  override protected type DbA = InitPaymentMethodRow
  override protected type DbK = InitPaymentMethodId

  override protected val companion: TableCompanion[InitPaymentMethodRow, InitPaymentMethodId] = InitPaymentMethodRow

  override protected def keyToDb(key: InitPaymentMethodId): InitPaymentMethodId = key
  override protected def valueToDb(value: InitPaymentMethod): InitPaymentMethodRow = value.toDb
  override protected def valueToDomain(value: InitPaymentMethodRow): InitPaymentMethod = value.toDomain

}
object PostgresInitPaymentRepo {

  val layer: URLayer[Database, InitPaymentRepo] =
    ZLayer.fromFunction { PostgresInitPaymentRepo.apply }

}
