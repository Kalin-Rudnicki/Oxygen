package oxygen.example.domain.repo

import oxygen.example.conversion.dbToDomain.*
import oxygen.example.conversion.domainToDb.*
import oxygen.example.core.model.*
import oxygen.example.db.model.*
import oxygen.example.domain.model.payment.*
import oxygen.sql.*
import oxygen.sql.query.{PostgresCRUDRepo, TableCompanion}
import zio.*

final case class PostgresPaymentMethodRepo(db: Database) extends PaymentMethodRepo, PostgresCRUDRepo.MapInfallible[PaymentMethodId, PaymentMethod] {

  override protected type DbA = PaymentMethodRow
  override protected type DbK = PaymentMethodId

  override protected val companion: TableCompanion[PaymentMethodRow, PaymentMethodId] = PaymentMethodRow

  override protected def keyToDb(key: PaymentMethodId): PaymentMethodId = key
  override protected def valueToDb(value: PaymentMethod): PaymentMethodRow = value.toDb
  override protected def valueToDomain(value: PaymentMethodRow): PaymentMethod = value.toDomain

}
object PostgresPaymentMethodRepo {

  val layer: URLayer[Database, PaymentMethodRepo] =
    ZLayer.fromFunction { PostgresPaymentMethodRepo.apply }

}
