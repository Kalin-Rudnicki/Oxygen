package oxygen.example.domain.repo

import oxygen.example.conversion.dbToDomain.*
import oxygen.example.conversion.domainToDb.*
import oxygen.example.core.model.*
import oxygen.example.db.model.*
import oxygen.example.domain.model.payment.*
import oxygen.sql.*
import oxygen.sql.query.{PostgresCRUDRepo, TableCompanion}
import zio.*

final case class PostgresPaymentRepo(db: Database) extends PaymentRepo, PostgresCRUDRepo.MapInfallible[PaymentId, Payment] {

  override def paymentsForUser(userId: UserId): UIO[Seq[Payment]] =
    PaymentRow.byUserId.map(_.toDomain).execute(userId).to[Seq].orDie.usingDb(db)

  ///////  ///////////////////////////////////////////////////////////////

  override protected type DbA = PaymentRow
  override protected type DbK = PaymentId

  override protected val companion: TableCompanion[PaymentRow, PaymentId] = PaymentRow

  override protected def keyToDb(key: PaymentId): PaymentId = key
  override protected def valueToDb(value: Payment): PaymentRow = value.toDb
  override protected def valueToDomain(value: PaymentRow): Payment = value.toDomain

}
object PostgresPaymentRepo {

  val layer: URLayer[Database, PaymentRepo] =
    ZLayer.fromFunction { PostgresPaymentRepo.apply }

}
