package oxygen.sql.migration

import oxygen.predef.test.*
import oxygen.sql.migration.model.{MigrationSchema, MigrationState}
import oxygen.sql.query.TableCompanion
import oxygen.sql.schema.*
import scala.collection.immutable.ArraySeq

object MigrationVerifySpec extends OxygenSpecDefault {

  @tableName("account")
  final case class AccountV1(@primaryKey id: Int, name: String)
  object AccountV1 extends TableCompanion[AccountV1, Int](TableRepr.derived[AccountV1])

  // adds a nullable column relative to V1
  @tableName("account")
  final case class AccountV2(@primaryKey id: Int, name: String, nickname: Option[String])
  object AccountV2 extends TableCompanion[AccountV2, Int](TableRepr.derived[AccountV2])

  private def stateOf(tables: TableRepr[?]*): MigrationState =
    MigrationState.fromTables(ArraySeq.from(tables)) match
      case Right(s)    => s
      case Left(error) => throw new RuntimeException(error.toString)

  override def testSpec: TestSpec =
    suite("MigrationVerifySpec")(
      test("no pending diffs when the FS state already matches the current-code tables") {
        val pending = MigrationService.pendingDiffs(stateOf(AccountV1.tableRepr), MigrationSchema.of(AccountV1.tableRepr))
        assertTrue(pending.map(_.isEmpty) == Right(true))
      },
      test("pending diffs when the current-code tables drift ahead of the FS state") {
        val pending = MigrationService.pendingDiffs(stateOf(AccountV1.tableRepr), MigrationSchema.of(AccountV2.tableRepr))
        assertTrue(pending.map(_.nonEmpty) == Right(true))
      },
      test("duplicate tables are rejected") {
        assertTrue(MigrationState.fromTables(ArraySeq(AccountV1.tableRepr, AccountV1.tableRepr)).isLeft)
      },
    )

}
