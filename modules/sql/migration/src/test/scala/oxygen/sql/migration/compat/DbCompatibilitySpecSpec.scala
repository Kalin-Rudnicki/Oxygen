package oxygen.sql.migration.compat

import oxygen.predef.test.*
import oxygen.sql.migration.model.*
import oxygen.sql.migration.model.EntityRef.*
import oxygen.sql.migration.persistence.model.MigrationCompatibility
import oxygen.sql.schema.Column

object DbCompatibilitySpecSpec extends OxygenSpecDefault {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def tableRef(name: String): TableRef = EntityRef.TableRef("public", name)

  private def table(
      name: String,
      pk: ArraySeq[Column],
      columns: ArraySeq[Column],
      foreignKeys: ArraySeq[ForeignKeyState],
      indices: ArraySeq[IndexState],
  ): TableState =
    TableState(tableRef(name), pk, columns, foreignKeys, indices)

  private def simpleTable(name: String, columns: ArraySeq[Column]): TableState =
    table(name, ArraySeq.empty, columns, ArraySeq.empty, ArraySeq.empty)

  private def state(tables: TableState*): MigrationState =
    MigrationState(Set.empty, Set(SchemaRef("public")), tables.map(t => t.tableName -> t).toMap)

  private val idCol: Column = Column("id", Column.Type.UUID, nullable = false)

  private def index(table: String, unique: Boolean, columns: String*): IndexState =
    IndexState(None, tableRef(table), unique, ArraySeq.from(columns))

  private def foreignKey(self: String, references: String, column: String): ForeignKeyState =
    ForeignKeyState(None, tableRef(self), tableRef(references), ArraySeq(ForeignKeyState.Pair(column, "id")))

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Assertions
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def assertCompatible(from: MigrationState, to: MigrationState) = {
    val result = DbCompatibilitySpec.compare(from, to)
    assertTrue(
      result.compatibility == MigrationCompatibility.BackwardsCompatible,
      result.isCompatible,
    )
  }

  private def assertIncompatible(from: MigrationState, to: MigrationState) = {
    val result = DbCompatibilitySpec.compare(from, to)
    assertTrue(
      result.compatibility == MigrationCompatibility.Incompatible,
      !result.isCompatible,
    )
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Tests
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private val typeCompareSpec: TestSpec =
    suite("TypeComparison.compare")(
      test("identical is Same + compatible") {
        val c = TypeComparison.compare(Column.Type.Int, Column.Type.Int)
        assertTrue(c == TypeComparison.Same(Column.Type.Int), c.isCompatible, !c.isDifferent)
      },
      test("SmallInt -> Int -> BigInt widen (compatible)") {
        assertTrue(
          TypeComparison.compare(Column.Type.SmallInt, Column.Type.Int).isCompatible,
          TypeComparison.compare(Column.Type.SmallInt, Column.Type.BigInt).isCompatible,
          TypeComparison.compare(Column.Type.Int, Column.Type.BigInt).isCompatible,
          TypeComparison.compare(Column.Type.Real, Column.Type.DoublePrecision).isCompatible,
        )
      },
      test("reverse of a widening narrows (incompatible)") {
        assertTrue(
          !TypeComparison.compare(Column.Type.Int, Column.Type.SmallInt).isCompatible,
          !TypeComparison.compare(Column.Type.BigInt, Column.Type.Int).isCompatible,
          !TypeComparison.compare(Column.Type.DoublePrecision, Column.Type.Real).isCompatible,
        )
      },
      test("cross-kind change fails closed") {
        assertTrue(
          !TypeComparison.compare(Column.Type.Int, Column.Type.Text).isCompatible,
          !TypeComparison.compare(Column.Type.Int, Column.Type.Numeric).isCompatible,
          !TypeComparison.compare(Column.Type.Json, Column.Type.Jsonb).isCompatible,
        )
      },
      test("Array delegates to its element type") {
        assertTrue(
          TypeComparison.compare(Column.Type.Array(Column.Type.SmallInt), Column.Type.Array(Column.Type.Int)).isCompatible,
          !TypeComparison.compare(Column.Type.Array(Column.Type.Int), Column.Type.Array(Column.Type.SmallInt)).isCompatible,
        )
      },
    )

  private val columnSpec: TestSpec = {
    val base = state(simpleTable("foo", ArraySeq(idCol, Column("age", Column.Type.Int, nullable = false))))
    suite("columns")(
      test("identical states are compatible + not different") {
        assertCompatible(base, base) &&
        assertTrue(!DbCompatibilitySpec.compare(base, base).isDifferent)
      },
      test("add nullable column is compatible") {
        val to = state(simpleTable("foo", ArraySeq(idCol, Column("age", Column.Type.Int, nullable = false), Column("nick", Column.Type.Text, nullable = true))))
        assertCompatible(base, to)
      },
      test("add non-nullable column is incompatible") {
        val to = state(simpleTable("foo", ArraySeq(idCol, Column("age", Column.Type.Int, nullable = false), Column("nick", Column.Type.Text, nullable = false))))
        assertIncompatible(base, to)
      },
      test("drop column is incompatible") {
        val to = state(simpleTable("foo", ArraySeq(idCol)))
        assertIncompatible(base, to)
      },
      test("relax NOT NULL -> NULL is compatible") {
        val to = state(simpleTable("foo", ArraySeq(idCol, Column("age", Column.Type.Int, nullable = true))))
        assertCompatible(base, to)
      },
      test("tighten NULL -> NOT NULL is incompatible") {
        val nullableBase = state(simpleTable("foo", ArraySeq(idCol, Column("age", Column.Type.Int, nullable = true))))
        assertIncompatible(nullableBase, base)
      },
      test("column type widening compatible, narrowing incompatible") {
        val widened = state(simpleTable("foo", ArraySeq(idCol, Column("age", Column.Type.BigInt, nullable = false))))
        assertCompatible(base, widened) && assertIncompatible(widened, base)
      },
    )
  }

  private val tableSpec: TestSpec = {
    val base = state(simpleTable("foo", ArraySeq(idCol)))
    suite("tables")(
      test("add table is compatible") {
        val to = state(simpleTable("foo", ArraySeq(idCol)), simpleTable("bar", ArraySeq(idCol)))
        assertCompatible(base, to)
      },
      test("drop table is incompatible") {
        val from = state(simpleTable("foo", ArraySeq(idCol)), simpleTable("bar", ArraySeq(idCol)))
        assertIncompatible(from, base)
      },
      test("empty-table exception: constraints on a NEW table are compatible") {
        // A newly-added table with a non-nullable column AND a unique index is still compatible,
        // because the table is empty (the whole table lives in the `added` bucket).
        val newTable =
          table(
            "bar",
            ArraySeq(idCol),
            ArraySeq(idCol, Column("email", Column.Type.Text, nullable = false)),
            ArraySeq.empty,
            ArraySeq(index("bar", unique = true, "email")),
          )
        assertCompatible(base, state(simpleTable("foo", ArraySeq(idCol)), newTable))
      },
      test("PK change on an existing table is incompatible") {
        val from = state(table("foo", ArraySeq(idCol), ArraySeq(idCol, Column("k", Column.Type.Text, nullable = false)), ArraySeq.empty, ArraySeq.empty))
        val to = state(table(
          "foo",
          ArraySeq(Column("k", Column.Type.Text, nullable = false)),
          ArraySeq(idCol, Column("k", Column.Type.Text, nullable = false)),
          ArraySeq.empty,
          ArraySeq.empty,
        ))
        assertIncompatible(from, to)
      },
    )
  }

  private val constraintSpec: TestSpec = {
    val base = state(table("foo", ArraySeq(idCol), ArraySeq(idCol, Column("owner", Column.Type.UUID, nullable = false)), ArraySeq.empty, ArraySeq.empty))
    suite("constraints on existing tables")(
      test("add unique index is incompatible") {
        val to = state(table("foo", ArraySeq(idCol), ArraySeq(idCol, Column("owner", Column.Type.UUID, nullable = false)), ArraySeq.empty, ArraySeq(index("foo", unique = true, "owner"))))
        assertIncompatible(base, to)
      },
      test("add non-unique index is compatible") {
        val to = state(table("foo", ArraySeq(idCol), ArraySeq(idCol, Column("owner", Column.Type.UUID, nullable = false)), ArraySeq.empty, ArraySeq(index("foo", unique = false, "owner"))))
        assertCompatible(base, to)
      },
      test("drop index is compatible") {
        val from = state(table("foo", ArraySeq(idCol), ArraySeq(idCol, Column("owner", Column.Type.UUID, nullable = false)), ArraySeq.empty, ArraySeq(index("foo", unique = true, "owner"))))
        assertCompatible(from, base)
      },
      test("add foreign key to an existing table is incompatible") {
        val to = state(table("foo", ArraySeq(idCol), ArraySeq(idCol, Column("owner", Column.Type.UUID, nullable = false)), ArraySeq(foreignKey("foo", "bar", "owner")), ArraySeq.empty))
        assertIncompatible(base, to)
      },
    )
  }

  override def testSpec: TestSpec =
    suite("DbCompatibilitySpecSpec")(
      typeCompareSpec,
      columnSpec,
      tableSpec,
      constraintSpec,
    )

}
