package oxygen.sql.migration.compat

import oxygen.predef.core.*
import oxygen.schema.compat.{AddedRemovedBoth, FromToValues}
import oxygen.sql.migration.model.*
import oxygen.sql.migration.model.EntityRef.*
import oxygen.sql.migration.persistence.model.MigrationCompatibility
import oxygen.sql.schema.Column

/**
  * The structural diff of two [[MigrationState]]s (`from` -> `to`), the DB analogue of
  * `oxygen.schema.compat.ComparisonResult`. It reuses that package's generic `FromToValues` /
  * `AddedRemovedBoth` lattice, layers on DB-specific compatibility rules, and collapses to the
  * binary [[MigrationCompatibility]] the migration harness already understands -- while retaining
  * per-column diagnostics via [[toIndentedString]].
  *
  * The rules agree with `MigrationGenerator.classifyOne` everywhere they overlap; the one addition
  * is per-column type widening/narrowing (which the coarse gate does not model). The empty-table
  * exception falls out structurally: constraints on a freshly-`added` table sit in the added bucket
  * (compatible), and only constraints on a table present in `both` states face the strict rules.
  */
final case class DbComparisonResult(
    extensions: AddedRemovedBoth.Many[String, String],
    schemas: AddedRemovedBoth.Many[SchemaRef, SchemaRef],
    tables: AddedRemovedBoth.Many[TableState, DbComparisonResult.TableComparison],
) {

  import DbComparisonResult.worst

  lazy val compatibility: MigrationCompatibility =
    worst(
      Seq(
        extensions.added.map(_ => MigrationCompatibility.BackwardsCompatible), // CREATE EXTENSION
        extensions.removed.map(_ => MigrationCompatibility.BackwardsCompatible), // dropping an extension is not itself data-breaking
        schemas.added.map(_ => MigrationCompatibility.BackwardsCompatible), // CREATE SCHEMA
        schemas.removed.map(_ => MigrationCompatibility.Incompatible), // DROP SCHEMA
        tables.added.map(_ => MigrationCompatibility.BackwardsCompatible), // CREATE TABLE
        tables.removed.map(_ => MigrationCompatibility.Incompatible), // DROP TABLE
        tables.both.map(_.compatibility),
      ).flatten,
    )

  def isCompatible: Boolean = compatibility == MigrationCompatibility.BackwardsCompatible

  def isDifferent: Boolean =
    extensions.added.nonEmpty || extensions.removed.nonEmpty ||
      schemas.added.nonEmpty || schemas.removed.nonEmpty ||
      tables.added.nonEmpty || tables.removed.nonEmpty || tables.both.exists(_.isDifferent)

  /** Collapse away the unchanged `both` entries, leaving only the actual differences. */
  def pruned: DbComparisonResult =
    DbComparisonResult(
      extensions = extensions.prune(_ => None),
      schemas = schemas.prune(_ => None),
      tables = tables.prune(tc => Option.when(tc.isDifferent)(tc.pruned)),
    )

  def toIndentedString: IndentedString =
    IndentedString.section("DbComparisonResult:")(
      s"compatibility: ${MigrationCompatibility.show(compatibility)}",
      IndentedString.section("extensions:")(extensions.toIndentedString(e => s"- $e", e => s"- $e")),
      IndentedString.section("schemas:")(schemas.toIndentedString(s => s"- ${s.schemaName}", s => s"- ${s.schemaName}")),
      IndentedString.section("tables:")(tables.toIndentedString(t => s"- ${t.tableName}", _.toIndentedString)),
    )

}
object DbComparisonResult {

  /** Backwards-compatible unless ANY component is incompatible (mirrors `MigrationGenerator.classify`). */
  private[compat] def worst(cs: Seq[MigrationCompatibility]): MigrationCompatibility =
    if cs.contains(MigrationCompatibility.Incompatible) then MigrationCompatibility.Incompatible
    else MigrationCompatibility.BackwardsCompatible

  /** Diff of a single table present in BOTH states (an existing, potentially non-empty table). */
  final case class TableComparison(
      tableRef: TableRef,
      columns: AddedRemovedBoth.Many[Column, ColumnComparison],
      primaryKey: FromToValues[Set[String]],
      foreignKeys: AddedRemovedBoth.Many[ForeignKeyState, ConstraintComparison[ForeignKeyState]],
      indices: AddedRemovedBoth.Many[IndexState, ConstraintComparison[IndexState]],
  ) {

    lazy val compatibility: MigrationCompatibility =
      worst(
        Seq(
          columns.added.map(c => if c.nullable then MigrationCompatibility.BackwardsCompatible else MigrationCompatibility.Incompatible), // add nullable ok / non-nullable breaks
          columns.removed.map(_ => MigrationCompatibility.Incompatible), // DROP COLUMN
          columns.both.map(_.compatibility),
          Seq(if primaryKey.isDifferent then MigrationCompatibility.Incompatible else MigrationCompatibility.BackwardsCompatible), // PK change on existing table
          foreignKeys.added.map(_ => MigrationCompatibility.Incompatible), // add FK to existing table
          foreignKeys.removed.map(_ => MigrationCompatibility.BackwardsCompatible),
          foreignKeys.both.map(_.compatibility),
          indices.added.map(i => if i.unique then MigrationCompatibility.Incompatible else MigrationCompatibility.BackwardsCompatible), // add unique index to existing table
          indices.removed.map(_ => MigrationCompatibility.BackwardsCompatible),
          indices.both.map(_.compatibility),
        ).flatten,
      )

    def isDifferent: Boolean =
      columns.added.nonEmpty || columns.removed.nonEmpty || columns.both.exists(_.isDifferent) ||
        primaryKey.isDifferent ||
        foreignKeys.added.nonEmpty || foreignKeys.removed.nonEmpty || foreignKeys.both.exists(_.isDifferent) ||
        indices.added.nonEmpty || indices.removed.nonEmpty || indices.both.exists(_.isDifferent)

    def pruned: TableComparison =
      copy(
        columns = columns.prune(c => Option.when(c.isDifferent)(c)),
        foreignKeys = foreignKeys.prune(fk => Option.when(fk.isDifferent)(fk)),
        indices = indices.prune(ix => Option.when(ix.isDifferent)(ix)),
      )

    def toIndentedString: IndentedString =
      IndentedString.section(s"table($tableRef):")(
        IndentedString.keyValue("primary-key: ", primaryKey.toIndentedString(pk => pk.toSeq.sorted.mkString("(", ", ", ")"))),
        IndentedString.section("columns:")(columns.toIndentedString(c => s"- ${c.toSql}", _.toIndentedString)),
        IndentedString.section("foreign-keys:")(foreignKeys.toIndentedString(fk => s"- ${fk.ref}", _.toIndentedString)),
        IndentedString.section("indices:")(indices.toIndentedString(ix => s"- ${ix.ref}", _.toIndentedString)),
      )

  }

  /** Diff of a single column present in BOTH tables. */
  final case class ColumnComparison(
      name: String,
      nullable: FromToValues[Boolean],
      columnType: TypeComparison,
  ) {

    lazy val compatibility: MigrationCompatibility = {
      val nullableCompatibility: MigrationCompatibility = nullable match
        case FromToValues.Same(_)                => MigrationCompatibility.BackwardsCompatible
        case FromToValues.Different(false, true) => MigrationCompatibility.BackwardsCompatible // relax to NULL
        case FromToValues.Different(_, _)        => MigrationCompatibility.Incompatible // tighten to NOT NULL
      worst(Seq(nullableCompatibility, columnType.compatibility))
    }

    def isDifferent: Boolean = nullable.isDifferent || columnType.isDifferent

    def toIndentedString: IndentedString =
      IndentedString.keyValueSection(s"$name:")(
        "nullable: " -> nullable.toIndentedString(_.toString),
        "type: " -> columnType.toIndentedString,
      )

  }

  /**
    * Diff of a named constraint (FK / index) present in BOTH tables. A changed definition under the
    * same name is a drop-and-recreate on an existing table -- treated as incompatible.
    */
  final case class ConstraintComparison[A: Show](ref: EntityRef, value: FromToValues[A]) {

    def compatibility: MigrationCompatibility =
      if value.isDifferent then MigrationCompatibility.Incompatible else MigrationCompatibility.BackwardsCompatible

    def isDifferent: Boolean = value.isDifferent

    def toIndentedString: IndentedString =
      IndentedString.keyValueSection(s"$ref:")("value: " -> value.toIndentedString(_.show))

  }

}
