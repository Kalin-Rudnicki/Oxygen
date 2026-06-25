package oxygen.sql.migration

import oxygen.core.Version
import oxygen.predef.core.*
import oxygen.sql.migration.delta.MigrationPlanner
import oxygen.sql.migration.model.*
import oxygen.sql.migration.model.EntityRef.TableRef
import oxygen.sql.migration.persistence.conversion.domainToDb.*
import oxygen.sql.migration.persistence.model.*

/**
  * Pure migration generator: diffs the latest persisted state against the target (current-code)
  * state, classifies the result, and produces the next [[PersistedMigrationFile]].
  *
  * Deliberately free of IO / env-vars / DB: the env-var gating (ALLOW_UPDATE / ALLOW_INCOMPATIBLE)
  * and the decision to actually write the file are policy applied by the test harness (CP5), which
  * consults [[GenerateResult.compatibility]].
  */
object MigrationGenerator {

  /**
    * @param previous the latest persisted (version, state), or `None` for a genesis migration
    * @param target   the desired state, derived from the current-code table definitions
    */
  def generate(
      previous: Option[(Version, MigrationState)],
      target: MigrationState,
  ): Either[StateDiffError, GenerateResult] = {
    val currentState = previous.fold(MigrationState.empty)(_._2)
    MigrationPlanner.diffStates(currentState, target).map { diffs =>
      if diffs.isEmpty then GenerateResult.UpToDate
      else {
        val compatibility = classify(diffs)
        val nextVersion = bump(previous.map(_._1), compatibility)
        val steps = diffs.zipWithIndex.map { case (diff, idx) =>
          PersistedMigrationFile.Step(stepNo = idx + 1, derived = true, step = diff.toDb)
        }
        val file = PersistedMigrationFile(
          formatVersion = PersistedMigrationFile.currentFormatVersion,
          version = nextVersion.show,
          previousVersion = previous.map(_._1.show),
          compatibility = compatibility,
          diff = steps,
          state = target.toDb,
        )
        GenerateResult.Generated(file, compatibility)
      }
    }
  }

  /** Overall classification: incompatible if any single diff is incompatible. */
  def classify(diffs: ArraySeq[StateDiff]): MigrationCompatibility = {
    val createdTables: Set[TableRef] = diffs.collect { case StateDiff.AlterTable.CreateTable(t) => t.tableName }.toSet
    if diffs.exists(classifyOne(_, createdTables) == MigrationCompatibility.Incompatible)
    then MigrationCompatibility.Incompatible
    else MigrationCompatibility.BackwardsCompatible
  }

  /**
    * Per-diff classification. Context-aware: constraints (unique index / FK) on a table that is
    * being created in this same migration cannot fail against existing data (the table is empty),
    * so they stay compatible.
    */
  private def classifyOne(diff: StateDiff, createdTables: Set[TableRef]): MigrationCompatibility = {
    import MigrationCompatibility.*
    diff match
      // additive
      case _: StateDiff.AlterExtension.CreateExtension => BackwardsCompatible
      case _: StateDiff.AlterSchema.CreateSchema       => BackwardsCompatible
      case _: StateDiff.AlterTable.CreateTable         => BackwardsCompatible
      // columns
      case StateDiff.AlterColumn.CreateColumn(_, column)  => if column.nullable then BackwardsCompatible else Incompatible
      case StateDiff.AlterColumn.SetNullable(_, nullable) => if nullable then BackwardsCompatible else Incompatible
      case _: StateDiff.AlterColumn.DropColumn            => Incompatible
      case _: StateDiff.AlterColumn.RenameColumn          => Incompatible
      // foreign keys
      case StateDiff.AlterForeignKey.CreateForeignKey(fk)               => if createdTables.contains(fk.self) then BackwardsCompatible else Incompatible
      case _: StateDiff.AlterForeignKey.DropForeignKey                  => BackwardsCompatible
      case _: StateDiff.AlterForeignKey.RenameExplicitlyNamedForeignKey => BackwardsCompatible
      case _: StateDiff.AlterForeignKey.RenameAutoNamedForeignKey       => BackwardsCompatible
      // indices
      case StateDiff.AlterIndex.CreateIndex(idx)              => if !idx.unique || createdTables.contains(idx.self) then BackwardsCompatible else Incompatible
      case _: StateDiff.AlterIndex.DropIndex                  => BackwardsCompatible
      case _: StateDiff.AlterIndex.RenameExplicitlyNamedIndex => BackwardsCompatible
      case _: StateDiff.AlterIndex.RenameAutoNamedIndex       => BackwardsCompatible
      // destructive
      case _: StateDiff.AlterTable.DropTable   => Incompatible
      case _: StateDiff.AlterSchema.DropSchema => Incompatible
      // renames (deferred; shouldn't appear in auto-generated diffs, but classify defensively)
      case _: StateDiff.AlterSchema.RenameSchema => Incompatible
      case _: StateDiff.AlterTable.RenameTable   => Incompatible
  }

  /** Genesis -> 1.0.0; incompatible -> major bump; backwards-compatible -> minor bump. */
  private def bump(previous: Option[Version], compatibility: MigrationCompatibility): Version =
    previous match
      case None    => Version.make(1, 0, 0)()
      case Some(v) =>
        compatibility match
          case MigrationCompatibility.Incompatible        => Version.make(v.major + 1, 0, 0)()
          case MigrationCompatibility.BackwardsCompatible => Version.make(v.major, v.minor + 1, 0)()

  enum GenerateResult {
    case UpToDate
    case Generated(file: PersistedMigrationFile, compatibility: MigrationCompatibility)
  }

}
