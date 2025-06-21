package oxygen.sql.migration.model

import oxygen.predef.core.*
import oxygen.sql.schema.TableRepr

/**
  * Representation of a migration that has not yet been calculated.
  * This allows the flexibility to have oxygen-migration automatically figure out what migration steps you need.
  */
final case class PlannedMigration(
    version: Int,
    tables: Contiguous[TableRepr[?]],
    steps: Contiguous[PlannedMigration.StepType],
) {

  def toIndentedString: IndentedString =
    IndentedString.section(s"Planned Migration #$version")(
      IndentedString.section("tables:")(tables.map(_.toIndentedString)),
      IndentedString.section("steps:")(steps.map(_.toIndentedString)),
    )

}
object PlannedMigration {

  enum StepType {

    case Diff(diff: StateDiff)
    case Auto(tables: Contiguous[TableRepr[?]])

    final def toIndentedString: IndentedString = this match
      case StepType.Diff(diff)   => IndentedString.section("diff:")(diff.toIndentedString)
      case StepType.Auto(tables) => IndentedString.section("auto:")(tables.map(_.toIndentedString))

  }
  object StepType {

    /**
      * Auto, using tables from the migration.
      */
    def auto: SpecifyStep = SpecifyStep.InheritAuto

    /**
      * Auto, using explicitly specified tables.
      */
    def auto(tables: TableRepr[?]*): SpecifyStep =
      StepType.Auto(tables.toContiguous)

  }

  type SpecifyStep = StateDiff | SpecifyStep.InheritAuto | StepType
  object SpecifyStep {

    type InheritAuto = InheritAuto.type
    case object InheritAuto

    def apply(specify: SpecifyStep, tables: Contiguous[TableRepr[?]]): StepType = specify match
      case step: StepType  => step
      case InheritAuto     => StepType.Auto(tables)
      case diff: StateDiff => StepType.Diff(diff)

  }

  def make(version: Int)(tablesN: TableRepr[?]*)(steps: SpecifyStep*): PlannedMigration = {
    val tables = tablesN.toContiguous
    PlannedMigration(version, tables, steps.toContiguous.map(SpecifyStep(_, tables)))
  }

  def auto(version: Int)(tablesN: TableRepr[?]*): PlannedMigration =
    PlannedMigration.make(version)(tablesN*)(PlannedMigration.StepType.auto)

}
