package oxygen.sql.migration.model

import oxygen.predef.core.*

/**
  * Representation of a Migration that has been calculated, but not yet executed.
  */
final case class CalculatedMigration(
    planned: PlannedMigration,
    steps: ArraySeq[CalculatedMigration.Step],
) {

  def version: Int = planned.version

  def toIndentedString: IndentedString =
    IndentedString.section(s"Calculated Migration #${planned.version}")(
      planned.toIndentedString,
      IndentedString.section("steps:")(steps.map(_.toIndentedString)),
    )

}
object CalculatedMigration {

  final case class Step(
      stepNo: Int,
      derived: Boolean,
      step: StepType,
  ) {

    def toIndentedString: IndentedString =
      IndentedString.section(s"[$stepNo]${if (derived) " (derived)" else ""}:")(
        step.toIndentedString,
      )

  }

  enum StepType {

    case Diff(diff: StateDiff)

    final def toIndentedString: IndentedString = this match
      case StepType.Diff(diff) => IndentedString.section("diff:")(diff.toIndentedString)

  }

}
