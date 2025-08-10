package oxygen.sql.migration.model

import java.time.Instant
import oxygen.predef.core.*

/**
  * Representation of a Migration that has already been executed.
  */
final case class ExecutedMigration(
    version: Int,
    steps: Contiguous[ExecutedMigration.Step],
    startedAt: Instant,
    completedAt: Option[Instant],
) {

  def comesFrom(that: CalculatedMigration): Boolean =
    this.version == that.version &&
      this.steps.length == that.steps.length &&
      this.steps.iterator.zip(that.steps.iterator).forall { _.comesFrom(_) }

  def toIndentedString: IndentedString =
    IndentedString.section(s"Planned Migration #$version")(
      s"started-at: $startedAt",
      s"completedAt-at: ${completedAt.fold("never")(_.toString)}",
      IndentedString.section("steps")(steps.map(_.toIndentedString)),
    )

}
object ExecutedMigration {

  final case class Step(
      stepNo: Int,
      derived: Boolean,
      step: StepType,
      sql: Option[String],
  ) {

    def comesFrom(that: CalculatedMigration.Step): Boolean =
      this.stepNo == that.stepNo &&
        this.step.comesFrom(that.step)

    def toIndentedString: IndentedString =
      IndentedString.section(s"[$stepNo]${if (derived) " (derived)" else ""}:")(
        step.toIndentedString,
        sql.map { sql => IndentedString.section("sql:")(sql) },
      )

  }

  enum StepType {

    case Diff(diff: StateDiff)

    final def comesFrom(that: CalculatedMigration.StepType): Boolean =
      (this, that) match {
        case (ExecutedMigration.StepType.Diff(a), CalculatedMigration.StepType.Diff(b)) => a == b
      }

    final def toIndentedString: IndentedString = this match
      case StepType.Diff(diff) => IndentedString.section("diff:")(diff.toIndentedString)

  }

}
