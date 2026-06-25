package oxygen.sql.migration.model

import java.time.Instant
import oxygen.core.Version
import oxygen.predef.core.*

/**
  * Representation of a Migration that has already been executed against a database.
  */
final case class ExecutedMigration(
    version: Version,
    steps: ArraySeq[ExecutedMigration.Step],
    startedAt: Instant,
    completedAt: Option[Instant],
) {

  def toIndentedString: IndentedString =
    IndentedString.section(s"Executed Migration $version")(
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

    def toIndentedString: IndentedString =
      IndentedString.section(s"[$stepNo]${if derived then " (derived)" else ""}:")(
        step.toIndentedString,
        sql.map { sql => IndentedString.section("sql:")(sql) },
      )

  }

  enum StepType {

    case Diff(diff: StateDiff)

    final def toIndentedString: IndentedString = this match
      case StepType.Diff(diff) => IndentedString.section("diff:")(diff.toIndentedString)

  }

}
