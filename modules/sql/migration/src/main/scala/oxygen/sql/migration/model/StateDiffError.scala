package oxygen.sql.migration.model

import oxygen.core.IndentedString

sealed trait StateDiffError extends Throwable {

  override final def getMessage: String =
    toIndentedString.toString

  final def toIndentedString: IndentedString =
    this match {
      case StateDiffError.DeriveError(ref, cause) =>
        IndentedString.section("unable to auto-derive")(
          s"ref: $ref",
          s"cause: $cause",
        )
      case StateDiffError.DiffError(ref, cause) =>
        IndentedString.section("unable to generate diff")(
          s"ref: $ref",
          s"cause: $cause",
        )
      case StateDiffError.ApplyError(ref, cause, diff) =>
        IndentedString.section("unable to apply diff")(
          s"ref: $ref",
          s"cause: $cause",
          IndentedString.section("diff:")(diff.toIndentedString),
        )
    }

}
object StateDiffError {

  final case class DeriveError(
      ref: EntityRef,
      cause: Cause,
  ) extends StateDiffError

  final case class DiffError(
      ref: EntityRef,
      cause: Cause,
  ) extends StateDiffError

  final case class ApplyError(
      ref: EntityRef,
      cause: Cause,
      diff: StateDiff,
  ) extends StateDiffError

  sealed trait Cause
  object Cause {
    case object DoesNotExist extends Cause
    case object AlreadyExists extends Cause
    case object InvalidPrimaryKeyAlteration extends Cause
    case object NullabilityNotChanged extends Cause
    case object SameNameDifferentType extends Cause
    case object NonDistinctColumnNames extends Cause
    final case class TableStillInSchema(table: EntityRef.TableRef) extends Cause
  }

}
