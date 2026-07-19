package oxygen.transform

import oxygen.core.model.ScopePath
import oxygen.predef.core.*

final case class TransformError(
    scope: List[ScopePath],
    cause: TransformError.Cause,
) extends Error {

  def atIndex(index: Int): TransformError = TransformError(ScopePath.Index(index) :: scope, cause)
  def atField(field: String): TransformError = TransformError(ScopePath.Field(field) :: scope, cause)
  def atSubType(typeName: String): TransformError = TransformError(ScopePath.SubType(typeName) :: scope, cause)

  override def errorMessage: Text = str"${Text.mkString(scope)} : $cause"

}
object TransformError {

  def getRequired[A](fieldName: String, value: Option[A]): Either[TransformError, A] =
    value.toRight { TransformError(ScopePath.Field(fieldName) :: Nil, Cause.MissingRequired) }

  sealed trait Cause extends Showable
  object Cause {

    final case class DecodingFailure(message: String) extends Cause {
      override def show: Text = Text.fromString(message)
    }

    case object MissingRequired extends Cause {
      override def show: Text = str"Missing required"
    }

  }

}
