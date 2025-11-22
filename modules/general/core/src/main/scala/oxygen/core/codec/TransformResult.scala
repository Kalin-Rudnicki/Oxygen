package oxygen.core.codec

import oxygen.core.{Lazy, TypeTag}
import oxygen.core.collection.NonEmptyList
import oxygen.core.typeclass.Show

// TODO (KR) : is this TOO overkill...??
sealed trait TransformResult[+A] {

  val rTransforms: List[SuccessfulTransform]

}
object TransformResult {

  final case class Success[+A](rTransforms: List[SuccessfulTransform], value: A) extends TransformResult[A]

  final case class Failure(rTransforms: List[SuccessfulTransform], causes: NonEmptyList[Any]) extends TransformResult[Nothing]

  // TODO (KR) :

}

final case class SuccessfulTransform(
    fromType: TypeTag[?],
    toType: TypeTag[?],
    fromValue: Lazy[Show.Shown],
    toValue: Lazy[Show.Shown],
)

final case class TransformError(
    fromType: TypeTag[?],
    toType: TypeTag[?],
    fromValue: Lazy[Show.Shown],
    scope: List[TransformError.Scope],
)
object TransformError {

  sealed trait Scope
  object Scope {
    final case class InField(field: String) extends Scope
    final case class AtIndex(index: Int) extends Scope
  }

  sealed trait Cause
  object Cause {

    // FIX-PRE-MERGE (KR) :

  }

}
