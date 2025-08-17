package oxygen.schema.compiled

import oxygen.core.TypeTag
import scala.annotation.tailrec

sealed trait TypeRef {

  @tailrec
  final def concrete: TypeRef.Concrete = this match
    case concrete: TypeRef.Concrete     => concrete
    case TypeRef.JsonArray(underlying)  => underlying.concrete
    case TypeRef.JsonOption(underlying) => underlying.concrete

}
object TypeRef {

  sealed trait Concrete extends TypeRef {
    val ref: TypeTag[?]
  }

  sealed trait PlainRef extends TypeRef

  final case class ConcretePlainRef(ref: TypeTag[?]) extends PlainRef, Concrete

  // A | Option[A] | Array[A] | recursive
  sealed trait JsonRef extends TypeRef {

    // TODO (KR) : [OXY-70] improve representation of missing vs. null values
    final def toRequired: (RequiredJsonRef, Boolean) = this match
      case ref: RequiredJsonRef   => (ref, true)
      case JsonOption(underlying) => (underlying, false)

  }

  // A | Array[A] | recursive
  sealed trait RequiredJsonRef extends JsonRef

  // A
  final case class ConcreteJsonRef(ref: TypeTag[?]) extends RequiredJsonRef, Concrete

  // Array[A]
  final case class JsonArray(underlying: JsonRef) extends RequiredJsonRef

  // Option[A]
  final case class JsonOption(underlying: RequiredJsonRef) extends JsonRef

}
