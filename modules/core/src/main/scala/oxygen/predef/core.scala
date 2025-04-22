package oxygen.predef

object core {
  export oxygen.core.{EitherNel, Enum, IndentedString, TypeTag, Unapply}
  export oxygen.core.collection.NonEmptyList
  export oxygen.core.syntax.common.*
  export oxygen.core.typeclass.{StringCodec, StringDecoder, StringEncoder, Zip}
}
