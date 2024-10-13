package oxygen.predef

object core {
  export oxygen.core.{EitherNel, Enum, IndentedString, NonEmptyList, TypeTag, Unapply}
  export oxygen.core.syntax.common.*
  export oxygen.core.typeclass.{StringCodec, StringDecoder, StringEncoder, Zip}
}
