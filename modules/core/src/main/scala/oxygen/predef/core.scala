package oxygen.predef

object core {
  export oxygen.core.{EitherNel, Enum, IndentedString, Specified, ThrowableRepr, TypeTag, Unapply}
  export oxygen.core.collection.{Contiguous, Growable, NonEmptyList}
  export oxygen.core.idToSpecified
  export oxygen.core.syntax.common.*
  export oxygen.core.typeclass.{SeqOps, StringCodec, StringDecoder, StringEncoder, Zip}
}
