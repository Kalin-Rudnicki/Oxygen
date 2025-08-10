package oxygen.predef

object core {
  export oxygen.core.{EitherNel, Enum, IndentedString, Ior, Lazy, Specified, ThrowableRepr, TypeTag}
  export oxygen.core.collection.{Contiguous, Growable, NonEmptyList}
  export oxygen.core.idToSpecified
  export oxygen.core.syntax.common.*
  export oxygen.core.typeclass.{SeqOps, Show, StringCodec, StringDecoder, StringEncoder, Zip}
}
