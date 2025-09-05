package oxygen.predef

object core {
  export oxygen.core.{aToBSpecified, idToSpecified}
  export oxygen.core.{EitherNel, Enum, IndentedString, Ior, Lazy, PlatformCompat, Specified, ThrowableRepr, TypeTag}
  export oxygen.core.collection.{Growable, NonEmptyList}
  export oxygen.core.syntax.common.*
  export oxygen.core.typeclass.{SeqOps, SeqRead, SeqWrite, Show, StringCodec, StringDecoder, StringEncoder, Zip}
  export scala.collection.immutable.ArraySeq
}
