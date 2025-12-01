package oxygen.predef

/**
  * [[base]] is [[core]] before `oxygen-meta`
  */
object base {
  export oxygen.core.{___, unspecified, EitherNel, IndentedString, Ior, Lazy, PlatformCompat, Specified, ThrowableRepr, TypeTag}
  export oxygen.core.collection.{Growable, NonEmptyList}
  export oxygen.core.syntax.common.*
  export oxygen.core.typeclass.{EnumEncoding, EnumWithOther, SeqOps, SeqRead, SeqWrite, Show, StrictEnum, StringCodec, StringDecoder, StringEncoder, Zip}
  export scala.collection.immutable.ArraySeq
}
