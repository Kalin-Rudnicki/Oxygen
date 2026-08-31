package oxygen.crypto.model

import oxygen.core.ConversionUtils
import oxygen.json.{JsonDecoder, JsonEncoder}
import oxygen.schema.{JsonSchema, PlainTextSchema}

/**
  * Base for a **typed JWT**: a domain wrapper around a `JWT[A]` whose payload is a specific claim type `A`.
  *
  * Concrete tokens follow the same idiom as `CurrencyClass` — an `object` that `extends TypedJWT[Claims]`,
  * plus a top-level alias exposing the opaque member:
  * {{{
  *   type MyToken = MyToken.Type
  *   object MyToken extends TypedJWT[MyClaims]
  * }}}
  * so `MyToken` names the opaque token type and `MyToken.<…>` constructs / decodes it.
  *
  * Requires a `JsonSchema[A]` for the payload (from which both the encoder used to sign and the decoder used
  * to validate are drawn).
  *
  * NOTE: this lives in `oxygen-schema` (not `oxygen-crypto-model`) because it needs `PlainTextSchema` /
  * `JsonSchema`, and `oxygen-schema` depends on `oxygen-crypto-model` (not the reverse). The package is kept
  * as `oxygen.crypto.model` deliberately, so the typed-JWT machinery reads as one namespace with `JWT` /
  * `JWTHeaderAndPayload` even though it is compiled in the schema module.
  */
abstract class TypedJWT[A](using schema: JsonSchema[A]) {

  private given jsonEncoder: JsonEncoder[A] = schema.jsonEncoder
  private given jsonDecoder: JsonDecoder[A] = schema.jsonDecoder

  /**
    * The typed JWT itself (a `JWT[A]`) — the ergonomic "work with it" type, and the one to reach for when the
    * HTTP auth header format is already well-documented / fixed. When you need to pin an *exact* wire
    * encode/decode, use the granular [[Bearer]] / [[Token]] / [[BearerOrToken]] / [[TokenOrBearer]] variants.
    * Exposed by the extender as `type X = X.Type`.
    */
  final opaque type Type = JWT[A]
  object Type {

    def apply(jwt: JWT[A]): Type = jwt

    extension (self: Type) {

      /** The raw underlying `JWT[A]`. */
      def toJWT: JWT[A] = self

      /** The decoded claim payload. */
      def jwtPayload: A = self.payload

      /** The signed [[BearerToken]] — call `.bearer` for `Authorization: Bearer …`, or `.token` for the bare form. */
      def jwtBearer: BearerToken = self.token
    }

    def fromBearerToken(token: BearerToken): Either[String, Type] = JWT.decode[A](token).map(apply)
    def decodeBearer(value: String): Either[String, Type] = JWT.decodeBearer[A](value).map(apply)
    def decodeToken(value: String): Either[String, Type] = JWT.decodeToken[A](value).map(apply)
    def decodeBearerOrToken(value: String): Either[String, Type] = JWT.decodeBearerOrToken[A](value).map(apply)

    /** Assemble a token from its signed [[HeaderAndPayload]] (signature from `Signer.signBase64`). */
    def headerPayloadSignature(hap: HeaderAndPayload, signature: Signature.Base64): Type =
      apply(hap.withSignature(signature)) // HeaderAndPayload <: JWTHeaderAndPayload[A], so `withSignature` is in reach

    /**
      * The one HTTP-optimized schema — leverages `PlainTextSchema.jwt` (standard `Authorization: Bearer …`).
      * The granular variants below are, by contrast, plain `PlainTextSchema.string` transforms.
      */
    given schema: PlainTextSchema[Type] = PlainTextSchema.jwt[A].transform(apply, _.toJWT)

  }

  /** The base64url header+payload for THIS token type, ready to sign via `Signer.signBase64`. */
  final opaque type HeaderAndPayload <: JWTHeaderAndPayload[A] = JWTHeaderAndPayload[A]
  object HeaderAndPayload {
    def calculate(header: JWTHeader, payload: A): HeaderAndPayload = JWTHeaderAndPayload.calculate(header, payload)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Granular wire variants — pin an exact decode/encode for schema-driven (de)serialization.
  //      Each is `<: Type = Type` (so it IS a `Type`), with `decode` + an identity `Conversion[Type, _]`.
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /** Decodes AND encodes the strict `Bearer <header>.<payload>.<signature>` form. */
  final opaque type Bearer <: Type = Type
  object Bearer {
    def apply(value: Type): Bearer = value
    def decode(value: String): Either[String, Bearer] = Type.decodeBearer(value)
    given Conversion[Type, Bearer] = ConversionUtils.id
    given schema: PlainTextSchema[Bearer] = PlainTextSchema.string.transformOrFail(decode, Type.jwtBearer(_).bearer)
  }

  /** Decodes AND encodes the strict bare `<header>.<payload>.<signature>` token form (no `Bearer ` prefix). */
  final opaque type Token <: Type = Type
  object Token {
    def apply(value: Type): Token = value
    def decode(value: String): Either[String, Token] = Type.decodeToken(value)
    given Conversion[Type, Token] = ConversionUtils.id
    given schema: PlainTextSchema[Token] = PlainTextSchema.string.transformOrFail(decode, Type.jwtBearer(_).token)
  }

  // Naming convention for the "decode either" pair: BOTH decode either wire form; each ENCODES to the form
  // named FIRST — `BearerOrToken` encodes `Bearer …`, `TokenOrBearer` encodes the bare token.

  /** Decodes EITHER wire form; encodes the FIRST-named form → `Bearer <header>.<payload>.<signature>`. */
  final opaque type BearerOrToken <: Type = Type
  object BearerOrToken {
    def apply(value: Type): BearerOrToken = value
    def decode(value: String): Either[String, BearerOrToken] = Type.decodeBearerOrToken(value)
    given Conversion[Type, BearerOrToken] = ConversionUtils.id
    given schema: PlainTextSchema[BearerOrToken] = PlainTextSchema.string.transformOrFail(decode, Type.jwtBearer(_).bearer)
  }

  /** Decodes EITHER wire form; encodes the FIRST-named form → the bare `<header>.<payload>.<signature>` token. */
  final opaque type TokenOrBearer <: Type = Type
  object TokenOrBearer {
    def apply(value: Type): TokenOrBearer = value
    def decode(value: String): Either[String, TokenOrBearer] = Type.decodeBearerOrToken(value)
    given Conversion[Type, TokenOrBearer] = ConversionUtils.id
    given schema: PlainTextSchema[TokenOrBearer] = PlainTextSchema.string.transformOrFail(decode, Type.jwtBearer(_).token)
  }

}
