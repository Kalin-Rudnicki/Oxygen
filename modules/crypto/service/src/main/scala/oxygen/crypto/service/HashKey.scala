package oxygen.crypto.service

import oxygen.crypto.model.*
import oxygen.json.{jsonDiscriminator, jsonType, JsonCodec}

@jsonDiscriminator("keyType")
sealed trait HashKey derives JsonCodec {
  val alg: JWTHeader.Alg
}
object HashKey {

  /////// Categories ///////////////////////////////////////////////////////////////

  @jsonDiscriminator("keyType")
  sealed trait CanSign extends HashKey derives JsonCodec {
    def signRaw(value: String): Signature.Raw
    final def signBase64(value: String): Signature.Base64 = signRaw(value).toBase64
  }

  @jsonDiscriminator("keyType")
  sealed trait CanValidate extends HashKey derives JsonCodec {
    def validateBase64(value: String, signature: Signature.Base64): Boolean
    def validateRaw(value: String, signature: Signature.Raw): Boolean
  }

  sealed trait Symmetric extends CanSign, CanValidate

  sealed trait Asymmetric
  sealed trait AsymmetricPublic extends Asymmetric, CanValidate
  sealed trait AsymmetricPrivate extends Asymmetric, CanSign
  sealed trait AsymmetricPair extends Asymmetric, CanSign, CanValidate

  /////// Implementations ///////////////////////////////////////////////////////////////

  @jsonType("hmac")
  final case class Hmac(key: CryptoKey.HS256) extends HashKey.Symmetric {
    override val alg: JWTHeader.Alg = JWTHeader.Alg.HS256
    override def signRaw(value: String): Signature.Raw = key.signRaw(value)
    override def validateRaw(value: String, signature: Signature.Raw): Boolean = key.validateRaw(value, signature)
    override def validateBase64(value: String, signature: Signature.Base64): Boolean = key.validateBase64(value, signature)
  }

  @jsonType("rsa-public")
  final case class RsaPublic(publicKey: CryptoKey.RSA.Public) extends HashKey.AsymmetricPublic {
    override val alg: JWTHeader.Alg = JWTHeader.Alg.RS256
    override def validateRaw(value: String, signature: Signature.Raw): Boolean = publicKey.validateRaw(value, signature)
    override def validateBase64(value: String, signature: Signature.Base64): Boolean = publicKey.validateBase64(value, signature)
  }

  @jsonType("rsa-private")
  final case class RsaPrivate(privateKey: CryptoKey.RSA.Private) extends HashKey.AsymmetricPrivate {
    override val alg: JWTHeader.Alg = JWTHeader.Alg.RS256
    override def signRaw(value: String): Signature.Raw = privateKey.signRaw(value)
  }

  @jsonType("rsa-pair")
  final case class RsaPair(publicKey: CryptoKey.RSA.Public, privateKey: CryptoKey.RSA.Private) extends HashKey.AsymmetricPair {
    override val alg: JWTHeader.Alg = JWTHeader.Alg.RS256
    override def signRaw(value: String): Signature.Raw = privateKey.signRaw(value)
    override def validateRaw(value: String, signature: Signature.Raw): Boolean = publicKey.validateRaw(value, signature)
    override def validateBase64(value: String, signature: Signature.Base64): Boolean = publicKey.validateBase64(value, signature)
  }
  object RsaPair {
    def pair(keys: CryptoKey.RSA.Pair): RsaPair = RsaPair(keys.publicKey, keys.privateKey)
  }

  @jsonType("none")
  case object Noop extends HashKey.Symmetric {
    override val alg: JWTHeader.Alg = JWTHeader.Alg.none
    override def signRaw(value: String): Signature.Raw = Signature.Raw(new Array[Byte](0))
    override def validateRaw(value: String, signature: Signature.Raw): Boolean = signature.bytes.unwrap.isEmpty
    override def validateBase64(value: String, signature: Signature.Base64): Boolean = signature.bytes.unwrap.isEmpty
  }

}
