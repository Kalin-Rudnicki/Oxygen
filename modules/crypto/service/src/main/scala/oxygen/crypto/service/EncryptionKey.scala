package oxygen.crypto.service

import java.nio.charset.StandardCharsets
import oxygen.crypto.model.*
import oxygen.json.{jsonDiscriminator, jsonType, JsonCodec}

@jsonDiscriminator("keyType")
sealed trait EncryptionKey derives JsonCodec { self: EncryptionKey.Symmetric | EncryptionKey.Asymmetric =>
  final def toUnion: EncryptionKey.Symmetric | EncryptionKey.Asymmetric = self
}
object EncryptionKey {

  /////// Categories ///////////////////////////////////////////////////////////////

  @jsonDiscriminator("keyType")
  sealed trait CanEncrypt extends EncryptionKey derives JsonCodec { self: EncryptionKey.Symmetric | EncryptionKey.Asymmetric =>
    def encrypt(value: String): EncryptedValue
  }
  object CanEncrypt {

    sealed trait Symmetric extends CanEncrypt { self: EncryptionKey.Symmetric =>
      override def encrypt(value: String): SymmetricEncryptedValue
    }

    sealed trait Asymmetric extends CanEncrypt { self: EncryptionKey.Asymmetric =>
      override def encrypt(value: String): AsymmetricEncryptedValue
    }

  }

  @jsonDiscriminator("keyType")
  sealed trait CanDecrypt extends EncryptionKey derives JsonCodec { self: EncryptionKey.Symmetric | EncryptionKey.Asymmetric =>
    def decrypt(value: EncryptedValue): String
  }
  object CanDecrypt {

    sealed trait Symmetric extends CanDecrypt { self: EncryptionKey.Symmetric =>
      def decryptSymmetric(value: SymmetricEncryptedValue): String
      override final def decrypt(value: EncryptedValue): String = value match
        case value: SymmetricEncryptedValue => decryptSymmetric(value)
        case _: AsymmetricEncryptedValue    => throw new RuntimeException("Symmetric encryption key is unable to decrypt asymmetrically encrypted value")
    }

    sealed trait Asymmetric extends CanDecrypt { self: EncryptionKey.Asymmetric =>
      def decryptAsymmetric(value: AsymmetricEncryptedValue): String
      override final def decrypt(value: EncryptedValue): String = value match
        case value: AsymmetricEncryptedValue => decryptAsymmetric(value)
        case _: SymmetricEncryptedValue      => throw new RuntimeException("Asymmetric encryption key is unable to decrypt symmetrically encrypted value")
    }

  }

  sealed trait Symmetric extends CanEncrypt.Symmetric, CanDecrypt.Symmetric

  sealed trait Asymmetric
  sealed trait AsymmetricPublic extends Asymmetric, CanEncrypt.Asymmetric
  sealed trait AsymmetricPrivate extends Asymmetric, CanDecrypt.Asymmetric
  sealed trait AsymmetricPair extends Asymmetric, CanEncrypt.Asymmetric, CanDecrypt.Asymmetric

  /////// Implementations ///////////////////////////////////////////////////////////////

  @jsonType("aes")
  final case class Aes(key: CryptoKey.AES) extends EncryptionKey.Symmetric {
    override def encrypt(value: String): SymmetricEncryptedValue = key.encrypt(value)
    override def decryptSymmetric(value: SymmetricEncryptedValue): String = key.decrypt(value)
  }

  @jsonType("rsa-public")
  final case class RsaPublic(publicKey: CryptoKey.RSA.Public) extends EncryptionKey.AsymmetricPublic {
    override def encrypt(value: String): AsymmetricEncryptedValue = publicKey.encrypt256(value)
  }

  @jsonType("rsa-private")
  final case class RsaPrivate(privateKey: CryptoKey.RSA.Private) extends EncryptionKey.AsymmetricPrivate {
    override def decryptAsymmetric(value: AsymmetricEncryptedValue): String = privateKey.decrypt(value)
  }

  @jsonType("rsa-pair")
  final case class RsaPair(publicKey: CryptoKey.RSA.Public, privateKey: CryptoKey.RSA.Private) extends EncryptionKey.AsymmetricPair {
    override def encrypt(value: String): AsymmetricEncryptedValue = publicKey.encrypt256(value)
    override def decryptAsymmetric(value: AsymmetricEncryptedValue): String = privateKey.decrypt(value)
  }
  object RsaPair {
    def pair(keys: CryptoKey.RSA.Pair): RsaPair = RsaPair(keys.publicKey, keys.privateKey)
  }

  @jsonType("none")
  case object Noop extends EncryptionKey.Symmetric {
    override def encrypt(value: String): SymmetricEncryptedValue =
      SymmetricEncryptedValue(SymmetricEncryptedValue.IV(new Array[Byte](0)), SymmetricEncryptedValue.Cipher.stringBytes(value))
    override def decryptSymmetric(value: SymmetricEncryptedValue): String =
      if value.iv.bytes.unwrap.length != 0 then throw new RuntimeException("Value was not encrypted using noop encryptor")
      else new String(value.cipher.bytes.unwrap, StandardCharsets.UTF_8)
  }

}
