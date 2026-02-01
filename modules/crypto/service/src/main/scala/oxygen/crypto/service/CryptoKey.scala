package oxygen.crypto.service

import java.nio.charset.StandardCharsets
import java.security.{KeyFactory, KeyPairGenerator, PrivateKey, PublicKey, Signature}
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import javax.crypto.{Cipher, KeyGenerator, Mac, SecretKey}
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}
import oxygen.core.typeclass.StringCodec
import oxygen.crypto.model.{AsymmetricEncryptedValue, KeyFormat, SymmetricEncryptedValue}
import oxygen.crypto.model.Signature as CryptoSignature
import oxygen.json.JsonCodec

object CryptoKey {

  final case class HS256 private (key: SecretKey) {

    def toDER: KeyFormat.DER = KeyFormat.DER(key.getEncoded)
    def toBase64: KeyFormat.Base64 = this.toDER.toBase64

    def signRaw(value: String): CryptoSignature.Raw = {
      val mac = Mac.getInstance("HmacSHA256")
      mac.init(key)
      CryptoSignature.Raw(mac.doFinal(value.getBytes(StandardCharsets.UTF_8)))
    }

    def signBase64(value: String): CryptoSignature.Base64 =
      signRaw(value).toBase64

    def validateRaw(value: String, signature: CryptoSignature.Raw): Boolean =
      validateBase64(value, signature.toBase64)

    def validateBase64(value: String, signature: CryptoSignature.Base64): Boolean =
      signBase64(value) == signature

  }
  object HS256 {

    def fromDER(key: KeyFormat.DER): HS256 =
      HS256(new SecretKeySpec(key.bytes, "HmacSHA256"))

    def fromPlain(key: String): HS256 =
      HS256.fromDER(KeyFormat.DER(key.getBytes(StandardCharsets.UTF_8)))

    def fromBase64(key: KeyFormat.Base64): HS256 =
      HS256.fromDER(key.toDER)

    def generate(): HS256 = {
      val kg = KeyGenerator.getInstance("HmacSHA256")
      HS256(kg.generateKey())
    }

    given stringCodec: StringCodec[CryptoKey.HS256] = KeyFormat.DER.stringCodec.transformCatchFail(HS256.fromDER, _.toDER)
    given jsonCodec: JsonCodec[CryptoKey.HS256] = JsonCodec.jsonStringUsingStringCodec

  }

  object RSA {

    final case class Private private[RSA] (key: PrivateKey) {

      def toDER: KeyFormat.DER = KeyFormat.DER(key.getEncoded)
      def toBase64: KeyFormat.Base64 = this.toDER.toBase64
      def toPKCS8: KeyFormat.PKCS8PEM = this.toDER.toBase64.toPKCS8

      def signRaw(value: String): CryptoSignature.Raw = {
        val signature = Signature.getInstance("SHA256withRSA")
        signature.initSign(key)
        signature.update(value.getBytes(StandardCharsets.UTF_8))
        CryptoSignature.Raw(signature.sign())
      }

      def signBase64(value: String): CryptoSignature.Base64 =
        signRaw(value).toBase64

      def unwrapKey(wrappedSymKey: AsymmetricEncryptedValue.WrappedKey): CryptoKey.AES = {
        val cipher = Cipher.getInstance("RSA/ECB/OAEPWithSHA-256AndMGF1Padding")
        cipher.init(Cipher.UNWRAP_MODE, key)
        val rawKey: SecretKey = cipher.unwrap(wrappedSymKey.bytes, "AES", Cipher.SECRET_KEY).asInstanceOf[SecretKey]
        CryptoKey.AES(rawKey)
      }

      def decrypt(value: AsymmetricEncryptedValue): String = {
        val symKey: CryptoKey.AES = unwrapKey(value.key)
        symKey.decrypt(value.symmetric)
      }

    }
    object Private {

      def fromDER(key: KeyFormat.DER): RSA.Private = {
        val factory = KeyFactory.getInstance("RSA")
        val spec = new PKCS8EncodedKeySpec(key.bytes)
        RSA.Private(factory.generatePrivate(spec))
      }

      def fromBase64(key: KeyFormat.Base64): RSA.Private =
        RSA.Private.fromDER(key.toDER)

      def fromPKCS8(key: KeyFormat.PKCS8PEM): RSA.Private =
        RSA.Private.fromDER(key.toBase64.toDER)

      given stringCodec: StringCodec[RSA.Private] = KeyFormat.PKCS8PEM.stringCodec.transformCatchFail(RSA.Private.fromPKCS8, _.toPKCS8)
      given jsonCodec: JsonCodec[RSA.Private] = JsonCodec.jsonStringUsingStringCodec

    }

    final case class Public private[RSA] (key: PublicKey) {

      def toDER: KeyFormat.DER = KeyFormat.DER(key.getEncoded)
      def toBase64: KeyFormat.Base64 = this.toDER.toBase64
      def toX509: KeyFormat.X509PEM = this.toDER.toBase64.toX509

      def validateRaw(value: String, signature: CryptoSignature.Raw): Boolean = {
        val sigInst = Signature.getInstance("SHA256withRSA")
        sigInst.initVerify(key)
        sigInst.update(value.getBytes(StandardCharsets.UTF_8))
        sigInst.verify(signature.bytes)
      }

      def validateBase64(value: String, signature: CryptoSignature.Base64): Boolean =
        validateRaw(value, signature.toRaw)

      def wrapKey(aesKey: CryptoKey.AES): AsymmetricEncryptedValue.WrappedKey = {
        val cipher = Cipher.getInstance("RSA/ECB/OAEPWithSHA-256AndMGF1Padding")
        cipher.init(Cipher.WRAP_MODE, key)
        val wrappedBytes: Array[Byte] = cipher.wrap(aesKey.key)
        AsymmetricEncryptedValue.WrappedKey(wrappedBytes)
      }

      def encrypt256(value: String): AsymmetricEncryptedValue = {
        val symKey: CryptoKey.AES = CryptoKey.AES.generate256()
        encrypt(value, symKey)
      }
      def encrypt192(value: String): AsymmetricEncryptedValue = {
        val symKey: CryptoKey.AES = CryptoKey.AES.generate192()
        encrypt(value, symKey)
      }
      def encrypt128(value: String): AsymmetricEncryptedValue = {
        val symKey: CryptoKey.AES = CryptoKey.AES.generate128()
        encrypt(value, symKey)
      }

      private def encrypt(value: String, symKey: CryptoKey.AES): AsymmetricEncryptedValue = {
        val symOut: SymmetricEncryptedValue = symKey.encrypt(value)
        val wrappedSymKey: AsymmetricEncryptedValue.WrappedKey = wrapKey(symKey)
        symOut.asymmetric(wrappedSymKey)
      }

    }
    object Public {

      def fromDER(key: KeyFormat.DER): RSA.Public = {
        val factory = KeyFactory.getInstance("RSA")
        val spec = new X509EncodedKeySpec(key.bytes)
        RSA.Public(factory.generatePublic(spec))
      }

      def fromBase64(key: KeyFormat.Base64): RSA.Public =
        RSA.Public.fromDER(key.toDER)

      def fromX509(key: KeyFormat.X509PEM): RSA.Public =
        RSA.Public.fromDER(key.toBase64.toDER)

      given stringCodec: StringCodec[RSA.Public] = KeyFormat.X509PEM.stringCodec.transformCatchFail(RSA.Public.fromX509, _.toX509)
      given jsonCodec: JsonCodec[RSA.Public] = JsonCodec.jsonStringUsingStringCodec

    }

    final case class Pair(privateKey: RSA.Private, publicKey: RSA.Public)
    object Pair {

      def fromDER(privateKey: KeyFormat.DER, publicKey: KeyFormat.DER): RSA.Pair =
        RSA.Pair(RSA.Private.fromDER(privateKey), RSA.Public.fromDER(publicKey))

      def fromBase64(privateKey: KeyFormat.Base64, publicKey: KeyFormat.Base64): RSA.Pair =
        RSA.Pair(RSA.Private.fromBase64(privateKey), RSA.Public.fromBase64(publicKey))

      def fromPEM(privateKey: KeyFormat.PKCS8PEM, publicKey: KeyFormat.X509PEM): RSA.Pair =
        RSA.Pair(RSA.Private.fromPKCS8(privateKey), RSA.Public.fromX509(publicKey))

      def generate3072(): RSA.Pair =
        RSA.Pair.generate(3072)

      private def generate(bits: Int): RSA.Pair = {
        val kpg = KeyPairGenerator.getInstance("RSA")
        kpg.initialize(bits)
        val kp = kpg.generateKeyPair()
        Pair(RSA.Private(kp.getPrivate), RSA.Public(kp.getPublic))
      }

      given jsonCodec: JsonCodec[RSA.Pair] = JsonCodec.derived

    }

  }

  final case class AES private[CryptoKey] (key: SecretKey) {

    def toDER: KeyFormat.DER = KeyFormat.DER(key.getEncoded)
    def toBase64: KeyFormat.Base64 = this.toDER.toBase64

    private def initCipher(iv: SymmetricEncryptedValue.IV, mode: Int): Cipher = {
      val cipher = Cipher.getInstance("AES/GCM/NoPadding")
      val gcmSpec = new GCMParameterSpec(128, iv.bytes)
      cipher.init(mode, key, gcmSpec)
      cipher
    }

    def encrypt(value: String): SymmetricEncryptedValue = {
      val iv: SymmetricEncryptedValue.IV = SymmetricEncryptedValue.IV.generate()
      val cipher = initCipher(iv, Cipher.ENCRYPT_MODE)
      val decryptedBytes: Array[Byte] = value.getBytes(StandardCharsets.UTF_8)
      val encryptedBytes: Array[Byte] = cipher.doFinal(decryptedBytes)
      SymmetricEncryptedValue(iv, SymmetricEncryptedValue.Cipher(encryptedBytes))
    }

    def decrypt(value: SymmetricEncryptedValue): String = {
      val cipher = initCipher(value.iv, Cipher.DECRYPT_MODE)
      val encryptedBytes: Array[Byte] = value.cipher.bytes
      val decryptedBytes: Array[Byte] = cipher.doFinal(encryptedBytes)
      new String(decryptedBytes, StandardCharsets.UTF_8)
    }

  }
  object AES {

    def fromDER(key: KeyFormat.DER): AES =
      AES(new SecretKeySpec(key.bytes, "AES"))

    def fromBase64(key: KeyFormat.Base64): AES =
      AES.fromDER(key.toDER)

    def generate128(): AES = generate(128)
    def generate192(): AES = generate(192)
    def generate256(): AES = generate(256)

    private def generate(bits: Int): AES = {
      val kg = KeyGenerator.getInstance("AES")
      kg.init(bits)
      AES(kg.generateKey())
    }

    given stringCodec: StringCodec[CryptoKey.AES] = KeyFormat.DER.stringCodec.transformCatchFail(AES.fromDER, _.toDER)
    given jsonCodec: JsonCodec[CryptoKey.AES] = JsonCodec.jsonStringUsingStringCodec

  }

}
