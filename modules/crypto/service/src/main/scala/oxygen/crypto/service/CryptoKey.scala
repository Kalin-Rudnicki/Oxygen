package oxygen.crypto.service

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.{AlgorithmParameters, KeyFactory, KeyPairGenerator, PrivateKey, PublicKey, Signature}
import java.security.interfaces.{ECPrivateKey, ECPublicKey}
import java.security.spec.{ECGenParameterSpec, ECParameterSpec, ECPoint, ECPublicKeySpec, PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.util.Base64
import javax.crypto.{Cipher, KeyGenerator, Mac, SecretKey}
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}
import oxygen.core.typeclass.StringCodec
import oxygen.crypto.model.{AsymmetricEncryptedValue, JWTHeader, KeyFormat, SymmetricEncryptedValue}
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
    given jsonCodec: JsonCodec[CryptoKey.HS256] = JsonCodec.jsonStringUsingStringCodec[CryptoKey.HS256].secret

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
      given jsonCodec: JsonCodec[RSA.Private] = JsonCodec.jsonStringUsingStringCodec[RSA.Private].secret

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
      given jsonCodec: JsonCodec[RSA.Public] = JsonCodec.jsonStringUsingStringCodec[RSA.Public].secret

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

  /**
    * Elliptic-curve keys for the JOSE `ES256` / `ES384` / `ES512` algorithms (P-256 / P-384 / P-521).
    *
    * The hash and coordinate size are derived from the key's curve, so a P-256 key validates ES256, etc.
    * JOSE signatures are the raw fixed-width `R‖S` concatenation (RFC 7515 A.3), whereas Java's ECDSA
    * `Signature` speaks ASN.1 DER — so we convert on the boundary in both directions.
    */
  object EC {

    private def curveStdName(crv: String): String = crv match
      case "P-256" => "secp256r1"
      case "P-384" => "secp384r1"
      case "P-521" => "secp521r1"
      case other   => throw new IllegalArgumentException(s"unsupported EC curve: $other")

    private def paramsFor(stdName: String): ECParameterSpec = {
      val ap = AlgorithmParameters.getInstance("EC")
      ap.init(new ECGenParameterSpec(stdName))
      ap.getParameterSpec(classOf[ECParameterSpec])
    }

    private def coordLen(fieldBits: Int): Int = (fieldBits + 7) / 8

    private def shaName(fieldBits: Int): String = fieldBits match
      case 256 => "SHA256"
      case 384 => "SHA384"
      case 521 => "SHA512"
      case n   => throw new IllegalArgumentException(s"unsupported EC field size: $n")

    private def algFor(fieldBits: Int): JWTHeader.Alg = fieldBits match
      case 256 => JWTHeader.Alg.ES256
      case 384 => JWTHeader.Alg.ES384
      case 521 => JWTHeader.Alg.ES512
      case n   => throw new IllegalArgumentException(s"unsupported EC field size: $n")

    // ---- JOSE raw R‖S  <->  ASN.1 DER SEQUENCE{INTEGER r, INTEGER s} ----

    private def derLen(n: Int): Array[Byte] =
      if n < 0x80 then Array[Byte](n.toByte)
      else if n < 0x100 then Array[Byte](0x81.toByte, n.toByte)
      else Array[Byte](0x82.toByte, (n >> 8).toByte, (n & 0xff).toByte)

    private def derInt(raw: Array[Byte]): Array[Byte] = {
      val stripped = raw.dropWhile(_ == 0.toByte)
      val body = if stripped.isEmpty then Array[Byte](0) else stripped
      val signed = if (body(0) & 0x80) != 0 then Array[Byte](0) ++ body else body
      Array[Byte](0x02.toByte) ++ derLen(signed.length) ++ signed
    }

    /** raw `R‖S` (2 × coordLen) -> DER, for handing a JOSE signature to Java's verifier. */
    private def rawToDer(raw: Array[Byte], cl: Int): Array[Byte] = {
      val content = derInt(raw.slice(0, cl)) ++ derInt(raw.slice(cl, cl * 2))
      Array[Byte](0x30.toByte) ++ derLen(content.length) ++ content
    }

    private def leftPad(b: Array[Byte], len: Int): Array[Byte] =
      if b.length >= len then b.takeRight(len)
      else Array.fill[Byte](len - b.length)(0.toByte) ++ b

    /** DER SEQUENCE{INTEGER,INTEGER} -> raw `R‖S`, for turning Java's signature into a JOSE one. */
    private def derToRaw(der: Array[Byte], cl: Int): Array[Byte] = {
      var i = 1 // skip 0x30 SEQUENCE tag
      val seqLen0 = der(i) & 0xff
      i += (if seqLen0 < 0x80 then 1 else 1 + (seqLen0 & 0x7f)) // skip SEQUENCE length octets
      i += 1 // skip 0x02 INTEGER tag (r)
      val rLen = der(i) & 0xff; i += 1
      val r = der.slice(i, i + rLen); i += rLen
      i += 1 // skip 0x02 INTEGER tag (s)
      val sLen = der(i) & 0xff; i += 1
      val s = der.slice(i, i + sLen)
      leftPad(r.dropWhile(_ == 0.toByte), cl) ++ leftPad(s.dropWhile(_ == 0.toByte), cl)
    }

    final case class Public private[EC] (key: PublicKey) {

      private def fieldBits: Int = key.asInstanceOf[ECPublicKey].getParams.getCurve.getField.getFieldSize

      /** The JOSE `alg` this key verifies, derived from its curve (P-256 -> ES256, ...). */
      def alg: JWTHeader.Alg = algFor(fieldBits)

      def toDER: KeyFormat.DER = KeyFormat.DER(key.getEncoded)
      def toBase64: KeyFormat.Base64 = this.toDER.toBase64
      def toX509: KeyFormat.X509PEM = this.toDER.toBase64.toX509

      def validateRaw(value: String, signature: CryptoSignature.Raw): Boolean = {
        val fb = fieldBits
        val sigInst = Signature.getInstance(s"${shaName(fb)}withECDSA")
        sigInst.initVerify(key)
        sigInst.update(value.getBytes(StandardCharsets.UTF_8))
        sigInst.verify(rawToDer(signature.bytes, coordLen(fb)))
      }

      def validateBase64(value: String, signature: CryptoSignature.Base64): Boolean =
        validateRaw(value, signature.toRaw)

    }
    object Public {

      def fromDER(key: KeyFormat.DER): EC.Public = {
        val factory = KeyFactory.getInstance("EC")
        EC.Public(factory.generatePublic(new X509EncodedKeySpec(key.bytes)))
      }

      def fromX509(key: KeyFormat.X509PEM): EC.Public =
        EC.Public.fromDER(key.toBase64.toDER)

      /** Build a public key from a JWK's `crv`/`x`/`y` (base64url big-endian coordinates). */
      def fromJWK(crv: String, x: String, y: String): EC.Public = {
        val params = paramsFor(curveStdName(crv))
        val dec = Base64.getUrlDecoder
        val point = new ECPoint(new BigInteger(1, dec.decode(x)), new BigInteger(1, dec.decode(y)))
        EC.Public(KeyFactory.getInstance("EC").generatePublic(new ECPublicKeySpec(point, params)))
      }

      given stringCodec: StringCodec[EC.Public] = KeyFormat.X509PEM.stringCodec.transformCatchFail(EC.Public.fromX509, _.toX509)
      given jsonCodec: JsonCodec[EC.Public] = JsonCodec.jsonStringUsingStringCodec[EC.Public].secret

    }

    final case class Private private[EC] (key: PrivateKey) {

      private def fieldBits: Int = key.asInstanceOf[ECPrivateKey].getParams.getCurve.getField.getFieldSize

      /** Produces a JOSE raw `R‖S` signature (not DER) — the form a JWT carries. */
      def signRaw(value: String): CryptoSignature.Raw = {
        val fb = fieldBits
        val sigInst = Signature.getInstance(s"${shaName(fb)}withECDSA")
        sigInst.initSign(key)
        sigInst.update(value.getBytes(StandardCharsets.UTF_8))
        CryptoSignature.Raw(derToRaw(sigInst.sign(), coordLen(fb)))
      }

      def signBase64(value: String): CryptoSignature.Base64 = signRaw(value).toBase64

    }

    final case class Pair(privateKey: EC.Private, publicKey: EC.Public)
    object Pair {

      /** Generate a fresh EC key pair on the given curve (`P-256` by default). */
      def generate(crv: String = "P-256"): EC.Pair = {
        val kpg = KeyPairGenerator.getInstance("EC")
        kpg.initialize(new ECGenParameterSpec(curveStdName(crv)))
        val kp = kpg.generateKeyPair()
        EC.Pair(EC.Private(kp.getPrivate), EC.Public(kp.getPublic))
      }

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
    given jsonCodec: JsonCodec[CryptoKey.AES] = JsonCodec.jsonStringUsingStringCodec[CryptoKey.AES].secret

  }

}
