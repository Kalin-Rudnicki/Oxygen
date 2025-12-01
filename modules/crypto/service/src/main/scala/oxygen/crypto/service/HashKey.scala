package oxygen.crypto.service

import java.nio.charset.StandardCharsets
import java.security.*
import javax.crypto.{Mac, SecretKey}
import javax.crypto.spec.SecretKeySpec
import oxygen.crypto.model.*
import oxygen.json.jsonDiscriminator
import oxygen.predef.core.*
import oxygen.predef.json.*

object HashKey {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Impls
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class HS256(base64Key: String) extends HashKey.Symmetric {

    override protected val alg: JWTHeader.Alg = JWTHeader.Alg.HS256

    private lazy val secretKey: SecretKey =
      new SecretKeySpec(Base64.stdDecoder.decode(base64Key), "HmacSHA256")

    override protected def issueSignature(headerAndPayload: String): String = {
      val mac = Mac.getInstance("HmacSHA256")
      mac.init(secretKey)
      val signatureBytes = mac.doFinal(headerAndPayload.getBytes(StandardCharsets.UTF_8))
      Base64.urlEncoder.encodeToString(signatureBytes)
    }

  }
  object HS256 {
    def textKey(keyText: String): HS256 = HS256(Base64.stdEncoder.encodeToString(keyText.getBytes(StandardCharsets.UTF_8)))
  }

  object RS256 {

    def cleanKey(keyText: String): Array[Byte] = {
      var texts: Seq[String] = keyText.split('\n').toSeq

      while texts.nonEmpty && (texts.head.isEmpty || texts.head.startsWith("-----")) do texts = texts.tail
      while texts.nonEmpty && (texts.last.isEmpty || texts.last.startsWith("-----")) do texts = texts.init

      Base64.stdDecoder.decode(texts.mkString)
    }

    private def makePrivateKey(privateKey: String): PrivateKey = {
      val keySpec = new java.security.spec.PKCS8EncodedKeySpec(cleanKey(privateKey))
      val keyFactory = KeyFactory.getInstance("RSA")
      keyFactory.generatePrivate(keySpec)
    }
    private def makePublicKey(publicKey: String): PublicKey = {
      val keySpec = new java.security.spec.X509EncodedKeySpec(cleanKey(publicKey))
      val keyFactory = KeyFactory.getInstance("RSA")
      keyFactory.generatePublic(keySpec)
    }

    private def validateSignature(publicKey: PublicKey)(headerAndPayload: String, signatureToValidate: String): Boolean = {
      val signature = Signature.getInstance("SHA256withRSA")
      signature.initVerify(publicKey)
      signature.update(headerAndPayload.getBytes(StandardCharsets.UTF_8))
      val decodedSignature = java.util.Base64.getUrlDecoder.decode(signatureToValidate)
      signature.verify(decodedSignature)
    }

    /**
      * @param privateKey Accepts key in PKCS8 format. Headers/footer/newlines are optional, and will be properly stripped if provided.
      * @param publicKey Accepts x509 format. Headers/footer/newlines are optional, and will be properly stripped if provided.
      */
    final case class Private(privateKey: String, publicKey: String) extends HashKey.Asymmetric.Private {

      private lazy val builtPrivateKey: PrivateKey = makePrivateKey(privateKey)
      private lazy val builtPublicKey: PublicKey = makePublicKey(publicKey)

      override protected val alg: JWTHeader.Alg = JWTHeader.Alg.RS256

      override protected def issueSignature(headerAndPayload: String): String = {
        val signature = Signature.getInstance("SHA256withRSA")
        signature.initSign(builtPrivateKey)
        signature.update(headerAndPayload.getBytes(StandardCharsets.UTF_8))
        val signedBytes = signature.sign()
        Base64.urlEncoder.encodeToString(signedBytes)
      }

      override protected def validateSignature(headerAndPayload: String, signatureToValidate: String): Boolean =
        RS256.validateSignature(builtPublicKey)(headerAndPayload, signatureToValidate)

    }

    /**
      * @param publicKey Accepts x509 format. Headers/footer/newlines are optional, and will be properly stripped if provided.
      */
    final case class Public(publicKey: String) extends HashKey.Asymmetric.Public {

      override protected val alg: JWTHeader.Alg = JWTHeader.Alg.RS256

      private lazy val builtPublicKey: PublicKey = makePublicKey(publicKey)

      override protected def validateSignature(headerAndPayload: String, signatureToValidate: String): Boolean =
        RS256.validateSignature(builtPublicKey)(headerAndPayload, signatureToValidate)

    }

  }

  case object none extends HashKey.Symmetric {

    override protected val alg: JWTHeader.Alg = JWTHeader.Alg.none

    override protected def issueSignature(headerAndPayload: String): String = ""

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Types
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait CanValidate {

    protected val alg: JWTHeader.Alg
    protected def validateSignature(headerAndPayload: String, signatureToValidate: String): Boolean

    final def validate(token: BearerToken): Either[JWTError, Unit] =
      if token.header.alg != alg then JWTError.InvalidAlgorithm(alg, token.header.alg).asLeft
      else if !validateSignature(token.`headerBase64.payloadBase64`, token.signatureBase64) then JWTError.InvalidSignature.asLeft
      else ().asRight

  }
  object CanValidate {

    @jsonDiscriminator("alg")
    enum Config(final val secret: HashKey.CanValidate) derives JsonCodec {
      case HS256(base64Key: String) extends Config(HashKey.HS256(base64Key))
      case RS256(publicKey: String) extends Config(HashKey.RS256.Public(publicKey))
      case none extends Config(HashKey.none)
    }

  }

  sealed trait CanIssue extends CanValidate {

    protected def issueSignature(headerAndPayload: String): String

    final def issue(payload: String): BearerToken = {
      val header = alg.toJWTHeader
      val headerBase64 = Base64.urlEncoder.encodeToString(header.toJsonStringCompact.getBytes)
      val payloadBase64 = Base64.urlEncoder.encodeToString(payload.getBytes)

      BearerToken(
        headerBase64 = headerBase64,
        header = header,
        payloadBase64 = payloadBase64,
        payload = payload,
        signatureBase64 = issueSignature(s"$headerBase64.$payloadBase64"),
      )
    }

  }
  object CanIssue {

    @jsonDiscriminator("alg")
    enum Config(final val secret: HashKey.CanIssue) derives JsonCodec {
      case HS256(base64Key: String) extends Config(HashKey.HS256(base64Key))
      case RS256(privateKey: String, publicKey: String) extends Config(HashKey.RS256.Private(privateKey, publicKey))
      case none extends Config(HashKey.none)
    }

  }

  trait Symmetric extends HashKey.CanIssue, HashKey.CanValidate {

    override protected final def validateSignature(headerAndPayload: String, signatureToValidate: String): Boolean =
      issueSignature(headerAndPayload) == signatureToValidate

  }

  sealed trait Asymmetric extends HashKey.CanValidate
  object Asymmetric {
    trait Public extends HashKey.Asymmetric
    trait Private extends HashKey.Asymmetric, HashKey.CanIssue
  }

}
