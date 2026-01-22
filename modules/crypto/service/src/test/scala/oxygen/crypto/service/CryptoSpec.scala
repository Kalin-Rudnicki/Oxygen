package oxygen.crypto.service

import oxygen.predef.test.*
import scala.util.Try

object CryptoSpec extends OxygenSpecDefault {

  private object keys {

    val hmac1: CryptoKey.HS256 = CryptoKey.HS256.generate()
    val hmac2: CryptoKey.HS256 = CryptoKey.HS256.generate()

    val aes1: CryptoKey.AES = CryptoKey.AES.generate256()
    val aes2: CryptoKey.AES = CryptoKey.AES.generate256()

    val rsa1: CryptoKey.RSA.Pair = CryptoKey.RSA.Pair.generate3072()
    val rsa2: CryptoKey.RSA.Pair = CryptoKey.RSA.Pair.generate3072()

  }

  private val plainTextValue: String = "I am plain text!"
  private val otherValue: String = "On no! A hacker!"

  private object signing {

    def testSpec: TestSpec =
      suite("signing")(hmac, rsa)

    private def hmac: TestSpec =
      suite("hmac")(
        test("works") {
          val signature1 = keys.hmac1.signBase64(plainTextValue)
          val signature2 = keys.hmac2.signBase64(plainTextValue)
          assertTrue(
            keys.hmac1.validateBase64(plainTextValue, signature1),
            keys.hmac2.validateBase64(plainTextValue, signature2),
            !keys.hmac1.validateBase64(plainTextValue, signature2),
            !keys.hmac2.validateBase64(plainTextValue, signature1),
            !keys.hmac1.validateBase64(otherValue, signature1),
            !keys.hmac2.validateBase64(otherValue, signature2),
          )
        },
      )

    private def rsa: TestSpec =
      suite("rsa")(
        test("works") {
          val signature1 = keys.rsa1.privateKey.signBase64(plainTextValue)
          val signature2 = keys.rsa2.privateKey.signBase64(plainTextValue)
          assertTrue(
            keys.rsa1.publicKey.validateBase64(plainTextValue, signature1),
            keys.rsa2.publicKey.validateBase64(plainTextValue, signature2),
            !keys.rsa1.publicKey.validateBase64(plainTextValue, signature2),
            !keys.rsa2.publicKey.validateBase64(plainTextValue, signature1),
            !keys.rsa1.publicKey.validateBase64(otherValue, signature1),
            !keys.rsa2.publicKey.validateBase64(otherValue, signature2),
          )
        },
      )

  }

  private object encryption {

    def testSpec: TestSpec =
      suite("encryption")(aes, rsa)

    private def aes: TestSpec =
      suite("aes")(
        test("works") {
          val encrypted1 = keys.aes1.encrypt(plainTextValue)
          val encrypted2 = keys.aes2.encrypt(plainTextValue)
          assertTrue(
            keys.aes1.decrypt(encrypted1) == plainTextValue,
            keys.aes2.decrypt(encrypted2) == plainTextValue,
            Try { keys.aes2.decrypt(encrypted1) }.isFailure,
            Try { keys.aes1.decrypt(encrypted2) }.isFailure,
          )
        },
      )

    private def rsa: TestSpec =
      suite("rsa")(
        test("works") {
          val encrypted1 = keys.rsa1.publicKey.encrypt256(plainTextValue)
          val encrypted2 = keys.rsa2.publicKey.encrypt256(plainTextValue)
          assertTrue(
            keys.rsa1.privateKey.decrypt(encrypted1) == plainTextValue,
            keys.rsa2.privateKey.decrypt(encrypted2) == plainTextValue,
            Try { keys.rsa2.privateKey.decrypt(encrypted1) }.isFailure,
            Try { keys.rsa1.privateKey.decrypt(encrypted2) }.isFailure,
          )
        },
      )

  }

  override def testSpec: TestSpec =
    suite("CryptoSpec")(
      signing.testSpec,
      encryption.testSpec,
    )

}
