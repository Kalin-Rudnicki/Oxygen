package oxygen.crypto.service

import oxygen.crypto.model.JWTHeader
import oxygen.crypto.service.ExampleServices.bearerTokenServices.*
import oxygen.predef.test.*

object BearerTokenServiceSpec extends OxygenSpecDefault {

  override def testSpec: TestSpec =
    suite("JWTServiceSpec")(
      suite("none")(
        test("can issue and validate its own token - 1") {
          for {
            token <- none.issueToken(Person.example1.jsonString)
            exit <- none.validateToken(token).exit
          } yield assert(exit)(succeeds(equalTo(())))
        },
      ),
      suite("hmac")(
        test("can issue and validate its own token - 1") {
          for {
            token <- hmac1.issueToken(Person.example1.jsonString)
            exit <- hmac1.validateToken(token).exit
          } yield assert(exit)(succeeds(equalTo(())))
        },
        test("can issue and validate its own token - 2") {
          for {
            token <- hmac2.issueToken(Person.example1.jsonString)
            exit <- hmac2.validateToken(token).exit
          } yield assert(exit)(succeeds(equalTo(())))
        },
        test("detects invalid signature") {
          for {
            token1 <- hmac1.issueToken(Person.example1.jsonString)
            token2 <- hmac1.issueToken(Person.example2.jsonString)
            modifiedToken1 = token1.copy(payloadBase64 = token2.payloadBase64, payload = token2.payload)
            exit <- hmac1.validateToken(modifiedToken1).exit
          } yield assert(exit)(fails(equalTo(TokenError.InvalidSignature)))
        },
        test("fails with invalid key - 1") {
          for {
            token <- hmac1.issueToken(Person.example1.jsonString)
            exit <- hmac2.validateToken(token).exit
          } yield assert(exit)(fails(equalTo(TokenError.InvalidSignature)))
        },
        test("fails with invalid key - 2") {
          for {
            token <- hmac2.issueToken(Person.example1.jsonString)
            exit <- hmac1.validateToken(token).exit
          } yield assert(exit)(fails(equalTo(TokenError.InvalidSignature)))
        },
      ),
      suite("rsa")(
        test("can issue and validate its own token - 1") {
          for {
            token <- rsa1.issueToken(Person.example1.jsonString)
            exit <- rsa1.validateToken(token).exit
          } yield assert(exit)(succeeds(equalTo(())))
        },
        test("can issue and validate its own token - 1") {
          for {
            token <- rsa2.issueToken(Person.example1.jsonString)
            exit <- rsa2.validateToken(token).exit
          } yield assert(exit)(succeeds(equalTo(())))
        },
        test("detects invalid signature") {
          for {
            token1 <- rsa1.issueToken(Person.example1.jsonString)
            token2 <- rsa1.issueToken(Person.example2.jsonString)
            modifiedToken1 = token1.copy(payloadBase64 = token2.payloadBase64, payload = token2.payload)
            exit <- rsa1.validateToken(modifiedToken1).exit
          } yield assert(exit)(fails(equalTo(TokenError.InvalidSignature)))
        },
        test("fails with invalid key - 1") {
          for {
            token <- rsa1.issueToken(Person.example1.jsonString)
            exit <- rsa2.validateToken(token).exit
          } yield assert(exit)(fails(equalTo(TokenError.InvalidSignature)))
        },
        test("fails with invalid key - 2") {
          for {
            token <- rsa2.issueToken(Person.example1.jsonString)
            exit <- rsa1.validateToken(token).exit
          } yield assert(exit)(fails(equalTo(TokenError.InvalidSignature)))
        },
      ),
      suite("invalid algo")(
        test("invalid algo detected") {
          for {
            token <- rsa1.issueToken(Person.example1.jsonString)
            exit <- hmac1.validateToken(token).exit
          } yield assert(exit)(fails(equalTo(TokenError.InvalidAlgorithm(JWTHeader.Alg.HS256, JWTHeader.Alg.RS256))))
        },
      ),
    )

}
