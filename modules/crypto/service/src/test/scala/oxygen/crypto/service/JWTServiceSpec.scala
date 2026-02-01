package oxygen.crypto.service

import java.time.Instant
import oxygen.crypto.model.SignatureError
import oxygen.crypto.service.ExampleServices.jwtServices.*
import oxygen.predef.test.*
import zio.test.TestClock

object JWTServiceSpec extends OxygenSpecDefault {

  private val expiryInstant: Instant = Instant.EPOCH.plus(ExampleServices.tokenTimeToLive)

  override def testSpec: TestSpec =
    suite("JWTServiceSpec")(
      suite("hmac")(
        test("can issue and validate its own token") {
          for {
            token <- hmac1.issueToken(Person.example1)
            exit <- hmac1.validateToken(token).exit
          } yield assert(exit)(succeeds(equalTo(())))
        },
        test("fails on invalid token") {
          for {
            token <- hmac1.issueToken(Person.example1)
            exit <- hmac2.validateToken(token).exit
          } yield assert(exit)(fails(equalTo(SignatureError.InvalidSignature)))
        },
        test("fails on expired token - exact") {
          for {
            token <- hmac1.issueToken(Person.example1)
            _ <- TestClock.adjust(ExampleServices.tokenTimeToLive)
            exit <- hmac1.validateToken(token).exit
          } yield assert(exit)(fails(equalTo(SignatureError.Expired(expiryInstant, expiryInstant))))
        },
        test("fails on expired token - after") {
          for {
            token <- hmac1.issueToken(Person.example1)
            _ <- TestClock.adjust(ExampleServices.tokenTimeToLive.plus(1.second))
            exit <- hmac1.validateToken(token).exit
          } yield assert(exit)(fails(equalTo(SignatureError.Expired(expiryInstant.plus(1.second), expiryInstant))))
        },
      ),
      suite("rsa")(
        test("can issue and validate its own token") {
          for {
            token <- rsa1.issueToken(Person.example1)
            exit <- rsa1.validateToken(token).exit
          } yield assert(exit)(succeeds(equalTo(())))
        },
        test("fails on invalid token") {
          for {
            token <- rsa1.issueToken(Person.example1)
            exit <- rsa2.validateToken(token).exit
          } yield assert(exit)(fails(equalTo(SignatureError.InvalidSignature)))
        },
        test("fails on expired token - exact") {
          for {
            token <- rsa1.issueToken(Person.example1)
            _ <- TestClock.adjust(ExampleServices.tokenTimeToLive)
            exit <- rsa1.validateToken(token).exit
          } yield assert(exit)(fails(equalTo(SignatureError.Expired(expiryInstant, expiryInstant))))
        },
        test("fails on expired token - after") {
          for {
            token <- rsa1.issueToken(Person.example1)
            _ <- TestClock.adjust(ExampleServices.tokenTimeToLive.plus(1.second))
            exit <- rsa1.validateToken(token).exit
          } yield assert(exit)(fails(equalTo(SignatureError.Expired(expiryInstant.plus(1.second), expiryInstant))))
        },
      ),
    )

}
