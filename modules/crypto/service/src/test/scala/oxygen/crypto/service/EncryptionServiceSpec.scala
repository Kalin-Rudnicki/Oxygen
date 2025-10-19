package oxygen.crypto.service

import oxygen.crypto.model.EncryptedValue
import oxygen.predef.test.*

object EncryptionServiceSpec extends OxygenSpecDefault {

  object cryptos {
    val noOp: EncryptionService = EncryptionService.Live(EncryptionKey.none)
    val live1: EncryptionService = EncryptionService.Live(EncryptionKey.AES("2HtUSy7O9cGCrn03TfbInaMBd/wAW7tzj/cRcpeo2MY="))
    val live2: EncryptionService = EncryptionService.Live(EncryptionKey.AES("yFmOe/YmRXeL0Pewa0Brq8njeW4DSFsAF0fWIvH+7bM="))
  }

  override def testSpec: TestSpec =
    suite("EncryptionServiceSpec")(
      suite("no-op")(
        test("can 'encrypt' and 'decrypt', and the value is the same") {
          for {
            initialStr <- RandomGen.lowerCaseString(25)
            encrypted <- cryptos.noOp.encrypt(initialStr)
            encryptedStr = encrypted.value
            roundTripStr <- cryptos.noOp.decrypt(encrypted)
          } yield assertTrue(
            roundTripStr == initialStr,
            encryptedStr == initialStr,
          )
        },
      ),
      suite("live")(
        test("basic round-trip works") {
          for {
            initialStr <- RandomGen.lowerCaseString(25)
            encrypted <- cryptos.live1.encrypt(initialStr)
            encryptedStr = encrypted.value
            roundTripStr <- cryptos.live1.decrypt(encrypted)
          } yield assertTrue(
            roundTripStr == initialStr,
            encryptedStr != initialStr,
          )
        },
        test("encrypting the same value with the same service creates different IV and Cypher") {
          for {
            initialStr <- RandomGen.lowerCaseString(150)
            encrypted1 <- cryptos.live1.encrypt(initialStr)
            encrypted2 <- cryptos.live1.encrypt(initialStr)
            roundTripStr1 <- cryptos.live1.decrypt(encrypted1)
            roundTripStr2 <- cryptos.live1.decrypt(encrypted2)
            ivc1 <- ZIO.fromTry { encrypted1.toIVCipher }
            ivc2 <- ZIO.fromTry { encrypted2.toIVCipher }

            _ <- ZIO.logInfo(
              s"""initialStr: $initialStr
                 |enc1: ${encrypted1.value}
                 |enc2: ${encrypted2.value}
                 |rt1: $roundTripStr1
                 |rt2: $roundTripStr2""".stripMargin,
            )
          } yield assertTrue(
            roundTripStr1 == initialStr,
            roundTripStr2 == initialStr,
            ivc1.iv.bytes.toSeq != ivc2.iv.bytes.toSeq,
            ivc1.cypher.bytes.toSeq != ivc2.cypher.bytes.toSeq,
          )
        },
        test("attempting to decrypt with invalid key fails") {
          for {
            initialStr <- RandomGen.lowerCaseString(25)
            encrypted <- cryptos.live1.encrypt(initialStr)
            res <- cryptos.live2.decrypt(encrypted).exit
          } yield assert(res)(fails(anything))
        },
        test("fails with invalid IV") {
          for {
            initialStr <- RandomGen.lowerCaseString(25)
            encrypted1 <- cryptos.live1.encrypt(initialStr)
            encrypted2 <- cryptos.live1.encrypt(initialStr)
            ivc1 <- ZIO.fromTry { encrypted1.toIVCipher }
            ivc2 <- ZIO.fromTry { encrypted2.toIVCipher }
            mixed = EncryptedValue.IVCipher(ivc1.iv, ivc2.cypher)
            res <- cryptos.live1.decrypt(mixed.toEncryptedValue).exit
          } yield assert(res)(fails(anything))
        },
      ),
    )

  override def testAspects: Chunk[TestSpecAspect] = Chunk(TestAspect.withLiveRandom)

}
