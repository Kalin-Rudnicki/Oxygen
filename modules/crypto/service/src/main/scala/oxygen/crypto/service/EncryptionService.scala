package oxygen.crypto.service

import java.nio.charset.StandardCharsets
import javax.crypto.Cipher
import javax.crypto.spec.GCMParameterSpec
import oxygen.crypto.model.{EncryptedValue, EncryptionError}
import zio.*

trait EncryptionService {

  def encrypt(value: String): IO[EncryptionError, EncryptedValue]

  def decrypt(value: EncryptedValue): IO[EncryptionError, String]

}
object EncryptionService {

  def layer: URLayer[EncryptionKey, EncryptionService] = ZLayer.fromFunction { Live.apply }
  def noOpLayer: ULayer[EncryptionService] = ZLayer.succeed { EncryptionService.Live(EncryptionKey.none) }

  final class Live(key: EncryptionKey) extends EncryptionService {

    override def encrypt(value: String): IO[EncryptionError, EncryptedValue] =
      key match {
        case key: EncryptionKey.AES =>
          for {
            iv <- aes.genIV
            cypher <- aes.encrypt(iv, key, value).mapError(EncryptionError.duringEncryption)
          } yield EncryptedValue.IVCipher(iv, cypher).toEncryptedValue
        case EncryptionKey.none =>
          ZIO.succeed(EncryptedValue(value))
      }

    override def decrypt(value: EncryptedValue): IO[EncryptionError, String] =
      key match {
        case key: EncryptionKey.AES =>
          for {
            ivCypher <- ZIO.fromTry { value.toIVCipher }.mapError(EncryptionError.duringDecryption)
            decrypted <- aes.decrypt(ivCypher.iv, key, ivCypher.cypher).mapError(EncryptionError.duringDecryption)
          } yield decrypted
        case EncryptionKey.none =>
          ZIO.succeed(value.value)
      }

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      AES
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    private object aes {

      val genIV: UIO[EncryptedValue.IV] =
        Random.nextBytes(12).map { bytes => EncryptedValue.IV(bytes.toArray) }

      def initCypher(iv: EncryptedValue.IV, key: EncryptionKey.AES, mode: Int): Task[Cipher] =
        for {
          cipher <- ZIO.attempt { Cipher.getInstance("AES/GCM/NoPadding") }
          gcmSpec <- ZIO.attempt { new GCMParameterSpec(128, iv.bytes) }
          _ <- ZIO.attempt { cipher.init(mode, key.secretKey, gcmSpec) }
        } yield cipher

      def encrypt(iv: EncryptedValue.IV, key: EncryptionKey.AES, plainText: String): Task[EncryptedValue.Cypher] =
        for {
          cipher <- initCypher(iv, key, Cipher.ENCRYPT_MODE)
          decrypted: Array[Byte] = plainText.getBytes(StandardCharsets.UTF_8)
          encrypted: Array[Byte] <- ZIO.attempt { cipher.doFinal(decrypted) }
        } yield EncryptedValue.Cypher(encrypted)

      def decrypt(iv: EncryptedValue.IV, key: EncryptionKey.AES, cypher: EncryptedValue.Cypher): Task[String] =
        for {
          cipher <- initCypher(iv, key, Cipher.DECRYPT_MODE)
          encrypted: Array[Byte] = cypher.bytes
          decrypted: Array[Byte] <- ZIO.attempt { cipher.doFinal(encrypted) }
        } yield new String(decrypted, StandardCharsets.UTF_8)

    }

  }

}
