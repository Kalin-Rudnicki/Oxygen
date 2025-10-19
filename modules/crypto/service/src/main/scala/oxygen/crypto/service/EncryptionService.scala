package oxygen.crypto.service

import java.nio.charset.StandardCharsets
import javax.crypto.Cipher
import javax.crypto.spec.GCMParameterSpec
import oxygen.crypto.model.{EncryptedValue, EncryptionError}
import oxygen.json.JsonCodec
import zio.*

trait EncryptionService {

  def encrypt(value: String): IO[EncryptionError, EncryptedValue]

  def decrypt(value: EncryptedValue): IO[EncryptionError, String]

}
object EncryptionService {

  val noOpLayer: ULayer[EncryptionService] = ZLayer.succeed { EncryptionService.NoOp }
  val liveLayer: URLayer[EncryptionService.Live.Config, EncryptionService] = ZLayer.fromFunction { EncryptionService.Live.apply }

  case object NoOp extends EncryptionService {

    override def encrypt(value: String): UIO[EncryptedValue] = ZIO.succeed(EncryptedValue(value))

    override def decrypt(value: EncryptedValue): UIO[String] = ZIO.succeed(value.value)

  }

  final case class Live(config: Live.Config) extends EncryptionService {

    private val genIV: UIO[EncryptedValue.IV] =
      Random.nextBytes(12).map { bytes => EncryptedValue.IV(bytes.toArray) }

    private def initCypher(iv: EncryptedValue.IV, mode: Int): Task[Cipher] =
      config.key match {
        case key: EncryptionKey.AES =>
          for {
            cipher <- ZIO.attempt { Cipher.getInstance("AES/GCM/NoPadding") }
            gcmSpec <- ZIO.attempt { new GCMParameterSpec(128, iv.bytes) }
            _ <- ZIO.attempt { cipher.init(mode, key.secretKey, gcmSpec) }
          } yield cipher
      }

    private def encrypt(iv: EncryptedValue.IV, plainText: String): Task[EncryptedValue.Cypher] =
      for {
        cipher <- initCypher(iv, Cipher.ENCRYPT_MODE)
        decrypted: Array[Byte] = plainText.getBytes(StandardCharsets.UTF_8)
        encrypted: Array[Byte] <- ZIO.attempt { cipher.doFinal(decrypted) }
      } yield EncryptedValue.Cypher(encrypted)

    private def decrypt(iv: EncryptedValue.IV, cypher: EncryptedValue.Cypher): Task[String] =
      for {
        cipher <- initCypher(iv, Cipher.DECRYPT_MODE)
        encrypted: Array[Byte] = cypher.bytes
        decrypted: Array[Byte] <- ZIO.attempt { cipher.doFinal(encrypted) }
      } yield new String(decrypted, StandardCharsets.UTF_8)

    override def encrypt(value: String): IO[EncryptionError, EncryptedValue] =
      for {
        iv <- genIV
        cypher <- encrypt(iv, value).mapError(EncryptionError.duringEncryption)
      } yield EncryptedValue.IVCipher(iv, cypher).toEncryptedValue

    override def decrypt(value: EncryptedValue): IO[EncryptionError, String] =
      for {
        ivCypher <- ZIO.fromTry { value.toIVCipher }.mapError(EncryptionError.duringDecryption)
        decrypted <- decrypt(ivCypher.iv, ivCypher.cypher).mapError(EncryptionError.duringDecryption)
      } yield decrypted

  }
  object Live {

    def make(key: EncryptionKey): Live = Live(Config(key))

    final case class Config(
        key: EncryptionKey,
    ) derives JsonCodec

  }

}
