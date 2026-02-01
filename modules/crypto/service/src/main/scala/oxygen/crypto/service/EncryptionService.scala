package oxygen.crypto.service

import oxygen.crypto.model.*
import oxygen.predef.core.*
import scala.reflect.TypeTest
import zio.*

object EncryptionService {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Encryptor
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Encryptor {

    def encrypt(value: String): UIO[EncryptedValue]

    final def encryptFormatted(value: String): UIO[EncryptedValue.Formatted] = encrypt(value).map(_.format)

  }
  object Encryptor {

    final class SymmetricLive(key: EncryptionKey.Symmetric) extends Encryptor {

      override def encrypt(value: String): UIO[SymmetricEncryptedValue] = attemptEncrypt { key.encrypt(value) }

    }

    final class AsymmetricLive(key: EncryptionKey.CanEncrypt.Asymmetric) extends Encryptor {

      override def encrypt(value: String): UIO[AsymmetricEncryptedValue] = attemptEncrypt { key.encrypt(value) }

    }

    def fromKey(key: EncryptionKey.CanEncrypt): Encryptor = key match
      case key: EncryptionKey.Symmetric             => Encryptor.SymmetricLive(key)
      case key: EncryptionKey.CanEncrypt.Asymmetric => Encryptor.AsymmetricLive(key)

    def live: URLayer[EncryptionKey.CanEncrypt, EncryptionService.Encryptor] = ZLayer.fromFunction { EncryptionService.Encryptor.fromKey }
    def untypedLive: URLayer[EncryptionKey, EncryptionService.Encryptor] = EncryptionService.narrowKeyLayer[EncryptionKey.CanEncrypt] >>> live

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Decryptor
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Decryptor {

    def decrypt(value: EncryptedValue): UIO[String]

    final def decryptFormatted(formattedValue: EncryptedValue.Formatted): UIO[String] =
      ZIO.attempt { formattedValue.unsafeParse }.catchAllCause { _ => ZIO.die(EncryptionError.MalformedEncryptedValue) }.flatMap(decrypt)

  }
  object Decryptor {

    final class SymmetricLive(key: EncryptionKey.Symmetric) extends Decryptor {

      override def decrypt(value: EncryptedValue): UIO[String] = value match
        case value: SymmetricEncryptedValue => attemptDecrypt { key.decryptSymmetric(value) }
        case _: AsymmetricEncryptedValue    => ZIO.die(EncryptionError.InvalidEncryptedValueType("symmetric", "asymmetric"))

    }

    final class AsymmetricLive(key: EncryptionKey.CanDecrypt.Asymmetric) extends Decryptor {

      override def decrypt(value: EncryptedValue): UIO[String] = value match
        case value: AsymmetricEncryptedValue => attemptDecrypt { key.decryptAsymmetric(value) }
        case _: SymmetricEncryptedValue      => ZIO.die(EncryptionError.InvalidEncryptedValueType("asymmetric", "symmetric"))

    }

    def fromKey(key: EncryptionKey.CanDecrypt): Decryptor = key match
      case key: EncryptionKey.Symmetric             => Decryptor.SymmetricLive(key)
      case key: EncryptionKey.CanDecrypt.Asymmetric => Decryptor.AsymmetricLive(key)

    def live: URLayer[EncryptionKey.CanDecrypt, EncryptionService.Decryptor] = ZLayer.fromFunction { EncryptionService.Decryptor.fromKey }
    def untypedLive: URLayer[EncryptionKey, EncryptionService.Decryptor] = EncryptionService.narrowKeyLayer[EncryptionKey.CanDecrypt] >>> live

  }

  def live: URLayer[EncryptionKey.CanEncrypt & EncryptionKey.CanDecrypt, EncryptionService.Encryptor & EncryptionService.Decryptor] = Encryptor.live ++ Decryptor.live
  def untypedLive: URLayer[EncryptionKey, EncryptionService.Encryptor & EncryptionService.Decryptor] = Encryptor.untypedLive ++ Decryptor.untypedLive

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def attemptEncrypt[A](thunk: => A)(using Trace): UIO[A] =
    ZIO.attempt { thunk }.mapError(EncryptionError.GenericEncryptionFailure(_)).!

  private def attemptDecrypt[A](thunk: => A)(using Trace): UIO[A] =
    ZIO.attempt { thunk }.mapError(EncryptionError.GenericDecryptionFailure(_)).!

  private def narrowKeyLayer[A <: EncryptionKey: {TypeTag as tt, Tag}](using typeTest: TypeTest[EncryptionKey, A]): URLayer[EncryptionKey, A] =
    ZLayer {
      ZIO.serviceWithZIO[EncryptionKey] {
        case typeTest(key) => ZIO.succeed(key)
        case key           => ZIO.dieMessage(s"Invalid key type, [expected=${tt.prefixObject}] but got [actual=${TypeTag.fromClass(key.getClass).prefixObject}]")
      }
    }

}
