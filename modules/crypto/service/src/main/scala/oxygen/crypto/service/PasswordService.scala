package oxygen.crypto.service

import org.mindrot.jbcrypt.BCrypt
import oxygen.crypto.model.Password
import zio.*

trait PasswordService {

  /**
    * Accepts a plain text password, and hashes it.
    * This hash is then safe to store, and you can then check if a password is correct using [[validate]].
    */
  def hashPassword(plainTextPassword: Password.PlainText): UIO[Password.Hashed]

  def validate(plainTextPasswordToValidate: Password.PlainText, correctHashedPassword: Password.Hashed): UIO[Boolean]

}
object PasswordService {

  val layer: ULayer[PasswordService] = ZLayer.succeed(PasswordService.Live)

  case object Live extends PasswordService {

    override def hashPassword(plainTextPassword: Password.PlainText): UIO[Password.Hashed] =
      for {
        salt <- ZIO.attempt { BCrypt.gensalt() }.!
        hashed <- ZIO.attempt { BCrypt.hashpw(plainTextPassword.unsafeGetRawPassword, salt) }.!
      } yield Password.Hashed.unsafeWrapPasswordHash(hashed)

    override def validate(plainTextPasswordToValidate: Password.PlainText, correctHashedPassword: Password.Hashed): UIO[Boolean] =
      ZIO.attempt { BCrypt.checkpw(plainTextPasswordToValidate.unsafeGetRawPassword, correctHashedPassword.getPasswordHash) }.!

  }

}
