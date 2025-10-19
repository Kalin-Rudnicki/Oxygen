package oxygen.crypto.model

import oxygen.predef.core.*

enum EncryptionError extends Throwable {

  case MalformedEncryptedValue(value: String)
  case GenericEncryptionFailure(cause: Throwable)
  case GenericDecryptionFailure(cause: Throwable)

  override final def toString: String = getMessage
  override final def getMessage: String = this match
    case EncryptionError.MalformedEncryptedValue(_)      => "Attempted to decrypt malformed encrypted value"
    case EncryptionError.GenericEncryptionFailure(cause) => s"Error during encryption: ${cause.safeGetMessage}"
    case EncryptionError.GenericDecryptionFailure(cause) => s"Error during decryption: ${cause.safeGetMessage}"

}
object EncryptionError {

  def duringEncryption(throwable: Throwable): EncryptionError = throwable match
    case e: EncryptionError => e
    case _                  => EncryptionError.GenericEncryptionFailure(throwable)

  def duringDecryption(throwable: Throwable): EncryptionError = throwable match
    case e: EncryptionError => e
    case _                  => EncryptionError.GenericDecryptionFailure(throwable)

}
