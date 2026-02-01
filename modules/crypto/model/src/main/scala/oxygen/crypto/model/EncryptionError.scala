package oxygen.crypto.model

import oxygen.predef.core.*

enum EncryptionError extends Error {

  case MalformedEncryptedValue
  case InvalidEncryptedValueType(expected: String, actual: String)
  case GenericEncryptionFailure(cause: Throwable)
  case GenericDecryptionFailure(cause: Throwable)

  override final def errorMessage: Text = this match
    case EncryptionError.MalformedEncryptedValue                     => str"Attempted to decrypt malformed encrypted value"
    case EncryptionError.InvalidEncryptedValueType(expected, actual) => str"Invalid encrypted value type: [expected=$expected], but got [actual=$actual]"
    case EncryptionError.GenericEncryptionFailure(cause)             => str"Error during encryption"
    case EncryptionError.GenericDecryptionFailure(cause)             => str"Error during decryption"

  override final def causes: ArraySeq[Error] = this match
    case EncryptionError.GenericEncryptionFailure(cause) => ArraySeq(Error.fromThrowable(cause))
    case EncryptionError.GenericDecryptionFailure(cause) => ArraySeq(Error.fromThrowable(cause))
    case _                                               => ArraySeq.empty

}
object EncryptionError {

  def duringEncryption(throwable: Throwable): EncryptionError = throwable match
    case e: EncryptionError => e
    case _                  => EncryptionError.GenericEncryptionFailure(throwable)

  def duringDecryption(throwable: Throwable): EncryptionError = throwable match
    case e: EncryptionError => e
    case _                  => EncryptionError.GenericDecryptionFailure(throwable)

}
