package oxygen.crypto.model

import java.time.Instant
import oxygen.predef.core.*

enum JWTError extends Throwable {

  case InvalidAlgorithm(expected: JWTHeader.Alg, actual: JWTHeader.Alg)
  case InvalidSignature
  case Expired(now: Instant, expiredAt: Instant)
  case CryptoFailure(cause: Throwable)

  override final def toString: String = getMessage
  override final def getMessage: String = this match
    case JWTError.InvalidAlgorithm(expected, actual) => s"Token was encrypted with a different algorithm [expected=$expected, actual=$actual]"
    case JWTError.InvalidSignature                   => s"Token has invalid signature"
    case JWTError.Expired(now, expiredAt)            => s"Token is expired [now=$now, expiredAt=$expiredAt]"
    case JWTError.CryptoFailure(cause)               => s"Error during token crypto: ${cause.safeGetMessage}"

}
