package oxygen.crypto.service

import java.time.Instant
import oxygen.crypto.model.JWTHeader
import oxygen.predef.core.*

enum TokenError extends Throwable {

  case InvalidAlgorithm(expected: JWTHeader.Alg, actual: JWTHeader.Alg)
  case InvalidSignature
  case Expired(now: Instant, expiredAt: Instant)
  case CryptoFailure(cause: Throwable)

  override final def getMessage: String = this match
    case TokenError.InvalidAlgorithm(expected, actual) => s"Token was encrypted with a different algorithm [expected=$expected, actual=$actual]"
    case TokenError.InvalidSignature                   => s"Token has invalid signature"
    case TokenError.Expired(now, expiredAt)            => s"Token is expired [now=$now, expiredAt=$expiredAt]"
    case TokenError.CryptoFailure(cause)               => s"Error during token crypto: ${cause.safeGetMessage}"

}
