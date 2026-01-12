package oxygen.crypto.model

import java.time.Instant
import oxygen.predef.core.*

enum JWTError extends Error {

  case InvalidAlgorithm(expected: JWTHeader.Alg, actual: JWTHeader.Alg)
  case InvalidSignature
  case Expired(now: Instant, expiredAt: Instant)
  case CryptoFailure(cause: Throwable)

  override def errorMessage: Text = this match
    case JWTError.InvalidAlgorithm(expected, actual) => str"Token was encrypted with a different algorithm [expected=${expected.toText}, actual=${actual.toText}]"
    case JWTError.InvalidSignature                   => str"Token has invalid signature"
    case JWTError.Expired(now, expiredAt)            => str"Token is expired [now=${now.toText}, expiredAt=${expiredAt.toText}]"
    case JWTError.CryptoFailure(cause)               => str"Error during token crypto"

  override def causes: ArraySeq[Error] = this match
    case JWTError.CryptoFailure(cause) => ArraySeq(Error.fromThrowable(cause))
    case _                             => ArraySeq.empty

}
