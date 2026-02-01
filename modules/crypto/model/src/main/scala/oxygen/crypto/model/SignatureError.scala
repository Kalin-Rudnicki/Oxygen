package oxygen.crypto.model

import java.time.Instant
import oxygen.predef.core.*

enum SignatureError extends Error {

  case InvalidAlgorithm(expected: JWTHeader.Alg, actual: JWTHeader.Alg)
  case InvalidSignature
  case NotYetValid(now: Instant, validAfter: Instant)
  case Expired(now: Instant, expiredAt: Instant)
  case GenericSigningFailure(cause: Throwable)
  case GenericValidationFailure(cause: Throwable)

  override final def errorMessage: Text = this match
    case SignatureError.InvalidAlgorithm(expected, actual) => str"Token was encrypted with a different algorithm [expected=${expected.toText}, actual=${actual.toText}]"
    case SignatureError.InvalidSignature                   => str"Token has invalid signature"
    case SignatureError.NotYetValid(now, validAfter)       => str"Token is not yet valid [now=${now.toText}, validAfter=${validAfter.toText}]"
    case SignatureError.Expired(now, expiredAt)            => str"Token is expired [now=${now.toText}, expiredAt=${expiredAt.toText}]"
    case SignatureError.GenericSigningFailure(cause)       => str"Error during token signing"
    case SignatureError.GenericValidationFailure(cause)    => str"Error during token validation"

  override final def causes: ArraySeq[Error] = this match
    case SignatureError.GenericSigningFailure(cause)    => ArraySeq(Error.fromThrowable(cause))
    case SignatureError.GenericValidationFailure(cause) => ArraySeq(Error.fromThrowable(cause))
    case _                                              => ArraySeq.empty

}
