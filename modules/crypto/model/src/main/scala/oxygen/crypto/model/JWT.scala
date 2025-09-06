package oxygen.crypto.model

import java.time.Instant
import java.util.UUID
import oxygen.json.JsonCodec

final case class JWT[A](
    payload: A,
    token: BearerToken,
)
object JWT {

  type Std[P] = JWT[StandardPayload[P]]

  final case class StandardPayload[P](
      tokenId: UUID,
      issuedAt: Instant,
      expiresAt: Instant,
      payload: P,
  ) derives JsonCodec

}
