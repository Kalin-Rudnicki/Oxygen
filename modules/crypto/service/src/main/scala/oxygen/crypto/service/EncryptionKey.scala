package oxygen.crypto.service

import java.util.Base64
import javax.crypto.SecretKey
import javax.crypto.spec.SecretKeySpec
import oxygen.json.{jsonDiscriminator, JsonCodec}

@jsonDiscriminator("alg")
sealed trait EncryptionKey derives JsonCodec
object EncryptionKey {

  final case class AES(base64Key: String) extends EncryptionKey {

    lazy val secretKey: SecretKey = {
      val decoded = Base64.getDecoder.decode(base64Key)
      decoded.length match
        case 16 | 24 | 32 => new SecretKeySpec(decoded, "AES")
        case other        => throw new IllegalArgumentException(s"Invalid AES key length: $other bytes")
    }

  }

}
