package oxygen.crypto.model

object Base64 {
  val urlEncoder: java.util.Base64.Encoder = java.util.Base64.getUrlEncoder.withoutPadding()
  val urlDecoder: java.util.Base64.Decoder = java.util.Base64.getUrlDecoder

  val stdEncoder: java.util.Base64.Encoder = java.util.Base64.getEncoder
  val stdDecoder: java.util.Base64.Decoder = java.util.Base64.getDecoder
}
