package oxygen.http.core.internal

object HttpBytes {

  val newline: Array[Byte] = "\r\n".getBytes
  val colon: Array[Byte] = ": ".getBytes
  val space: Array[Byte] = " ".getBytes
  val slash: Array[Byte] = "/".getBytes
  val question: Array[Byte] = "?".getBytes
  val and: Array[Byte] = "&".getBytes
  val equal: Array[Byte] = "=".getBytes

  val httpVersionSpace: Array[Byte] = "HTTP/1.1 ".getBytes
  val httpVersionEOL: Array[Byte] = " HTTP/1.1\r\n".getBytes

  val startCharset: Array[Byte] = "; charset=".getBytes

  val doneStreaming: Array[Byte] = "0\r\n\r\n".getBytes

  object PartialHeaders {

    val contentType: Array[Byte] = "Content-Type: ".getBytes
    val contentLength: Array[Byte] = "Content-Length: ".getBytes

  }

  object FullHeaders {

    val connectionClose: Array[Byte] = "Connection: close\r\n".getBytes
    val transferEncodingChunked: Array[Byte] = "Transfer-Encoding: chunked\r\n".getBytes
    val acceptAny: Array[Byte] = "Accept: */*\r\n".getBytes

  }

}
