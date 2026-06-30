package oxygen.crypto.model

/**
  * Pure-Scala SHA-256 (FIPS 180-4). Implemented in shared code so it runs identically on JVM / JS /
  * Native with no platform crypto dependency — notably usable from Scala.js, where the only native
  * digest (Web Crypto `crypto.subtle.digest`) is async-only and so can't back a synchronous API.
  *
  * Used for the RFC 7636 PKCE `S256` challenge; the operations are all 32-bit `Int` arithmetic, which
  * Scala.js implements with the same wraparound semantics as the JVM.
  */
object Sha256 {

  private val K: Array[Int] = Array(
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
  )

  def hash(message: Array[Byte]): Array[Byte] = {
    var h0 = 0x6a09e667; var h1 = 0xbb67ae85; var h2 = 0x3c6ef372; var h3 = 0xa54ff53a
    var h4 = 0x510e527f; var h5 = 0x9b05688c; var h6 = 0x1f83d9ab; var h7 = 0x5be0cd19

    // pad to a multiple of 64 bytes: 0x80, then zeros, then the 64-bit big-endian bit length
    val bitLen = message.length.toLong * 8L
    val withTerminator = message.length + 1
    val padLen = ((56 - withTerminator % 64) + 64) % 64
    val total = withTerminator + padLen + 8
    val msg = new Array[Byte](total)
    System.arraycopy(message, 0, msg, 0, message.length)
    msg(message.length) = 0x80.toByte
    var b = 0
    while b < 8 do { msg(total - 1 - b) = ((bitLen >>> (8 * b)) & 0xff).toByte; b += 1 }

    val w = new Array[Int](64)
    var chunk = 0
    while chunk < total do {
      var t = 0
      while t < 16 do {
        val j = chunk + t * 4
        w(t) = ((msg(j) & 0xff) << 24) | ((msg(j + 1) & 0xff) << 16) | ((msg(j + 2) & 0xff) << 8) | (msg(j + 3) & 0xff)
        t += 1
      }
      while t < 64 do {
        val s0 = Integer.rotateRight(w(t - 15), 7) ^ Integer.rotateRight(w(t - 15), 18) ^ (w(t - 15) >>> 3)
        val s1 = Integer.rotateRight(w(t - 2), 17) ^ Integer.rotateRight(w(t - 2), 19) ^ (w(t - 2) >>> 10)
        w(t) = w(t - 16) + s0 + w(t - 7) + s1
        t += 1
      }

      var a = h0; var bb = h1; var c = h2; var d = h3; var e = h4; var f = h5; var g = h6; var hh = h7
      t = 0
      while t < 64 do {
        val bigS1 = Integer.rotateRight(e, 6) ^ Integer.rotateRight(e, 11) ^ Integer.rotateRight(e, 25)
        val ch = (e & f) ^ (~e & g)
        val temp1 = hh + bigS1 + ch + K(t) + w(t)
        val bigS0 = Integer.rotateRight(a, 2) ^ Integer.rotateRight(a, 13) ^ Integer.rotateRight(a, 22)
        val maj = (a & bb) ^ (a & c) ^ (bb & c)
        val temp2 = bigS0 + maj
        hh = g; g = f; f = e; e = d + temp1; d = c; c = bb; bb = a; a = temp1 + temp2
        t += 1
      }

      h0 += a; h1 += bb; h2 += c; h3 += d; h4 += e; h5 += f; h6 += g; h7 += hh
      chunk += 64
    }

    val out = new Array[Byte](32)
    val hs = Array(h0, h1, h2, h3, h4, h5, h6, h7)
    var k = 0
    while k < 8 do {
      out(k * 4) = ((hs(k) >>> 24) & 0xff).toByte
      out(k * 4 + 1) = ((hs(k) >>> 16) & 0xff).toByte
      out(k * 4 + 2) = ((hs(k) >>> 8) & 0xff).toByte
      out(k * 4 + 3) = (hs(k) & 0xff).toByte
      k += 1
    }
    out
  }

}
