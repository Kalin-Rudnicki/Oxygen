package oxygen.core

import java.util.UUID
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.Random

trait PlatformCompatPlatformSpecific { self: PlatformCompat =>

  override def randomUUID(): UUID = UUID(Random.nextLong(), Random.nextLong())

  override def secureRandomBytes(byteCount: Int): Array[Byte] = {
    val arr = new Uint8Array(byteCount)
    js.Dynamic.global.crypto.getRandomValues(arr)
    Array.tabulate(byteCount)(i => arr(i).toByte)
  }

  override def typeNameAndArgs(klass: Class[?]): (String, List[String]) =
    (
      klass.getName,
      Nil,
    )

}
