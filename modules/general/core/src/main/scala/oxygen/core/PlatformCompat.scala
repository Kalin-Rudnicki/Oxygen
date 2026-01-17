package oxygen.core

import java.util.UUID

trait PlatformCompat {

  def randomUUID(): UUID

  def typeNameAndArgs(klass: Class[?]): (String, List[String])

}
object PlatformCompat extends PlatformCompat, PlatformCompatPlatformSpecific
