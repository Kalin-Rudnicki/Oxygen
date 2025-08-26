package oxygen.core

import java.util.UUID

trait PlatformCompat {

  def randomUUID(): UUID

}
object PlatformCompat extends PlatformCompat, PlatformCompatPlatformSpecific
