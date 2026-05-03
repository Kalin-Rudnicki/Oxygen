package oxygen.node

import scala.scalajs.js
import zio.*

object OS {

  /** Current working directory */
  def cwd: Task[String] =
    ZIO.attemptBlocking { js.Dynamic.global.process.cwd().asInstanceOf[String] }

  /** User home directory */
  def home: Task[String] =
    ZIO.attemptBlocking { js.Dynamic.global.process.env.HOME.asInstanceOf[String] }

  /** Platform (linux, darwin, win32) */
  def platform: Task[String] =
    ZIO.attemptBlocking { js.Dynamic.global.process.platform.asInstanceOf[String] }

}
