package oxygen.node

import scala.scalajs.js
import zio.*

object FS {

  def readFile(path: String, encoding: String = "utf8"): Task[String] =
    ZIO.async { callback =>
      facades.FS.readFile(
        path,
        encoding,
        (err: js.Error, data: String) => {
          if err != null then callback(Exit.fail(new Exception(err.message)))
          else callback(Exit.succeed(data))
        },
      )
    }

  def writeFile(path: String, data: String): Task[Unit] =
    ZIO.async { callback =>
      facades.FS.writeFile(
        path,
        data,
        (err: js.Error) => {
          if err != null then callback(Exit.fail(new Exception(err.message)))
          else callback(Exit.succeed(()))
        },
      )
    }

  def exists(path: String): Task[Boolean] =
    ZIO.attemptBlocking { facades.FS.existsSync(path) }

  def mkdir(path: String, options: MkdirOptions = MkdirOptions()): Task[Unit] =
    ZIO.async { callback =>
      facades.FS.mkdir(
        path,
        options.toJS,
        (err: js.Error) => {
          if err != null then callback(Exit.fail(new Exception(err.message)))
          else callback(Exit.succeed(()))
        },
      )
    }

  def listDir(path: String): Task[Seq[String]] =
    ZIO.async { callback =>
      facades.FS.readdir(
        path,
        (err: js.Error, files: js.Array[String]) => {
          if err != null then callback(Exit.fail(new Exception(err.message)))
          else callback(Exit.succeed(files.toSeq))
        },
      )
    }

}
