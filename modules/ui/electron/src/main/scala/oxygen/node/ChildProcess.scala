package oxygen.node

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import zio.*

object ChildProcess {

  def spawn(command: String, args: Seq[String] = Nil, options: SpawnOptions = SpawnOptions()): Task[String] =
    ZIO.async { callback =>
      val jsArgs = args.toJSArray
      val jsOptions = options.toJS
      val process = facades.ChildProcess.spawn(command, jsArgs, jsOptions)

      val output = new StringBuilder()

      process.stdout.on(
        "data",
        (data: js.Any) => {
          output.append(data.toString)
        },
      )

      process.stderr.on(
        "data",
        (data: js.Any) => {
          output.append(data.toString) // you can separate stdout/stderr if you want
        },
      )

      process.on(
        "close",
        (code: Int) => {
          if code == 0 then callback(Exit.succeed(output.result().trim))
          else callback(Exit.fail(new Exception(s"Process exited with code $code")))
        },
      )

      process.on(
        "error",
        (err: js.Error) => {
          callback(Exit.fail(new Exception(err.message)))
        },
      )
    }

}
