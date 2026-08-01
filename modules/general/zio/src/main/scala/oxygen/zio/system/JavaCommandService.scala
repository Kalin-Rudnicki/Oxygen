package oxygen.zio.system

import java.io.{InputStream, OutputStream}
import java.lang as jl
import java.nio.charset.StandardCharsets
import oxygen.predef.core.*
import oxygen.zio.ZioCauses
import oxygen.zio.error.CommandError
import oxygen.zio.syntax.error.*
import oxygen.zio.syntax.log.*
import zio.*
import zio.stream.*

object JavaCommandService extends CommandService {

  private val charset = StandardCharsets.UTF_8

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Public API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override def executeSync(
      command: BuiltCommand,
      stdIn: CommandInputSource,
      trim: Boolean,
  )(using Trace): IO[CommandError, (stdOut: String, stdErr: String, exitCode: Int)] =
    for {
      outRef <- Ref.make("")
      errRef <- Ref.make("")
      exitCode <- run(
        command = command,
        stdIn = stdIn,
        stdOut = OutputMode.Collect(outRef),
        stdErr = OutputMode.Collect(errRef),
      )
      stdOut <- outRef.get
      stdErr <- errRef.get
    } yield (
      stdOut = maybeTrim(stdOut, trim),
      stdErr = maybeTrim(stdErr, trim),
      exitCode = exitCode,
    )

  override def executeSyncStreamErr(
      command: BuiltCommand,
      stdIn: CommandInputSource,
      stdErr: CommandOutputSource,
      trim: Boolean,
  )(using Trace): IO[CommandError, (stdOut: String, exitCode: Int)] =
    for {
      outRef <- Ref.make("")
      stdErrMode <- OutputMode.fromSource(command, stdErr)
      exitCode <- run(
        command = command,
        stdIn = stdIn,
        stdOut = OutputMode.Collect(outRef),
        stdErr = stdErrMode,
      )
      stdOut <- outRef.get
    } yield (
      stdOut = maybeTrim(stdOut, trim),
      exitCode = exitCode,
    )

  override def executeCode(
      command: BuiltCommand,
      stdIn: CommandInputSource,
      stdOut: CommandOutputSource,
      stdErr: CommandOutputSource,
  )(using Trace): IO[CommandError, Int] =
    for {
      stdOutMode <- OutputMode.fromSource(command, stdOut)
      stdErrMode <- OutputMode.fromSource(command, stdErr)
      exitCode <- run(
        command = command,
        stdIn = stdIn,
        stdOut = stdOutMode,
        stdErr = stdErrMode,
      )
    } yield exitCode

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Core runner
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def run(
      command: BuiltCommand,
      stdIn: CommandInputSource,
      stdOut: OutputMode,
      stdErr: OutputMode,
  )(using Trace): IO[CommandError, Int] =
    ZIO.scoped {
      for {
        inRedirect <- inputRedirect(command, stdIn)
        outRedirect = outputRedirect(stdOut)
        errRedirect = outputRedirect(stdErr)
        process <- startProcess(command, inRedirect, outRedirect, errRedirect)

        // Stdout/stderr must be consumed concurrently with waitFor to avoid pipe-buffer deadlocks
        // whenever those streams are PIPE'd into our process.
        outFiber <- consumeOutput(command, process.getInputStream, stdOut).forkScoped
        errFiber <- consumeOutput(command, process.getErrorStream, stdErr).forkScoped
        inFiber <- writeInput(command, process, stdIn).forkScoped

        exitCode <-
          ZIO
            .attemptBlockingInterrupt { process.waitFor() }
            .convertCausesFail { executionFailure(command, "wait for process exit", _) }

        // Process is done — stop feeding stdin; still drain any piped stdout/stderr fully.
        _ <- inFiber.interrupt
        _ <- outFiber.join
        _ <- errFiber.join
      } yield exitCode
    }

  private def startProcess(
      command: BuiltCommand,
      inRedirect: jl.ProcessBuilder.Redirect,
      outRedirect: jl.ProcessBuilder.Redirect,
      errRedirect: jl.ProcessBuilder.Redirect,
  )(using Trace): ZIO[Scope, CommandError, jl.Process] = {
    val acquire: IO[CommandError, jl.Process] =
      for {
        cwdFile <- resolveCwd(command)
        process <-
          ZIO
            .attemptBlocking {
              val pb = new jl.ProcessBuilder((command.command :: command.args)*)

              cwdFile.foreach(pb.directory)

              if command.env.nonEmpty then {
                val env = pb.environment()
                command.env.foreach { (k, v) => env.put(k, v) }
              }

              pb.redirectInput(inRedirect)
              pb.redirectOutput(outRedirect)
              pb.redirectError(errRedirect)

              pb.start()
            }
            .convertCausesFail { executionFailure(command, "start process", _) }
      } yield process

    val release: jl.Process => UIO[Unit] = process =>
      ZIO.succeed {
        if process.isAlive then {
          process.destroy()
          if process.isAlive then process.destroyForcibly()
        }
      }

    ZIO.acquireRelease(acquire)(release)
  }

  private def resolveCwd(command: BuiltCommand)(using Trace): IO[CommandError, Option[java.io.File]] =
    ZIO
      .foreach(command.cwd)(_.toJavaFile)
      .convertCausesFail { executionFailure(command, "resolve working directory", _) }

  private def resolveJavaFile(command: BuiltCommand, path: Path, whileAttemptingTo: String)(using Trace): IO[CommandError, java.io.File] =
    path.toJavaFile.convertCausesFail { executionFailure(command, whileAttemptingTo, _) }

  private def inputRedirect(command: BuiltCommand, stdIn: CommandInputSource)(using Trace): IO[CommandError, jl.ProcessBuilder.Redirect] =
    stdIn match {
      case CommandInputSource.Empty     => ZIO.succeed { jl.ProcessBuilder.Redirect.DISCARD }
      case CommandInputSource.Pipe      => ZIO.succeed { jl.ProcessBuilder.Redirect.INHERIT }
      case CommandInputSource.File(path) =>
        resolveJavaFile(command, path, "resolve stdin file").map { jl.ProcessBuilder.Redirect.from }
      case _: CommandInputSource.Const  => ZIO.succeed { jl.ProcessBuilder.Redirect.PIPE }
      case _: CommandInputSource.Stream => ZIO.succeed { jl.ProcessBuilder.Redirect.PIPE }
    }

  private def outputRedirect(mode: OutputMode): jl.ProcessBuilder.Redirect =
    mode match {
      case OutputMode.Discard       => jl.ProcessBuilder.Redirect.DISCARD
      case OutputMode.ToFile(file)  => jl.ProcessBuilder.Redirect.to(file)
      case _                        => jl.ProcessBuilder.Redirect.PIPE
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Stdin
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def writeInput(
      command: BuiltCommand,
      process: jl.Process,
      stdIn: CommandInputSource,
  )(using Trace): IO[CommandError, Unit] =
    stdIn match {
      // OS / ProcessBuilder already owns these
      case CommandInputSource.Empty | CommandInputSource.Pipe | _: CommandInputSource.File =>
        ZIO.unit

      case CommandInputSource.Const(value) =>
        ZIO
          .attemptBlockingInterrupt {
            val os: OutputStream = process.getOutputStream
            try {
              os.write(value.getBytes(charset))
              os.flush()
            } finally os.close()
          }
          .convertCausesFail { executionFailure(command, "write stdin", _) }

      case CommandInputSource.Stream(bytes) =>
        val os: OutputStream = process.getOutputStream
        bytes
          .run { ZSink.fromOutputStream(os) }
          .unit
          .ensuring { ZIO.attempt { os.flush() }.ignore *> ZIO.attempt { os.close() }.ignore }
          .convertCausesFail { executionFailure(command, "write stdin stream", _) }
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Stdout / stderr
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def consumeOutput(
      command: BuiltCommand,
      stream: InputStream,
      mode: OutputMode,
  )(using Trace): IO[CommandError, Unit] =
    mode match {
      // OS / ProcessBuilder already owns these
      case OutputMode.Discard | _: OutputMode.ToFile =>
        ZIO.unit

      case OutputMode.Collect(ref) =>
        ZStream
          .fromInputStream(stream)
          .runCollect
          .map { chunk => new String(chunk.toArray, charset) }
          .flatMap(ref.set)
          .convertCausesFail { executionFailure(command, "read process output", _) }

      case OutputMode.PipeTo(target) =>
        ZIO
          .attemptBlockingInterrupt {
            try stream.transferTo(target)
            finally stream.close()
          }
          .unit
          .convertCausesFail { executionFailure(command, "pipe process output", _) }

      case OutputMode.Log(logLevel, showCommand) =>
        val logLine: String => UIO[Unit] = showCommand match {
          case show: ShowCommand.NonEmpty =>
            line => ZIO.logAtLevel(logLevel)(line, Cause.Empty) @@ show.toAspect(command)
          case ShowCommand.Empty =>
            line => ZIO.logAtLevel(logLevel)(line, Cause.Empty)
        }

        ZStream
          .fromInputStream(stream)
          .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
          .mapZIO(logLine)
          .runDrain
          .convertCausesFail { executionFailure(command, "log process output", _) }
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def maybeTrim(value: String, trim: Boolean): String =
    if trim then value.trim else value

  private def executionFailure(command: BuiltCommand, whileAttemptingTo: String, causes: ZioCauses): CommandError =
    CommandError.ExecutionFailure(command, Error(s"Failed to $whileAttemptingTo", causes))

  /**
    * Internal sink policy for a single process stream (stdout or stderr).
    * Built from [[CommandOutputSource]] for the streaming APIs, or [[Collect]] for capture APIs.
    *
    * File redirects are resolved up front to a [[java.io.File]] and handed to ProcessBuilder
    * (`Redirect.from` / `Redirect.to`); no ZIO-side stream copy.
    */
  private enum OutputMode {
    case Discard
    case Collect(ref: Ref[String])
    case PipeTo(target: OutputStream)
    case ToFile(file: java.io.File)
    case Log(logLevel: LogLevel, showCommand: ShowCommand)
  }
  private object OutputMode {

    def fromSource(command: BuiltCommand, source: CommandOutputSource)(using Trace): IO[CommandError, OutputMode] =
      source match {
        case CommandOutputSource.Empty =>
          ZIO.succeed { OutputMode.Discard }
        case CommandOutputSource.PipeStdOut =>
          ZIO.succeed { OutputMode.PipeTo(jl.System.out) }
        case CommandOutputSource.PipeStdErr =>
          ZIO.succeed { OutputMode.PipeTo(jl.System.err) }
        case CommandOutputSource.File(path) =>
          resolveJavaFile(command, path, "resolve output file").map { OutputMode.ToFile(_) }
        case CommandOutputSource.Log(level, show) =>
          ZIO.succeed { OutputMode.Log(level, show) }
      }

  }

}
