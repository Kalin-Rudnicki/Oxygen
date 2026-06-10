package oxygen.executable
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import oxygen.cli.*
import oxygen.predef.core.*
import oxygen.predef.zio.*
import oxygen.zio.JarUtils
import zio.*
object AutoComplete {
  private def generateScriptText(targetPath: String, scriptPath: String, isNative: Boolean, baseName: String, programArgs: List[String]): String = {
    val fn = s"__oxygen_cli_complete__$baseName"
    val args = if programArgs.nonEmpty then s"\n         ${programArgs.map(_.unesc("\"")).mkString(" ")} \\" else ""
    val end = if isNative then s"complete -F $fn $baseName\ncomplete -F $fn ./$baseName" else s"if [[ \"$$1\" == \"--:\" && \"$$2\" == \"export\" ]]; then alias $baseName=\"$scriptPath\"; complete -F $fn $scriptPath; complete -F $fn $baseName; else java -jar $targetPath \\$args \"$$@\"; fi"
    s"#!/bin/bash\n$fn() { local r=$$(OXYGEN_CLI_COMPLETE__NUM_WORDS=\"$$(( $${#COMP_WORDS[@]} - 1 ))\" OXYGEN_CLI_COMPLETE__ARG_IDX=\"$$(( COMP_CWORD - 1 ))\" OXYGEN_CLI_COMPLETE__ACC_ARGS=\"$$acc\" OXYGEN_CLI_COMPLETE__JOIN_STR=\"$$'\\n-----separator-----\\n'\" ${if isNative then "" else s"java -jar "}$targetPath --: complete); COMPREPLY=($$r); };\n$end"
  }
  private def generateScript(programArgs: List[String]): Task[Unit] =
    JarUtils.getJarFile.flatMap { path =>
      val abs = Paths.get(path).toAbsolutePath
      val name = abs.getFileName.toString.stripSuffix(".jar")
      val bash = abs.toString.stripSuffix(".jar") + ".sh"
      val text = generateScriptText(abs.toString, bash, !name.endsWith(".jar"), name, programArgs)
      if name.endsWith(".jar") then ZIO.attempt(Files.write(Paths.get(bash), text.getBytes(StandardCharsets.UTF_8))).unit *> Command("chmod")("+x", bash).executeSuccess() *> Console.printLine(s"Generated $bash")
      else Console.printLine(text)
    }
  private def getEnvVar[A: StringDecoder as dec](key: String): Task[A] =
    System.env(key).someOrFail(new RuntimeException(s"Missing env $key")).flatMap(v => ZIO.fromEither(dec.decodeSimple(v)).mapError(new RuntimeException(_)))
  private def autoComplete(app: CompiledCliApp[?]): Task[Unit] =
    for {
      n <- getEnvVar[Int](CompletionRequest.numWordsEnv)
      i <- getEnvVar[Int](CompletionRequest.argIdxEnv)
      a <- getEnvVar[String](CompletionRequest.accArgsEnv)
      j <- getEnvVar[String](CompletionRequest.joinStrEnv)
      args = a.detailedSplit(scala.util.matching.Regex.quote(j).r, true, true).toList
      _ <- ZIO.fail(new RuntimeException(s"bad args")).unless(args.length == n && i < n)
      out = app.complete(CompletionRequest(n, i, args, j)).distinct.sorted
      _ <- Console.printLine(out.mkString(j))
    } yield ()
  def handleArgs(args: List[String], app: CompiledCliApp[?]): IO[AutoCompleteError, Unit] = args match {
    case "generate" :: rest => generateScript(rest match { case "--oxygen-args" :: p => p :+ "--"; case "--args" :: p => p; case Nil => Nil; case _ => Nil }).mapError(AutoCompleteError.ProgramError(_))
    case "complete" :: Nil => autoComplete(app).mapError(AutoCompleteError.ProgramError(_))
    case _ => ZIO.fail(AutoCompleteError.Help("supported: generate, complete"))
  }
}
