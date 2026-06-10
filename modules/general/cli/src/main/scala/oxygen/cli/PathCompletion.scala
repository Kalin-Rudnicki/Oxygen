package oxygen.cli

import zio.*

private[cli] object PathCompletion {

  def parentAndPrefix(in: String): (String, String) =
    if in.isEmpty then ("", "")
    else
      val normalized = in.replace('\\', '/')
      if normalized.endsWith("/") then (normalized, "")
      else
        val sep = normalized.lastIndexOf('/')
        if sep < 0 then ("", normalized)
        else (normalized.substring(0, sep + 1), normalized.substring(sep + 1))

  def buildCompletions(parent: String, prefix: String, entries: Seq[String], isDirectory: String => Boolean): Seq[String] =
    val matched =
      val filtered = CompletionOptions.filterPrefix(entries, prefix)
      filtered.map { name =>
        val value = s"$parent$name"
        if isDirectory(name) then s"$value/" else value
      }
    matched

  /** When a single directory match is unambiguous, list inside it (recursively while still unique). */
  def expandSingleMatch(completions: Seq[String], listAt: String => Seq[String]): Seq[String] =
    @annotation.tailrec
    def loop(current: Seq[String]): Seq[String] =
      current match
        case Seq(single) if single.endsWith("/") =>
          listAt(single) match
            case Nil               => current
            case expanded @ Seq(_) => loop(expanded)
            case expanded          => expanded
        case _ => current
    loop(completions)

  def completeUnderBase(basePath: String, in: String): Task[Seq[String]] =
    PathCompletionPlatform.completeUnderBase(basePath, in)

}
