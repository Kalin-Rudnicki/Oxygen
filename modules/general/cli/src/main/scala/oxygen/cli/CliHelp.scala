package oxygen.cli

import oxygen.predef.core.*
import scala.annotation.tailrec

object CliHelp {

  def peel(named: NamedArgs): (NamedArgs, Option[HelpType]) = {
    @tailrec
    def loop(queue: List[NamedArg], kept: List[NamedArg], found: Option[HelpType]): (NamedArgs, Option[HelpType]) =
      queue match
        case LongNameArg(_, "help-extra", nested) :: tail if NamedArgNested.isEmpty(nested) =>
          loop(tail, kept, found.orElse(Some(HelpType.HelpExtra)))
        case LongNameArg(_, "help", nested) :: tail if NamedArgNested.isEmpty(nested) =>
          loop(tail, kept, found.orElse(Some(HelpType.Help)))
        case ShortNameArg(_, 'H', nested) :: tail if NamedArgNested.isEmpty(nested) =>
          loop(tail, kept, found.orElse(Some(HelpType.HelpExtra)))
        case ShortNameArg(_, 'h', nested) :: tail if NamedArgNested.isEmpty(nested) =>
          loop(tail, kept, found.orElse(Some(HelpType.Help)))
        case MultiShortNameArg(_, _, 'H') :: tail =>
          loop(tail, kept, found.orElse(Some(HelpType.HelpExtra)))
        case MultiShortNameArg(_, _, 'h') :: tail =>
          loop(tail, kept, found.orElse(Some(HelpType.Help)))
        case head :: tail => loop(tail, head :: kept, found)
        case Nil          => (NamedArgs(kept.reverse), found)

    loop(named.args, Nil, None)
  }

  def peelArgs(args: Args): (Args, Option[HelpType]) =
    peel(args.named) match
      case (stripped, ht) => (Args(args.positional, stripped), ht)

  val builtinFlags: List[String] = List("--help", "--help-extra", "-h", "-H")

  def builtinFlagCompletions(value: String): List[String] =
    builtinFlags.filter(_.startsWith(value))

  def mergeCompletions(builtins: List[String], rest: List[String]): List[String] =
    (builtins ::: rest).distinct.sorted

  def compose(
      parser: ArgsParser[?],
      helpType: HelpType,
      subCommandNames: Set[String] = Set.empty,
      title: Option[Help] = None,
      expandedSubCommands: Option[Help] = None,
  ): Help = {
    val parserHelp: Help = helpType match
      case HelpType.Help      => parser.help.stripDocs
      case HelpType.HelpExtra => parser.help
    val subCommandsSection: Option[Help] =
      expandedSubCommands
        .map(help => Help.And(Help.BlankLine, help))
        .orElse(subCommandsHelp(subCommandNames))
    val sections = List(
      title,
      Some(parserHelp),
      subCommandsSection,
      helpTypeExtra(helpType),
    ).flatten
    sections match
      case Nil        => Help.Empty
      case one :: Nil => one
      case many       => many.reduceLeft(Help.And(_, _))
  }

  def printHelp(
      parser: ArgsParser[?],
      helpType: HelpType,
      subCommandNames: Set[String] = Set.empty,
      title: Option[Help] = None,
      expandedSubCommands: Option[Help] = None,
  ): String =
    compose(parser, helpType, subCommandNames, title, expandedSubCommands).toString

  private def helpTypeExtra(helpType: HelpType): Option[Help] = helpType match
    case HelpType.HelpExtra => None
    case HelpType.Help      => None

  private def subCommandsHelp(subCommandNames: Set[String]): Option[Help] =
    if subCommandNames.isEmpty then None
    else Help.Raw(s"Commands:\n${subCommandNames.toList.sorted.map(name => s"  $name").mkString("\n")}").some

  def paramNameCompletions(help: Help, value: String): List[String] =
    mergeCompletions(builtinFlagCompletions(value), collectParamNames(help).filter(_.startsWith(value)))

  private def collectParamNames(help: Help): List[String] = help match
    case Help.Named(longName, shortName, _, _) =>
      s"--$longName" :: shortName.map(c => s"-$c").toList
    case Help.Flag(longName, shortName, _) =>
      s"--$longName" :: shortName.map(c => s"-$c").toList
    case Help.Toggle(trueName, falseName, shortNames, _) =>
      val long = List(s"--$trueName", s"--$falseName")
      val short = shortNames match
        case Some((t, f)) => List(s"-$t", s"-$f")
        case None         => Nil
      long ::: short
    case Help.And(left, right)   => collectParamNames(left) ::: collectParamNames(right)
    case Help.Or(left, right)    => collectParamNames(left) ::: collectParamNames(right)
    case Help.WithHints(base, _) => collectParamNames(base)
    case Help.Extra(inner)       => collectParamNames(inner)
    case _                       => Nil

}
