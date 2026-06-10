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

  def compose(parser: ArgsParser[?], helpType: HelpType, subCommands: Map[String, ?] = Map.empty, title: Option[String] = None): Help = {
    val sections = List(
      title.map(Help.Raw(_)),
      Some(parser.help),
      subCommandsHelp(subCommands),
      helpTypeExtra(helpType),
    ).flatten
    sections match
      case Nil        => Help.Empty
      case one :: Nil => one
      case many       => many.reduceLeft(Help.And(_, _))
  }

  def printHelp(parser: ArgsParser[?], helpType: HelpType, subCommands: Map[String, ?] = Map.empty, title: Option[String] = None): String =
    compose(parser, helpType, subCommands, title).toString

  private def helpTypeExtra(helpType: HelpType): Option[Help] = helpType match
    case HelpType.HelpExtra => None
    case HelpType.Help      => None

  private def subCommandsHelp(subCommands: Map[String, ?]): Option[Help] =
    if subCommands.isEmpty then None
    else Help.Raw(s"Commands:\n${subCommands.keys.toList.sorted.map(name => s"  $name").mkString("\n")}").some

  def paramNameCompletions(help: Help, value: String): List[String] =
    collectParamNames(help).filter(_.startsWith(value)).distinct.sorted

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