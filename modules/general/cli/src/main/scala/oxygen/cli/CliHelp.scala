package oxygen.cli

import oxygen.json.Json
import scala.annotation.tailrec

object CliHelp {

  /** A machine-readable view of a parser's params, for `OXYGEN_CLI_JSON` help output. */
  def paramsJson(help: Help): Json.Arr = {
    def shortStr(c: Option[Char]): List[(String, Json)] = c.toList.map(ch => "short" -> Json.string(ch.toString))

    def hintFields(subHelp: SubHelp, extra: List[HelpHint]): List[(String, Json)] = {
      val hints: List[HelpHint] = extra ::: subHelp.hints
      val docFields: List[(String, Json)] =
        if subHelp.docs.nonEmpty then List("doc" -> Json.arr(subHelp.docs.map(Json.string)*)) else Nil
      val hintFs: List[(String, Json)] = hints.flatMap {
        case HelpHint.Default(v)     => List("default" -> Json.string(v))
        case HelpHint.EnumValues(vs) => List("enum" -> Json.arr(vs.map(Json.string)*))
        case HelpHint.Optional       => List("optional" -> Json.boolean(true))
        case HelpHint.Repeated       => List("repeated" -> Json.boolean(true))
        case HelpHint.RepeatedNel    => List("repeated" -> Json.boolean(true))
        case _                       => Nil
      }
      (docFields ::: hintFs).distinctBy(_._1) // a hint can appear in both `subHelp.hints` and `extra`
    }

    def loop(h: Help, extra: List[HelpHint], acc: List[Json]): List[Json] = h match
      case Help.And(l, r)             => loop(r, extra, loop(l, extra, acc))
      case Help.Or(l, r)              => loop(r, extra, loop(l, extra, acc))
      case Help.WithHints(base, msgs) => loop(base, extra ::: msgs.toList, acc)
      case Help.Extra(inner)          => loop(inner, extra, acc)
      case Help.Positional(name, sh)  =>
        acc :+ Json.obj((List("kind" -> Json.string("positional"), "name" -> Json.string(name)) ::: hintFields(sh, extra))*)
      case Help.Named(long, short, _, sh) =>
        acc :+ Json.obj((List("kind" -> Json.string("named"), "long" -> Json.string(long)) ::: shortStr(short) ::: hintFields(sh, extra))*)
      case Help.Flag(long, short, sh) =>
        acc :+ Json.obj((List("kind" -> Json.string("flag"), "long" -> Json.string(long)) ::: shortStr(short) ::: hintFields(sh, extra))*)
      case Help.Toggle(trueName, falseName, shortNames, sh) =>
        val shorts: List[(String, Json)] = shortNames.toList.flatMap { case (t, f) => List("shortTrue" -> Json.string(t.toString), "shortFalse" -> Json.string(f.toString)) }
        acc :+ Json.obj((List("kind" -> Json.string("toggle"), "trueName" -> Json.string(trueName), "falseName" -> Json.string(falseName)) ::: shorts ::: hintFields(sh, extra))*)
      case _ => acc

    Json.arr(loop(help, Nil, Nil)*)
  }

  def peel(named: NamedArgs): (NamedArgs, Option[HelpType]) = {
    @tailrec
    def loop(queue: List[NamedArg], kept: List[NamedArg], found: Option[HelpType]): (NamedArgs, Option[HelpType]) =
      queue match
        // Built-in help is long-only (`--help` / `--help-extra`): no `-h`/`-H`, so the entire short-name
        // namespace belongs to the app and auto short-name derivation never has to reserve a letter.
        case LongNameArg(_, "help-extra", nested) :: tail if NamedArgNested.isEmpty(nested) =>
          loop(tail, kept, found.orElse(Some(HelpType.HelpExtra)))
        case LongNameArg(_, "help", nested) :: tail if NamedArgNested.isEmpty(nested) =>
          loop(tail, kept, found.orElse(Some(HelpType.Help)))
        case head :: tail => loop(tail, head :: kept, found)
        case Nil          => (NamedArgs(kept.reverse), found)

    loop(named.args, Nil, None)
  }

  def peelArgs(args: Args): (Args, Option[HelpType]) =
    peel(args.named) match
      case (stripped, ht) => (Args(args.positional, stripped), ht)

  val builtinFlags: List[String] = List("--help", "--help-extra")

  def builtinFlagCompletions(value: String): List[String] =
    builtinFlags.filter(_.startsWith(value))

  def mergeCompletions(builtins: List[String], rest: List[String]): List[String] =
    val (flags, nonFlags) = (builtins ::: rest).distinct.partition(_.startsWith("-"))
    nonFlags.sorted ::: flags.sorted

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
