package oxygen.cli

import oxygen.predef.core.*
import scala.annotation.tailrec

sealed trait Arg {
  val idx: Int
}

final case class PositionalArg(idx: Int, value: String) extends Arg

sealed trait NamedArg extends Arg

final case class LongNameArg(idx: Int, name: String, nested: Any) extends NamedArg

final case class ShortNameArg(idx: Int, name: Char, nested: Any) extends NamedArg

final case class MultiShortNameArg(idx: Int, subIdx: Int, name: Char) extends NamedArg

object NamedArgNested {

  val empty: PositionalArgs = PositionalArgs(Nil)

  def values(args: List[PositionalArg]): PositionalArgs = PositionalArgs(args)

  def positional(nested: Any): PositionalArgs = nested.asInstanceOf[PositionalArgs]

  def isEmpty(nested: Any): Boolean = positional(nested).args.isEmpty

}
object Arg {

  def parse(stringArgs: List[String]): Either[String, Args] =
    for {
      (before, after) <- splitOn_--(stringArgs)
      rawBefore <- before.traverse(RawArg.parse)
      rawAfter <- after.traverse(RawArg.parse)
      groupedBefore <- GroupedArg.parse(rawBefore)
      groupedAfter <- GroupedArg.parse(rawAfter)
      (positional, named) <- classify(groupedBefore)
      positionalAfter <- classifyPositional(groupedAfter)
    } yield Args(PositionalArgs(positional ::: positionalAfter), NamedArgs(named))

  def splitOnRaw_--(stringArgs: List[String]): (Boolean, List[String], List[String]) = {
    @tailrec
    def loop(queue: List[String], rStack: List[String]): (Boolean, List[String], List[String]) = queue match
      case "--" :: tail => (true, rStack.reverse, tail)
      case head :: tail => loop(tail, head :: rStack)
      case Nil          => (false, Nil, stringArgs)

    loop(stringArgs, Nil)
  }

  def splitOn_--(stringArgs: List[String]): Either[String, (List[String], List[String])] = {
    val (_, a, b) = splitOnRaw_--(stringArgs)
    (a, b).asRight
  }

  private def classify(grouped: List[GroupedArg]): Either[String, (List[PositionalArg], List[NamedArg])] = {
    @tailrec
    def loop(
        queue: List[GroupedArg],
        positional: List[PositionalArg],
        named: List[NamedArg],
    ): Either[String, (List[PositionalArg], List[NamedArg])] =
      queue match
        case GroupedArg.Value(i, v) :: tail =>
          loop(tail, PositionalArg(i, v) :: positional, named)
        case GroupedArg.Bracketed(i, inner) :: tail =>
          parseBracketed(inner) match
            case Right(value) => loop(tail, PositionalArg(i, value) :: positional, named)
            case Left(error)  => error.asLeft
        case GroupedArg.ShortParams(i, names) :: tail =>
          loop(tail, positional, names.toList.zipWithIndex.map { case (n, i2) => MultiShortNameArg(i, i2, n) }.reverse ::: named)
        case GroupedArg.LongParam(i, name) :: tail =>
          parseNamedValues(tail) match
            case Right((values, rest)) =>
              loop(rest, positional, LongNameArg(i, name, NamedArgNested.values(values)) :: named)
            case Left(error) => error.asLeft
        case GroupedArg.ShortParam(i, name) :: tail =>
          parseNamedValues(tail) match
            case Right((values, rest)) =>
              loop(rest, positional, ShortNameArg(i, name, NamedArgNested.values(values)) :: named)
            case Left(error) => error.asLeft
        case GroupedArg.ParamWithValue(i, name, value) :: tail =>
          loop(tail, positional, LongNameArg(i, name, NamedArgNested.values(PositionalArg(i, value) :: Nil)) :: named)
        case GroupedArg.ShortParamWithValue(i, name, value) :: tail =>
          loop(tail, positional, ShortNameArg(i, name, NamedArgNested.values(PositionalArg(i, value) :: Nil)) :: named)
        case Nil =>
          (positional.reverse, named.reverse).asRight

    loop(grouped, Nil, Nil)
  }

  private def classifyPositional(grouped: List[GroupedArg]): Either[String, List[PositionalArg]] =
    classify(grouped).map(_._1)

  private def parseNamedValues(queue: List[GroupedArg]): Either[String, (List[PositionalArg], List[GroupedArg])] = {
    @tailrec
    def loop(
        q: List[GroupedArg],
        stack: List[PositionalArg],
    ): Either[String, (List[PositionalArg], List[GroupedArg])] =
      q match
        case GroupedArg.Value(i, v) :: tail       => loop(tail, PositionalArg(i, v) :: stack)
        case GroupedArg.Bracketed(i, inner) :: tail =>
          parseBracketed(inner) match
            case Right(value) => loop(tail, PositionalArg(i, value) :: stack)
            case Left(error)  => error.asLeft
        case _ => (stack.reverse, q).asRight

    loop(queue, Nil)
  }

  private def parseBracketed(args: List[GroupedArg]): Either[String, String] =
    for {
      (positional, named) <- classify(args)
    } yield BracketedRepr(positional, named).render

  private final case class BracketedRepr(positional: List[PositionalArg], named: List[NamedArg]) {

    def render: String = {
      val parts: List[String] =
        positional.map(_.value) :::
          named.flatMap {
            case LongNameArg(_, name, nested)  => renderNamed(s"--$name", nested)
            case ShortNameArg(_, name, nested) => renderNamed(s"-$name", nested)
            case MultiShortNameArg(_, _, name) => s"-$name" :: Nil
          }
      s"{${parts.mkString(" ")}}"
    }

    private def renderNamed(prefix: String, nested: Any): List[String] =
      NamedArgNested.positional(nested).args match
        case Nil  => prefix :: Nil
        case args => s"$prefix=${args.map(_.value).mkString(" ")}" :: Nil

  }

  private sealed trait RawArg
  private object RawArg {

    sealed trait Simple extends RawArg

    final case class Value(value: String) extends RawArg.Simple
    final case class ShortParams(names: NonEmptyList[Char]) extends RawArg.Simple
    final case class LongParam(name: String) extends RawArg.Simple
    final case class ShortParam(name: Char) extends RawArg.Simple
    final case class ParamWithValue(name: String, value: String) extends RawArg.Simple
    final case class ShortParamWithValue(name: Char, value: String) extends RawArg.Simple
    case object OpenBrace extends RawArg
    case object CloseBrace extends RawArg

    private val escRegex = "^\\\\(.+)$".r
    private val longWithValueRegex = "^--([A-Za-z0-9]+(?:-[A-Za-z0-9]+)*)=(.*)$".r
    private val longRegex = "^--([A-Za-z0-9]+(?:-[A-Za-z0-9]+)*)$".r
    private val shortWithValueRegex = "^-([A-Za-z0-9])=(.*)$".r
    private val shortRegex = "^-([A-Za-z0-9]+)$".r

    def parse(string: String): Either[String, RawArg] = string match
      case escRegex(value)                              => Value(value).asRight
      case longWithValueRegex(name, value)              => ParamWithValue(name, value).asRight
      case longRegex(name)                              => LongParam(name).asRight
      case shortWithValueRegex(name, value)             => ShortParamWithValue(name.head, value).asRight
      case shortRegex(chars) if chars.length == 1       => ShortParam(chars.head).asRight
      case shortRegex(chars)                            => ShortParams(NonEmptyList.unsafeFromList(chars.toList)).asRight
      case "{"                                          => OpenBrace.asRight
      case "}"                                          => CloseBrace.asRight
      case _                                            => Value(string).asRight

  }

  private sealed trait GroupedArg {
    val index: Int
  }
  private object GroupedArg {

    final case class Value(index: Int, value: String) extends GroupedArg
    final case class Bracketed(index: Int, values: List[GroupedArg]) extends GroupedArg
    final case class ShortParams(index: Int, names: NonEmptyList[Char]) extends GroupedArg
    final case class LongParam(index: Int, name: String) extends GroupedArg
    final case class ShortParam(index: Int, name: Char) extends GroupedArg
    final case class ParamWithValue(index: Int, name: String, value: String) extends GroupedArg
    final case class ShortParamWithValue(index: Int, name: Char, value: String) extends GroupedArg

    private def fromSimple(i: Int, arg: RawArg.Simple): GroupedArg = arg match
      case RawArg.Value(value)                     => GroupedArg.Value(i, value)
      case RawArg.ShortParams(names)               => GroupedArg.ShortParams(i, names)
      case RawArg.LongParam(name)                  => GroupedArg.LongParam(i, name)
      case RawArg.ShortParam(name)                 => GroupedArg.ShortParam(i, name)
      case RawArg.ParamWithValue(name, value)      => GroupedArg.ParamWithValue(i, name, value)
      case RawArg.ShortParamWithValue(name, value) => GroupedArg.ShortParamWithValue(i, name, value)

    private def parseGrouped(
        i: Int,
        queue: List[(RawArg, Int)],
        stack: List[GroupedArg],
    ): Either[String, (GroupedArg.Bracketed, List[(RawArg, Int)])] =
      queue match
        case (simple: RawArg.Simple, i2) :: tail =>
          parseGrouped(i, tail, fromSimple(i2, simple) :: stack)
        case (RawArg.CloseBrace, _) :: tail =>
          (GroupedArg.Bracketed(i, stack.reverse), tail).asRight
        case (RawArg.OpenBrace, i2) :: tail =>
          parseGrouped(i2, tail, Nil) match
            case Right((value, rest)) => parseGrouped(i2, rest, value :: stack)
            case Left(error)          => error.asLeft
        case Nil =>
          s"Missing closing brace for opening brace at index $i".asLeft

    private def parseRoot(
        queue: List[(RawArg, Int)],
        stack: List[GroupedArg],
    ): Either[String, List[GroupedArg]] =
      queue match
        case (simple: RawArg.Simple, i) :: tail =>
          parseRoot(tail, fromSimple(i, simple) :: stack)
        case Nil =>
          stack.reverse.asRight
        case (RawArg.OpenBrace, i) :: tail =>
          parseGrouped(i, tail, Nil) match
            case Right((value, rest)) => parseRoot(rest, value :: stack)
            case Left(error)          => error.asLeft
        case (RawArg.CloseBrace, i) :: _ =>
          s"Unexpected closing brace at index $i".asLeft

    private[Arg] def parse(args: List[RawArg]): Either[String, List[GroupedArg]] =
      parseRoot(args.zipWithIndex, Nil)

  }

}