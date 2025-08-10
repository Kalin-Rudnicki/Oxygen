package oxygen.cli

import oxygen.predef.core.*
import scala.annotation.tailrec

sealed trait Arg {
  val index: Int
}
object Arg {

  // =====| Types |=====

  sealed trait ValueLike extends Arg
  sealed trait ParamLike extends Arg {
    val name: SimpleName
    val values: List[Arg.ValueLike]
    val subIndex: Int = 0
  }
  object ParamLike {
    implicit val ordering: Ordering[ParamLike] = Ordering.by[ParamLike, Int](_.index).orElseBy(_.subIndex)
  }

  final case class Value(index: Int, value: String) extends Arg.ValueLike
  final case class Bracketed(index: Int, values: List[Arg.ValueLike], params: List[Arg.ParamLike]) extends Arg.ValueLike
  final case class ShortParamMulti(index: Int, override val subIndex: Int, name: ShortName) extends Arg.ParamLike {
    override val values: List[ValueLike] = Nil
  }
  final case class ScopedParam(index: Int, name: SimpleName, values: List[Arg.ValueLike]) extends Arg.ParamLike

  // =====| Helpers |=====

  def parse(stringArgs: List[String]): Either[String, (List[Arg.ValueLike], List[Arg.ParamLike])] =
    for {
      rawArgs <- stringArgs.traverse(RawArg.parse)
      groupedArgs <- GroupedArg.parse(rawArgs)
      bracketed <- parseBracketed(-1, groupedArgs)
    } yield (bracketed.values, bracketed.params)

  /**
    * Splits on the first occurrence of the string "--", returning args `(before the "--", after the "--")`.
    * If no "--" is found, returns `(Nil, all args)`.
    */
  def splitOn_--(stringArgs: List[String]): (List[String], List[String]) = {
    @tailrec
    def loop(queue: List[String], rStack: List[String]): (List[String], List[String]) = queue match
      case "--" :: tail => (rStack.reverse, tail)
      case head :: tail => loop(tail, head :: rStack)
      case Nil          => (Nil, stringArgs)

    loop(stringArgs, Nil)
  }

  @tailrec
  private def parseAllValueLike(
      queue: List[GroupedArg],
      stack: List[Arg.ValueLike],
  ): Either[String, (List[Arg.ValueLike], List[GroupedArg])] =
    queue match {
      case GroupedArg.Value(i, v) :: tail      => parseAllValueLike(tail, Arg.Value(i, v) :: stack)
      case GroupedArg.Bracketed(i, vs) :: tail =>
        parseBracketed(i, vs) match {
          case Right(value) => parseAllValueLike(tail, value :: stack)
          case Left(error)  => error.asLeft
        }
      case GroupedArg.ShortParams(_, _) :: _       => (stack.reverse, queue).asRight
      case GroupedArg.LongParam(_, _) :: _         => (stack.reverse, queue).asRight
      case GroupedArg.ParamWithValue(_, _, _) :: _ => (stack.reverse, queue).asRight
      case Nil                                     => (stack.reverse, queue).asRight
    }

  @tailrec
  private def parseAllParamLikes(
      queue: List[GroupedArg],
      stack: List[Arg.ParamLike],
  ): Either[String, List[Arg.ParamLike]] =
    queue match {
      case GroupedArg.ShortParams(i, NonEmptyList(name, Nil)) :: tail =>
        parseAllValueLike(tail, Nil) match {
          case Right((values, rest)) => parseAllParamLikes(rest, Arg.ScopedParam(i, name, values) :: stack)
          case Left(error)           => error.asLeft
        }
      case GroupedArg.LongParam(i, name) :: tail =>
        parseAllValueLike(tail, Nil) match {
          case Right((values, rest)) => parseAllParamLikes(rest, Arg.ScopedParam(i, name, values) :: stack)
          case Left(error)           => error.asLeft
        }
      case GroupedArg.ParamWithValue(i, name, value) :: tail =>
        parseAllParamLikes(
          tail,
          Arg.ScopedParam(i, name, Arg.Value(i, value) :: Nil) :: stack,
        )
      case GroupedArg.ShortParams(i, names) :: tail =>
        parseAllParamLikes(
          tail,
          names.toList.zipWithIndex.map { case (n, i2) => Arg.ShortParamMulti(i, i2, n) }.reverse ::: stack,
        )
      case a :: _ =>
        // TODO (KR) : show value?
        s"Unexpected value at index ${a.index}".asLeft
      case Nil =>
        stack.reverse.asRight
    }

  private def parseBracketed(i: Int, args: List[GroupedArg]): Either[String, Arg.Bracketed] =
    for {
      (values, rest) <- parseAllValueLike(args, Nil)
      params <- parseAllParamLikes(rest, Nil)
    } yield Arg.Bracketed(i, values, params)

  // =====| Internal Representations |=====

  private sealed trait RawArg
  private object RawArg {

    sealed trait Simple extends RawArg

    final case class Value(value: String) extends RawArg.Simple
    final case class ShortParams(names: NonEmptyList[ShortName]) extends RawArg.Simple
    final case class LongParam(name: LongName) extends RawArg.Simple
    final case class ParamWithValue(name: SimpleName, value: String) extends RawArg.Simple
    case object OpenBrace extends RawArg
    case object CloseBrace extends RawArg

    private val escRegex = "^\\\\(.+)$".r
    private val longWithValueRegex = "^--([A-Za-z0-9]+(?:-[A-Za-z0-9]+)*)=(.*)$".r
    private val longRegex = "^--([A-Za-z0-9]+(?:-[A-Za-z0-9]+)*)$".r
    private val shortWithValueRegex = "^-([A-Za-z0-9])=(.*)$".r
    private val shortRegex = "^-([A-Za-z0-9]+)$".r

    def parse(string: String): Either[String, RawArg] = string match
      case escRegex(value)                             => Value(value).asRight
      case longWithValueRegex(LongName(name), value)   => name.map(ParamWithValue(_, value))
      case longRegex(LongName(name))                   => name.map(LongParam(_))
      case shortWithValueRegex(ShortName(name), value) => name.map(ParamWithValue(_, value))
      case shortRegex(ShortName.many(names))           => names.map(ShortParams(_))
      case "{"                                         => OpenBrace.asRight
      case "}"                                         => CloseBrace.asRight
      case _                                           => Value(string).asRight

  }

  private sealed trait GroupedArg {
    val index: Int
  }
  private object GroupedArg {

    final case class Value(index: Int, value: String) extends GroupedArg
    final case class Bracketed(index: Int, values: List[GroupedArg]) extends GroupedArg
    final case class ShortParams(index: Int, names: NonEmptyList[ShortName]) extends GroupedArg
    final case class LongParam(index: Int, name: LongName) extends GroupedArg
    final case class ParamWithValue(index: Int, name: SimpleName, value: String) extends GroupedArg

    private def fromSimple(i: Int, arg: RawArg.Simple): GroupedArg = arg match
      case RawArg.Value(value)                => GroupedArg.Value(i, value)
      case RawArg.ShortParams(names)          => GroupedArg.ShortParams(i, names)
      case RawArg.LongParam(name)             => GroupedArg.LongParam(i, name)
      case RawArg.ParamWithValue(name, value) => GroupedArg.ParamWithValue(i, name, value)

    private def parseGrouped(
        i: Int,
        queue: List[(RawArg, Int)],
        stack: List[GroupedArg],
    ): Either[String, (GroupedArg.Bracketed, List[(RawArg, Int)])] =
      queue match {
        case (simple: RawArg.Simple, i2) :: tail =>
          parseGrouped(i, tail, fromSimple(i2, simple) :: stack)
        case (RawArg.CloseBrace, _) :: tail =>
          (GroupedArg.Bracketed(i, stack.reverse), tail).asRight
        case (RawArg.OpenBrace, i2) :: tail =>
          parseGrouped(i2, tail, Nil) match {
            case Right((value, rest)) => parseGrouped(i2, rest, value :: stack)
            case Left(error)          => error.asLeft
          }
        case Nil =>
          s"Missing closing brace for opening brace at index $i".asLeft
      }

    private def parseRoot(
        queue: List[(RawArg, Int)],
        stack: List[GroupedArg],
    ): Either[String, List[GroupedArg]] =
      queue match {
        case (simple: RawArg.Simple, i) :: tail =>
          parseRoot(tail, fromSimple(i, simple) :: stack)
        case Nil =>
          stack.reverse.asRight
        case (RawArg.OpenBrace, i) :: tail =>
          parseGrouped(i, tail, Nil) match {
            case Right((value, rest)) => parseRoot(rest, value :: stack)
            case Left(error)          => error.asLeft
          }
        case (RawArg.CloseBrace, i) :: _ =>
          s"Unexpected closing brace at index $i".asLeft
      }

    def parse(args: List[RawArg]): Either[String, List[GroupedArg]] =
      parseRoot(args.zipWithIndex, Nil)

  }

}
