package oxygen.zio

import oxygen.core.syntax.ior.*
import oxygen.predef.core.*
import scala.annotation.tailrec
import zio.*
import zio.stream.*

/**
  * [[SparseStreamAggregator]] serves the purpose of being able to aggregate a stream of sparse data.
  *
  * Example:
  *
  * Input:
  * - (1.some, None, None)
  * - (2.some, None, None)
  * - (None, true.some, None)
  * - (3.some, None, None)
  * - (None, None, "A".some)
  * - (4.some, None, None)
  * - (None, false.some, None)
  * - (None, None, "B".some)
  * - (None, None, "C".some)
  * - (None, None, "D".some)
  *
  * Output:
  * - (1, None, Contiguous())
  * - (2, true.some, Contiguous())
  * - (3, None, Contiguous("A"))
  * - (4, false.some, Contiguous("B", "C", "D"))
  */
trait SparseStreamAggregator[_RawInput, _Output] {

  /////// Types ///////////////////////////////////////////////////////////////

  final type RawInput = _RawInput
  final type Output = _Output

  protected type _Input
  protected type _State

  final type Input = _Input
  final type State = _State

  def accept(input: Input): SparseStreamAggregator.Attempt.Required[Ior.Distinct[State, Output]]
  def accept(state: State, input: Input): SparseStreamAggregator.Attempt.Required[Ior[State, Output]]
  def complete(state: State): SparseStreamAggregator.Attempt.Required[Output]
  def complete(): SparseStreamAggregator.Attempt.Required[Output]

  /////// Abstract Methods ///////////////////////////////////////////////////////////////

  def parseInput(raw: RawInput): SparseStreamAggregator.Attempt.Optional[Input]

  /////// Final Methods ///////////////////////////////////////////////////////////////

  final def accept(state: Option[State], input: Input): SparseStreamAggregator.Attempt.Required[Ior[State, Output]] = state match
    case Some(state) => accept(state, input)
    case None        => accept(input)
  final def complete(state: Option[State]): SparseStreamAggregator.Attempt.Required[Output] = state match
    case Some(state) => complete(state)
    case None        => complete()

  final def optional: SparseStreamAggregator[RawInput, Option[Output]] = SparseStreamAggregator.Optional(this)
  final def many[S[_]: SeqOps]: SparseStreamAggregator[RawInput, S[Output]] = SparseStreamAggregator.Many[RawInput, Output, S](this)

  /**
    * Note: When combining multiple aggs, the LHS will be eagerly emitted.
    *       This means that if you do `(a *: b) *: c` instead of `a *: b *: c` (which is the same as `a *: (b *: c)`,
    *       then you will get errors, because it will be impossible to ever get a `c`.
    */
  final def *:[RawInput2, Output2](
      that: SparseStreamAggregator[RawInput2, Output2],
  )(using zipInput: Zip[RawInput2, RawInput], zipOutput: Zip[Output2, Output]): SparseStreamAggregator[zipInput.Out, zipOutput.Out] =
    SparseStreamAggregator.AndThen(that, this, zipInput, zipOutput)

  final def toPipeline: ZPipeline[Any, Nothing, RawInput, Output] =
    ZPipeline.fromChannel(SparseStreamAggregator.aggregatorChannel(this))

  final def aggregateStream[R, E](stream: ZStream[R, E, RawInput]): ZStream[R, E, Output] =
    stream >>> toPipeline

}
object SparseStreamAggregator {

  def of[A]: SparseStreamAggregator[Option[A], A] = new Leaf[A]

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Impls
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class Leaf[A] extends SparseStreamAggregator[Option[A], A] {

    override protected type _Input = A
    override protected type _State = Nothing

    override def parseInput(raw: RawInput): Attempt.Optional[A] = raw match
      case Some(value) => Attempt.Value(value)
      case None        => Attempt.NoValue

    override def accept(input: Input): Attempt.Required[Ior.Distinct[State, Output]] = Attempt.Value(input.asIorRight)

    override def accept(state: State, input: Input): Attempt.Required[Ior[State, Output]] = Attempt.Invalid // not possible, Input=Nothing

    override def complete(state: State): Attempt.Required[Output] = Attempt.Invalid // not possible, Input=Nothing

    override def complete(): Attempt.Required[Output] = Attempt.Invalid

  }

  final case class Optional[_RawInput, _Output](inner: SparseStreamAggregator[_RawInput, _Output]) extends SparseStreamAggregator[_RawInput, Option[_Output]] {

    override protected type _Input = inner.Input
    override protected type _State = inner.State

    override def accept(input: Input): Attempt.Required[Ior.Distinct[State, Output]] =
      inner.accept(input).map(_.map(_.some))

    override def accept(state: State, input: Input): Attempt.Required[Ior[State, Output]] =
      inner.accept(state, input).map(_.map(_.some))

    override def complete(state: State): Attempt.Required[Output] =
      inner.complete(state).map(_.some)

    override def complete(): Attempt.Required[Output] =
      Attempt.Value(None)

    override def parseInput(raw: RawInput): Attempt.Optional[Input] =
      inner.parseInput(raw)

  }

  final case class Many[_RawInput, _Output, S[_]: SeqOps as seqOps](inner: SparseStreamAggregator[_RawInput, _Output]) extends SparseStreamAggregator[_RawInput, S[_Output]] {

    override protected type _Input = inner.Input
    override protected type _State = (Option[inner.State], Growable[inner.Output])

    private def acceptShared(state1: Option[inner.State], acc: Growable[inner.Output], input: Input): Attempt.Required[Ior.Distinct[State, Nothing]] =
      inner.accept(state1, input).map {
        case Ior.Left(state2)         => (state2.some, acc).asIorLeft
        case Ior.Right(output)        => (None, acc :+ output).asIorLeft
        case Ior.Both(state2, output) => (state2.some, acc :+ output).asIorLeft
      }

    override def accept(input: Input): Attempt.Required[Ior.Distinct[State, Output]] = acceptShared(None, Growable.empty, input)
    override def accept(state: State, input: Input): Attempt.Required[Ior[State, Output]] = acceptShared(state._1, state._2, input)

    override def complete(state: State): Attempt.Required[Output] =
      state._1 match
        case Some(state1) => inner.complete(state1).map { output => (state._2 :+ output).to[S] }
        case None         => Attempt.Value(state._2.to[S])

    override def complete(): Attempt.Required[Output] =
      Attempt.Value(seqOps.newBuilder[inner.Output].result())

    override def parseInput(raw: RawInput): Attempt.Optional[Input] =
      inner.parseInput(raw)

  }

  final case class AndThen[_RawInput1, _Output1, _RawInput2, _Output2, _ZipInput, _ZipOutput](
      _1: SparseStreamAggregator[_RawInput1, _Output1],
      _2: SparseStreamAggregator[_RawInput2, _Output2],
      zipInput: Zip.Out[_RawInput1, _RawInput2, _ZipInput],
      zipOutput: Zip.Out[_Output1, _Output2, _ZipOutput],
  ) extends SparseStreamAggregator[_ZipInput, _ZipOutput] {

    override protected type _Input = Either[_1.Input, _2.Input]
    override protected type _State = Either[_1.State, (_1.Output, Option[_2.State])]

    override def accept(input: Input): Attempt.Required[Ior.Distinct[State, Output]] =
      input match {
        case Left(_1Input) =>
          _1.accept(_1Input).map {
            case Ior.Left(_1State2)  => _1State2.asLeft.asIorLeft
            case Ior.Right(_1Output) => (_1Output, None).asRight.asIorLeft
          }
        case Right(_2Input) =>
          _1.complete().flatMap { _1Output =>
            _2.accept(_2Input).map {
              case Ior.Left(_2State2)  => (_1Output, _2State2.some).asRight.asIorLeft
              case Ior.Right(_2Output) => zipOutput.zip(_1Output, _2Output).asIorRight
            }
          }
      }

    override def accept(state: State, input: Input): Attempt.Required[Ior[State, Output]] = {
      (state, input) match {
        case (Left(_1State1), Left(_1Input)) =>
          _1.accept(_1State1, _1Input).flatMap {
            case Ior.Left(_1State2) =>
              Attempt.Value(_1State2.asLeft.asIorLeft)
            case Ior.Right(_1Output) =>
              _2.complete().map { _2Output => zipOutput.zip(_1Output, _2Output).asIorRight }
            case Ior.Both(_1State2, _1Output) =>
              _2.complete().map { _2Output => Ior.Both(_1State2.asLeft, zipOutput.zip(_1Output, _2Output)) }
          }
        case (Right((_1Output1, opt_2State1)), Left(_1Input)) =>
          _2.complete(opt_2State1).flatMap { _2Output =>
            _1.accept(_1Input).map {
              case Ior.Left(_1State2)   => Ior.Both(_1State2.asLeft, zipOutput.zip(_1Output1, _2Output))
              case Ior.Right(_1Output2) => Ior.Both((_1Output2, None).asRight, zipOutput.zip(_1Output1, _2Output))
            }
          }
        case (Left(_1State1), Right(_2Input)) =>
          _1.complete(_1State1).flatMap { _1Output =>
            _2.accept(_2Input).map {
              case Ior.Left(_2State2)  => (_1Output, _2State2.some).asRight.asIorLeft
              case Ior.Right(_2Output) => zipOutput.zip(_1Output, _2Output).asIorRight
            }
          }
        case (Right((_1Output1, opt_2State1)), Right(_2Input)) =>
          _2.accept(opt_2State1, _2Input).flatMap {
            case Ior.Left(_2State2)           => Attempt.Value((_1Output1, _2State2.some).asRight.asIorLeft)
            case Ior.Right(_2Output)          => Attempt.Value(zipOutput.zip(_1Output1, _2Output).asIorRight)
            case Ior.Both(_2State2, _2Output) => _1.complete().map { _1Output2 => Ior.Both((_1Output2, _2State2.some).asRight, zipOutput.zip(_1Output1, _2Output)) }
          }
      }
    }

    override def complete(state: State): Attempt.Required[Output] =
      state match {
        case Left(_1State) =>
          for {
            _1Output <- _1.complete(_1State)
            _2Output <- _2.complete()
          } yield zipOutput.zip(_1Output, _2Output)
        case Right((_1Output, opt_2State)) =>
          for {
            _2Output <- _2.complete(opt_2State)
          } yield zipOutput.zip(_1Output, _2Output)
      }

    override def complete(): Attempt.Required[Output] =
      for {
        _1Output <- _1.complete()
        _2Output <- _2.complete()
      } yield zipOutput.zip(_1Output, _2Output)

    override def parseInput(raw: RawInput): Attempt.Optional[Input] = {
      val (raw1, raw2) = zipInput.unzip(raw)
      (_1.parseInput(raw1), _2.parseInput(raw2)) match {
        case (Attempt.Value(v1), Attempt.NoValue) => Attempt.Value(v1.asLeft)
        case (Attempt.NoValue, Attempt.Value(v2)) => Attempt.Value(v2.asRight)
        case (Attempt.NoValue, Attempt.NoValue)   => Attempt.NoValue
        case _                                    => Attempt.Invalid
      }
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object Attempt {

    sealed trait Optional[+A] {

      final def toRequired: Required[A] = this match
        case required: Required[A] => required
        case NoValue               => Invalid

    }

    sealed trait Required[+A] extends Optional[A] {

      final def map[B](f: A => B): Attempt.Required[B] = this match
        case Value(value) => Attempt.Value(f(value))
        case Invalid      => Invalid

      final def flatMap[B](f: A => Attempt.Required[B]): Attempt.Required[B] = this match
        case Value(value) => f(value)
        case Invalid      => Invalid

    }

    final case class Value[+A](value: A) extends Required[A]
    case object NoValue extends Optional[Nothing]
    case object Invalid extends Required[Nothing]

  }

  private def aggregatorChannel[E, RawInput, Output](agg: SparseStreamAggregator[RawInput, Output]): ZChannel[Any, E, Chunk[RawInput], Any, E, Chunk[Output], Any] = {
    type State = Option[agg.State]

    def handleChunk(state1: State, inChunk: Chunk[RawInput]): (Either[String, State], Chunk[Output]) = {
      val builder = Chunk.newBuilder[Output]

      val len = inChunk.length
      @tailrec
      def loop(idx: Int, state1: State): (Either[String, State], Chunk[Output]) =
        if (idx < len) {
          val rawIn = inChunk(idx)

          agg.parseInput(rawIn) match {
            case Attempt.Value(parsedIn) =>
              agg.accept(state1, parsedIn) match {
                case Attempt.Value(value) =>
                  value match {
                    case Ior.Left(state2)         => loop(idx + 1, state2.some)
                    case Ior.Right(output)        => builder.addOne(output); loop(idx + 1, None)
                    case Ior.Both(state2, output) => builder.addOne(output); loop(idx + 1, state2.some)
                  }
                case Attempt.Invalid =>
                  (
                    s"""Unable to apply input:
                       |  rawInput: $rawIn
                       |  parsedInput: $parsedIn
                       |  state: $state1""".stripMargin.asLeft,
                    builder.result(),
                  )
              }
            case Attempt.Invalid =>
              (
                s"""Unable to parse input:
                   |  rawInput: $rawIn
                   |  state: $state1""".stripMargin.asLeft,
                builder.result(),
              )
            case Attempt.NoValue =>
              (
                s"""Input has no value:
                   |  rawInput: $rawIn
                   |  state: $state1""".stripMargin.asLeft,
                builder.result(),
              )
          }
        } else
          (state1.asRight, builder.result())

      loop(0, state1)
    }

    def rec(state1: State): ZChannel[Any, E, Chunk[RawInput], Any, E, Chunk[Output], Unit] =
      ZChannel.readWithCause[Any, E, Chunk[RawInput], Any, E, Chunk[Output], Unit](
        { inChunk =>
          val (state2, outChunk) = handleChunk(state1, inChunk)

          (state2, outChunk.nonEmpty) match {
            case (Right(state2), true)  => ZChannel.write(outChunk) *> rec(state2)
            case (Right(state2), false) => rec(state2)
            case (Left(invalid), true)  => ZChannel.write(outChunk) *> ZChannel.fromZIO { ZIO.dieMessage(invalid) } // TODO (KR) : improve this
            case (Left(invalid), false) => ZChannel.write(outChunk) *> ZChannel.fromZIO { ZIO.dieMessage(invalid) } // TODO (KR) : improve this
          }
        },
        ZChannel.refailCause,
        _ =>
          state1 match {
            case Some(state1) =>
              agg.complete(state1) match {
                case Attempt.Value(output) => ZChannel.write(Chunk.single(output)) *> ZChannel.unit
                case Attempt.Invalid       => ZChannel.fromZIO { ZIO.dieMessage(s"Invalid input: EOF, state=$state1") } // TODO (KR) : improve this
              }
            case None =>
              ZChannel.unit
          },
      )

    rec(None)
  }

}
