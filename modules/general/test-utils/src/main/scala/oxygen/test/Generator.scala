package oxygen.test

import java.time.*
import java.util.UUID
import oxygen.meta.K0.*
import oxygen.predef.core.*
import oxygen.zio.instances.{*, given}
import scala.quoted.{Expr, Quotes, Type}
import zio.*
import zio.stream.*
import zio.test.Sized

sealed trait Generator[+A] { self =>

  // =====| Stream |=====

  def streamN(n: Int): UStream[A]
  final def streamSized: UStream[A] = ZStream.fromZIO(Sized.size).flatMap(streamN)

  def streamExhaustiveOrSized: UStream[A]

  // =====| Gen |=====

  def gen: UIO[A]
  def genN(n: Int): UIO[Chunk[A]]

  final def genExhaustiveOrSized: UIO[Chunk[A]] = streamExhaustiveOrSized.runCollect

  final def genExhaustiveOrSizedWithSize(size: Int): UIO[Chunk[A]] = Sized.withSize(size) { streamExhaustiveOrSized.runCollect }

  // =====| ops |=====

  final def concat[A2 >: A](that: Generator[A2]): Generator[A2] =
    (self, that) match
      case (self: Generator.Bounded[A], that: Generator.Bounded[A2]) => self.concatBounded(that)
      case _                                                         => Generator.ConcatUnbounded(self, that)

  final def ++[A2 >: A](that: Generator[A2]): Generator[A2] = concat(that)

  def map[B](f: A => B): Generator[B] = Generator.MapUnbounded(this, f)

  final def flatMap[B](f: A => Generator[B]): Generator[B] = Generator.FlatMapUnbounded(this, f)

}
object Generator extends GeneratorLowPriority.LowPriority1, Derivable[Generator.Bounded] {

  def const[A](value: A): Generator.Finite[A] = finite(value)
  def finite[A](values: A*): Generator.Finite[A] = Finite(Chunk.from(values))
  def random[A](eff: UIO[A]): Generator.Rand[A] = Rand(eff)

  def apply[A: Generator as gen]: Generator[A] = gen

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given byte: Generator.Rand[Byte] = random(Random.nextInt.map(_.toByte))
  given short: Generator.Rand[Short] = random(Random.nextInt.map(_.toShort))
  given int: Generator.Rand[Int] = random(Random.nextInt)
  given long: Generator.Rand[Long] = random(Random.nextLong)
  given bigInt: Generator.Rand[BigInt] = random(Random.nextLong.map(BigInt(_)))

  given float: Generator.Rand[Float] = random(Random.nextFloat)
  given double: Generator.Rand[Double] = random(Random.nextDouble)
  given bigDecimal: Generator.Rand[BigDecimal] = random(Random.nextDouble.map(BigDecimal(_)))

  given boolean: Generator.Finite[Boolean] = finite(true, false)
  given string: Generator.Rand[String] = random(RandomGen.lowerCaseString())
  given uuid: Generator.Rand[UUID] = random(Random.nextUUID)

  given localDate: Generator.Rand[LocalDate] = random(RandomGen.localDateBetween())
  given localDateTime: Generator.Rand[LocalDateTime] = random(RandomGen.localDateTimeBetween())
  given zonedDateTime: Generator.Rand[ZonedDateTime] = random(RandomGen.zonedDateTimeBetween())
  given offsetDateTime: Generator.Rand[OffsetDateTime] = random(RandomGen.offsetDateTimeBetween())
  given instant: Generator.Rand[Instant] = random(RandomGen.instantBetween())

  given finiteOption: [A: Generator.Finite as g] => Generator.Finite[Option[A]] =
    g.map(_.some) ++ Generator.finite(None)

  given finiteEither: [A: Generator.Finite as ga, B: Generator.Finite as gb] => Generator.Finite[Either[A, B]] =
    ga.map(_.asLeft) ++ gb.map(_.asRight)

  given seq: [S[_]: SeqOps as seqOps, A: Generator as g] => Generator[S[A]] = OfSeq(seqOps, g)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def streamZioChunk[A](zio: UIO[Chunk[A]]): UStream[A] =
    ZStream.fromZIO(zio).flatMap(ZStream.fromChunk)

  /////// Bounded ///////////////////////////////////////////////////////////////

  sealed trait Bounded[+A] extends Generator[A] { self =>

    def streamExhaustive: UStream[A]
    final def genExhaustive: UIO[Chunk[A]] = streamExhaustive.runCollect

    final def genExhaustiveWithSize(size: Int): UIO[Chunk[A]] = Sized.withSize(size) { streamExhaustive.runCollect }

    override final def streamExhaustiveOrSized: UStream[A] = streamExhaustive

    override def gen: UIO[A] = genExhaustive.flatMap(RandomGen.oneOf)
    override def genN(n: Int): UIO[Chunk[A]] = genExhaustive.flatMap(RandomGen.oneOfN(_, n))

    override final def streamN(n: Int): UStream[A] = streamZioChunk { genN(n) }

    final def concatBounded[A2 >: A](that: Generator.Bounded[A2]): Generator.Bounded[A2] =
      (self, that) match
        case (self: Generator.Finite[A], that: Generator.Finite[A2]) => self.concatFinite(that)
        case _                                                       => Generator.ConcatBounded(self, that)

    final def ++[A2 >: A](that: Generator.Bounded[A2]): Generator.Bounded[A2] = concatBounded(that)

    override def map[B](f: A => B): Generator.Bounded[B] = MapBounded(this, f)

  }
  object Bounded {
    def apply[A: Bounded as gen]: Bounded[A] = gen
  }

  final case class Finite[+A](values: Chunk[A]) extends Bounded[A] {

    override def streamExhaustive: UStream[A] = ZStream.fromChunk(values)

    def concatFinite[A2 >: A](that: Generator.Finite[A2]): Generator.Finite[A2] = Generator.Finite(this.values ++ that.values)

    def ++[A2 >: A](that: Generator.Finite[A2]): Generator.Finite[A2] = concatFinite(that)

    override def map[B](f: A => B): Generator.Finite[B] = Finite(values.map(f))

  }
  object Finite {
    def apply[A: Finite as gen]: Finite[A] = gen
  }

  final case class ConcatBounded[+A](_1: Generator.Bounded[A], _2: Generator.Bounded[A]) extends Bounded[A] {

    override def streamExhaustive: UStream[A] = _1.streamExhaustive ++ _2.streamExhaustive

  }

  final case class MapBounded[A, A2](a: Bounded[A], f: A => A2) extends Bounded[A2] {

    override def streamExhaustive: UStream[A2] = ???

  }

  /////// Unbounded ///////////////////////////////////////////////////////////////

  sealed trait Unbounded[+A] extends Generator[A] {

    override final def streamN(n: Int): UStream[A] = streamZioChunk { genN(n) }

  }

  final case class OfSeq[S[_], A](seqOps: SeqOps[S], g: Generator[A]) extends Unbounded[S[A]] {

    private given SeqOps[S] = seqOps

    override def streamExhaustiveOrSized: UStream[S[A]] =
      streamZioChunk { Sized.size.flatMap { s => g.genN(s * s).map(all => Chunk.from(all.grouped(s).map(_.into[S]))) } } ++ ZStream.succeed(Seq.empty[A].into[S])

    override def gen: UIO[S[A]] = genExhaustiveOrSized.flatMap { RandomGen.oneOf(_) }
    override def genN(n: Int): UIO[Chunk[S[A]]] = genExhaustiveOrSized.flatMap { RandomGen.oneOfN(_, n) }

  }

  final case class ConcatUnbounded[+A](_1: Generator[A], _2: Generator[A]) extends Unbounded[A] {

    override def gen: UIO[A] = genExhaustiveOrSized.flatMap { RandomGen.oneOf(_) }
    override def genN(n: Int): UIO[Chunk[A]] = genExhaustiveOrSized.flatMap { RandomGen.oneOfN(_, n) }

    override def streamExhaustiveOrSized: UStream[A] = _1.streamExhaustiveOrSized ++ _2.streamExhaustiveOrSized

  }

  final case class MapUnbounded[A, A2](a: Generator[A], f: A => A2) extends Unbounded[A2] {

    override def streamExhaustiveOrSized: UStream[A2] = a.streamExhaustiveOrSized.map(f)

    override def gen: UIO[A2] = a.gen.map(f)

    override def genN(n: Int): UIO[Chunk[A2]] = a.genN(n).map(_.map(f))

  }

  final case class FlatMapUnbounded[A, A2](a: Generator[A], f: A => Generator[A2]) extends Unbounded[A2] {

    override def streamExhaustiveOrSized: UStream[A2] = a.streamExhaustiveOrSized.flatMap(f(_).streamExhaustiveOrSized)

    override def gen: UIO[A2] = streamExhaustiveOrSized.runCollect.flatMap(RandomGen.oneOf(_))

    override def genN(n: Int): UIO[Chunk[A2]] = streamExhaustiveOrSized.runCollect.flatMap(RandomGen.oneOfN(_, n))

  }

  final case class Rand[+A](gen: UIO[A]) extends Unbounded[A] {

    override def genN(n: Int): UIO[Chunk[A]] = gen.replicateZIO(n).map(Chunk.from)

    override def streamExhaustiveOrSized: UStream[A] = ZStream.fromZIO(Sized.size).flatMap(streamN)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override protected def productDeriver[A](using Quotes, Type[Bounded], Type[A], ProductGeneric[A], Derivable[Bounded]): Derivable.ProductDeriver[Bounded, A] =
    Derivable.ProductDeriver.withDisjointInstances[Bounded, Generator, A] { instances =>
      new Derivable.ProductDeriver.Split[Bounded, A] {

        private def streamImpl: Expr[UStream[A]] =
          generic.instantiate.monad[ProjectStream[Any, Nothing]] { [a] => (_, _) ?=> (field: generic.Field[a]) =>
            '{ ${ field.getExpr(instances) }.streamExhaustiveOrSized }
          }

        override def deriveCaseClass(generic: ProductGeneric.CaseClassGeneric[A]): Expr[Bounded[A]] =
          '{
            new Bounded[A] {
              override def streamExhaustive: UStream[A] = $streamImpl
            }
          }

        override def deriveCaseObject(generic: ProductGeneric.CaseObjectGeneric[A]): Expr[Bounded[A]] =
          '{ const(${ generic.instantiate.instance }) }

      }
    }

  override protected def sumDeriver[A](using Quotes, Type[Bounded], Type[A], SumGeneric[A], Derivable[Bounded]): Derivable.SumDeriver[Bounded, A] =
    SumGeneric[A] match {
      case generic: SumGeneric.EnumGeneric[A] =>
        Derivable.SumDeriver.impl { '{ Finite(${ generic.mapChildren.mapToSeqExpr[Chunk, A] { [a <: A] => (_, _) ?=> (kase: generic.Case[a]) => kase.generic.instantiate.instance } }) } }
      case generic =>
        Derivable.SumDeriver.withInstances[Bounded, A] { instances =>
          Derivable.SumDeriver.impl {
            generic.mapChildren
              .mapExpr[Bounded[A]] { [a <: A] => (_, _) ?=> (kase: generic.Case[a]) => kase.getExpr(instances) }
              .to[Seq]
              .reduce { (a, b) => '{ $a ++ $b } }
          }
        }
    }

  override inline def derived[A]: Bounded[A] = ${ derivedImpl[A] }

}

object GeneratorLowPriority {

  trait LowPriority1 extends LowPriority2 {

    given boundedOption: [A: Generator.Bounded as g] => Generator.Bounded[Option[A]] =
      g.map(_.some) ++ Generator.finite(None)

    given boundedEither: [A: Generator.Bounded as ga, B: Generator.Bounded as gb] => Generator.Bounded[Either[A, B]] =
      ga.map(_.asLeft) ++ gb.map(_.asRight)

  }

  trait LowPriority2 {

    given option: [A: Generator as g] => Generator[Option[A]] =
      g.map(_.some) ++ Generator.finite(None)

    given either: [A: Generator as ga, B: Generator as gb] => Generator[Either[A, B]] =
      ga.map(_.asLeft) ++ gb.map(_.asRight)

  }

}
