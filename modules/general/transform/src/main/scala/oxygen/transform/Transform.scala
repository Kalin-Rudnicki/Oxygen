package oxygen.transform

import oxygen.core.typeclass.Pure
import oxygen.predef.core.*
import oxygen.transform.generic.*
import scala.quoted.*

trait Transform[From, To] extends TransformOrFail[From, To] {

  def transform(from: From): To

  override final def transformOrFail(from: From): Either[TransformError, To] = transform(from).asRight

  override final def map[To2](f: To => To2): Transform[From, To2] = this >> Transform.fromF(f)
  final def >>[To2](that: Transform[To, To2]): Transform[From, To2] = Transform.Then(this, that)

}
object Transform extends TransformLowPriority.LowPriority1 {

  def fromF[A, B](f: A => B): Transform[A, B] = Transform.FromFunction(f)

  final class TransformId[A] extends Transform[A, A] {
    override def transform(from: A): A = from
  }

  final case class FromFunction[A, B](f: A => B) extends Transform[A, B] {
    override def transform(from: A): B = f(from)
  }

  final case class TransformOption[A, B](t: Transform[A, B]) extends Transform[Option[A], Option[B]] {
    override def transform(from: Option[A]): Option[B] =
      from.map(t.transform)
  }

  final case class TransformSeq[SA[_]: SeqRead as sa, SB[_]: SeqWrite as sb, A, B](t: Transform[A, B]) extends Transform[SA[A], SB[B]] {
    override def transform(from: SA[A]): SB[B] =
      sb.fromIterableOnce(sa.newIterator(from).map(t.transform))
  }

  final case class TransformPure[F[_]: Pure as pure, A, B](t: Transform[A, B]) extends Transform[A, F[B]] {
    override def transform(from: A): F[B] =
      pure.pure(t.transform(from))
  }

  final case class Then[A, B, C](a: Transform[A, B], b: Transform[B, C]) extends Transform[A, C] {
    override def transform(from: A): C =
      b.transform(a.transform(from))
  }

  given id: [A] => Transform[A, A] = new TransformId[A]

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  inline def derived[From, To]: Transform[From, To] = ${ TransformMacros.deriveTransform[From, To] }

}

object TransformLowPriority {

  trait LowPriority1 extends LowPriority2 {

    given option: [A, B] => (transform: Transform[A, B]) => Transform[Option[A], Option[B]] = Transform.TransformOption(transform)

  }

  trait LowPriority2 extends LowPriority3 {

    given seq: [SA[_]: SeqRead, SB[_]: SeqWrite, A, B] => (transform: Transform[A, B]) => Transform[SA[A], SB[B]] = Transform.TransformSeq(transform)

  }

  trait LowPriority3 {

    given pure: [F[_]: Pure, A, B] => (transform: Transform[A, B]) => Transform[A, F[B]] = Transform.TransformPure(transform)

  }

}
