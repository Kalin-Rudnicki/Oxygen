package oxygen.transform

import oxygen.core.typeclass.Pure
import oxygen.predef.core.*
import oxygen.transform.generic.*
import scala.quoted.*

trait Transform[From, To] {
  def transform(from: From): To
}
object Transform extends TransformLowPriority.LowPriority1 {

  final class TransformId[A] extends Transform[A, A] {
    override def transform(from: A): A = from
  }

  final case class TransformOption[A, B](transform: Transform[A, B]) extends Transform[Option[A], Option[B]] {

    override def transform(from: Option[A]): Option[B] =
      from.map(transform.transform)

  }

  final case class TransformSeq[SA[_]: SeqOps as sa, SB[_]: SeqOps as sb, A, B](transform: Transform[A, B]) extends Transform[SA[A], SB[B]] {

    override def transform(from: SA[A]): SB[B] =
      sb.fromIterableOnce(sa.newIterator(from).map(transform.transform))

  }

  final case class TransformPure[F[_]: Pure as pure, A, B](transform: Transform[A, B]) extends Transform[A, F[B]] {

    override def transform(from: A): F[B] =
      pure.pure(transform.transform(from))

  }

  given id: [A] => Transform[A, A] = new TransformId[A]

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  inline def derived[From, To]: Transform[From, To] = ${ TransformMacros.derivedImpl[From, To] }

}

object TransformLowPriority {

  trait LowPriority1 extends LowPriority2 {

    given option: [A, B] => (transform: Transform[A, B]) => Transform[Option[A], Option[B]] = Transform.TransformOption(transform)

    given seq: [SA[_]: SeqOps, SB[_]: SeqOps, A, B] => (transform: Transform[A, B]) => Transform[SA[A], SB[B]] = Transform.TransformSeq(transform)

  }

  trait LowPriority2 {

    given option: [F[_]: Pure, A, B] => (transform: Transform[A, B]) => Transform[A, F[B]] = Transform.TransformPure(transform)

  }

}
