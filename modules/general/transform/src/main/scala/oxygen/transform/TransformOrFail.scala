package oxygen.transform

import oxygen.core.typeclass.Pure
import oxygen.predef.core.*
import oxygen.transform.generic.*
import scala.collection.mutable

trait TransformOrFail[From, To] {

  def transformOrFail(from: From): Either[TransformError, To]
  final def atField(fieldName: String): TransformOrFail[From, To] = TransformOrFail.AtField(fieldName, this)
  final def atSubType(typeName: String): TransformOrFail[From, To] = TransformOrFail.AtSubType(typeName, this)

  final def >>[To2](that: TransformOrFail[To, To2]): TransformOrFail[From, To2] = TransformOrFail.Then(this, that)

  def map[To2](f: To => To2): TransformOrFail[From, To2] = this >> Transform.fromF(f)
  final def mapOrFail[To2](f: To => Either[String, To2]): TransformOrFail[From, To2] = this >> TransformOrFail.fromEitherF(f)

}
object TransformOrFail extends TransformOrFailLowPriority.LowPriority1 {

  def fromEitherF[A, B](f: A => Either[String, B]): TransformOrFail[A, B] = TransformOrFail.FromEitherFunction(f)

  final case class TransformInfallible[From, To](t: Transform[From, To]) extends TransformOrFail[From, To] {
    override def transformOrFail(from: From): Either[TransformError, To] = t.transform(from).asRight
  }

  final case class TransformOption[A, B](t: TransformOrFail[A, B]) extends TransformOrFail[Option[A], Option[B]] {
    override def transformOrFail(from: Option[A]): Either[TransformError, Option[B]] =
      from.traverse(t.transformOrFail)
  }

  final case class TransformRequireOption[A, B](t: TransformOrFail[A, B]) extends TransformOrFail[Option[A], B] {
    override def transformOrFail(from: Option[A]): Either[TransformError, B] = from match
      case Some(from) => t.transformOrFail(from)
      case None       => TransformError(Nil, TransformError.Cause.MissingRequired).asLeft
  }

  final case class TransformSeq[SA[_]: SeqRead as sa, SB[_]: SeqWrite as sb, A, B](t: TransformOrFail[A, B]) extends TransformOrFail[SA[A], SB[B]] {

    override def transformOrFail(from: SA[A]): Either[TransformError, SB[B]] = {
      val iter: Iterator[A] = sa.newIterator(from)
      val builder: mutable.Builder[B, SB[B]] = sb.newBuilder[B]
      sa.knownSize(from) match
        case -1   => ()
        case size => builder.sizeHint(size)

      var idx: Int = 0

      while iter.hasNext do {
        val fromElem: A = iter.next()
        t.transformOrFail(fromElem) match {
          case Right(toElem) => builder.addOne(toElem)
          case Left(error)   => return error.atIndex(idx).asLeft
        }
        idx += 1
      }

      builder.result().asRight
    }

  }

  final case class TransformPure[F[_]: Pure as pure, A, B](t: TransformOrFail[A, B]) extends TransformOrFail[A, F[B]] {
    override def transformOrFail(from: A): Either[TransformError, F[B]] =
      t.transformOrFail(from).map(pure.pure)
  }

  final case class DecodeString[A](dec: StringDecoder[A]) extends TransformOrFail[String, A] {
    override def transformOrFail(from: String): Either[TransformError, A] =
      dec.decodeDetailed(from).leftMap { error => TransformError(Nil, TransformError.Cause.DecodingFailure(error)) }
  }

  final case class FromEitherFunction[From, To](f: From => Either[String, To]) extends TransformOrFail[From, To] {
    override def transformOrFail(from: From): Either[TransformError, To] =
      f(from).leftMap { error => TransformError(Nil, TransformError.Cause.DecodingFailure(error)) }
  }

  final case class Then[A, B, C](a: TransformOrFail[A, B], b: TransformOrFail[B, C]) extends TransformOrFail[A, C] {
    override def transformOrFail(from: A): Either[TransformError, C] = a.transformOrFail(from).flatMap(b.transformOrFail)
  }

  final case class AtField[From, To](fieldName: String, t: TransformOrFail[From, To]) extends TransformOrFail[From, To] {
    override def transformOrFail(from: From): Either[TransformError, To] = t.transformOrFail(from).leftMap(_.atField(fieldName))
  }

  final case class AtSubType[From, To](typeName: String, t: TransformOrFail[From, To]) extends TransformOrFail[From, To] {
    override def transformOrFail(from: From): Either[TransformError, To] = t.transformOrFail(from).leftMap(_.atSubType(typeName))
  }

  given fromInfallible: [From, To] => (t: Transform[From, To]) => TransformOrFail[From, To] = t

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  inline def derived[From, To]: TransformOrFail[From, To] = ${ TransformMacros.deriveTransformOrFail[From, To] }

}

object TransformOrFailLowPriority {

  trait LowPriority1 extends LowPriority2 {

    given option: [A, B] => (transform: TransformOrFail[A, B]) => TransformOrFail[Option[A], Option[B]] = TransformOrFail.TransformOption(transform)

  }

  trait LowPriority2 extends LowPriority3 {

    given requireOption: [A, B] => (transform: TransformOrFail[A, B]) => TransformOrFail[Option[A], B] = TransformOrFail.TransformRequireOption(transform)

    given seq: [SA[_]: SeqRead, SB[_]: SeqWrite, A, B] => (transform: TransformOrFail[A, B]) => TransformOrFail[SA[A], SB[B]] = TransformOrFail.TransformSeq(transform)

  }

  trait LowPriority3 extends LowPriority4 {

    given decodeString: [A: StringDecoder as dec] => TransformOrFail[String, A] = TransformOrFail.DecodeString(dec)

  }

  trait LowPriority4 {

    given pure: [F[_]: Pure, A, B] => (transform: TransformOrFail[A, B]) => TransformOrFail[A, F[B]] = TransformOrFail.TransformPure(transform)

  }

}
