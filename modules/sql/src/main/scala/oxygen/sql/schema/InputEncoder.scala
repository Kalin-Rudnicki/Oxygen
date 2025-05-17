package oxygen.sql.schema

import oxygen.predef.core.*
import oxygen.predef.meta.*
import oxygen.sql.generic.*
import oxygen.sql.query.InputWriter

trait InputEncoder[-A] {

  val size: Int

  def unsafeEncode(writer: InputWriter, value: A): Unit

  final def contramap[B](ba: B => A): InputEncoder[B] = this match
    case InputEncoder.SingleEncoder(encodeSingle)         => InputEncoder.SingleEncoder(bValue => encodeSingle(ba(bValue)))
    case InputEncoder.ColumnEncoder(column, encodeSingle) => InputEncoder.ColumnEncoder(column, bValue => encodeSingle(ba(bValue)))
    case InputEncoder.ContramapEncoder(a, ba2)            => InputEncoder.ContramapEncoder(a, bValue => ba2(ba(bValue)))
    case _                                                => InputEncoder.ContramapEncoder(this, ba)

  final def ~[A2 <: A](that: InputEncoder[A2]): InputEncoder[A2] =
    InputEncoder.Zip(this, that)

}
object InputEncoder extends K0.Derivable.WithInstances[InputEncoder] {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class SingleEncoder[A] private[InputEncoder] (
      encodeSingle: A => Any,
  ) extends InputEncoder[A] {
    override val size: Int = 1
    override def unsafeEncode(writer: InputWriter, value: A): Unit = writer.unsafeWrite(encodeSingle(value))
  }
  object SingleEncoder {

    def id[A]: SingleEncoder[A] = SingleEncoder[A](identity[Any])

    def enc[A](encodeSingle: A => Any): SingleEncoder[A] = SingleEncoder[A](encodeSingle)

  }

  final case class ColumnEncoder[A] private[InputEncoder] (
      column: Column,
      encodeSingle: A => Any,
  ) extends InputEncoder[A] {
    override val size: Int = 1
    override def unsafeEncode(writer: InputWriter, value: A): Unit = writer.unsafeWrite(encodeSingle(value))
    private[sql] def withColumn(column: Column): ColumnEncoder[A] = copy(column = column)
  }
  object ColumnEncoder {

    private[sql] def make[A](column: Column, encodeSingle: A => Any): ColumnEncoder[A] =
      ColumnEncoder(column, encodeSingle)

  }

  final case class OptionalEncoder[A](inner: InputEncoder[A]) extends InputEncoder[Option[A]] {

    override val size: Int = inner.size

    private val isSingle: Boolean = size == 1

    override def unsafeEncode(writer: InputWriter, value: Option[A]): Unit = value match
      case Some(value)      => inner.unsafeEncode(writer, value)
      case None if isSingle => writer.unsafeWrite(null)
      case None             => writer.writeNulls(size)

  }

  final case class ContramapEncoder[A, B] private[InputEncoder] (a: InputEncoder[A], ba: B => A) extends InputEncoder[B] {
    override val size: Int = a.size
    override def unsafeEncode(writer: InputWriter, value: B): Unit = a.unsafeEncode(writer, ba(value))
  }

  final case class ConcatAll[A](encoders: Contiguous[InputEncoder[A]]) extends InputEncoder[A] {
    override val size: Int = encoders.map(_.size).sum
    override def unsafeEncode(writer: InputWriter, value: A): Unit = encoders.foreach { _.unsafeEncode(writer, value) }
  }

  final case class Zip[A](a: InputEncoder[A], b: InputEncoder[A]) extends InputEncoder[A] {

    override val size: Int = a.size + b.size

    override def unsafeEncode(writer: InputWriter, value: A): Unit = {
      a.unsafeEncode(writer, value)
      b.unsafeEncode(writer, value)
    }

  }

  case object Empty extends InputEncoder[Any] {
    override val size: Int = 0
    override def unsafeEncode(writer: InputWriter, value: Any): Unit = ()
  }

  trait CustomEncoder[A] extends InputEncoder[A]

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given fromRowRepr: [A: RowRepr as repr] => InputEncoder[A] = repr.encoder

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override protected def internalDeriveProductI[Q <: Quotes, A](k0: K0[Q])(
      g: k0.ProductGeneric[A],
      i: k0.ValExpressions[InputEncoder],
  )(using quotes: Q, aTpe: Type[A], tTpe: Type[InputEncoder]): Expr[InputEncoder[A]] =
    DeriveProductInputEncoder[Q, A](k0)(g, i).makeInputEncoder

  override protected def internalDeriveSumI[Q <: Quotes, A](k0: K0[Q])(
      g: k0.SumGeneric[A],
      i: k0.ValExpressions[InputEncoder],
  )(using quotes: Q, aTpe: Type[A], tTpe: Type[InputEncoder]): Expr[InputEncoder[A]] =
    k0.meta.report.errorAndAbort("Not supported: InputEncoder.derive for sum type")

  inline def derived[A]: InputEncoder[A] = ${ derivedImpl[A] }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

}
