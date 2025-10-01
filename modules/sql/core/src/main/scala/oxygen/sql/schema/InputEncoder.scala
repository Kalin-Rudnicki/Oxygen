package oxygen.sql.schema

import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.sql.generic.typeclass.*
import oxygen.sql.query.InputWriter
import scala.quoted.*
import zio.*

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

  def toArrayElem(value: A): AnyRef = value.asInstanceOf[AnyRef]

  final def optional: InputEncoder[Option[A]] = InputEncoder.OptionalEncoder(this)

}
object InputEncoder extends K0.Derivable[InputEncoder] {

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

  final case class ArraySeqEncoder[A](inner: InputEncoder[A], colType: Column.Type) extends InputEncoder[ArraySeq[A]] {

    override val size: Int = inner.size

    override def unsafeEncode(writer: InputWriter, value: ArraySeq[A]): Unit = {
      writer.unsafeWriteArray(colType, value.map(inner.toArrayElem).toArray)
    }

    override def toArrayElem(value: ArraySeq[A]): AnyRef =
      value.map(inner.toArrayElem).toArray

  }

  final case class ContramapEncoder[A, B] private[InputEncoder] (a: InputEncoder[A], ba: B => A) extends InputEncoder[B] {
    override val size: Int = a.size
    override def unsafeEncode(writer: InputWriter, value: B): Unit = a.unsafeEncode(writer, ba(value))
    override def toArrayElem(value: B): AnyRef = a.toArrayElem(ba(value))
  }

  final case class ConcatAll[A](encoders: ArraySeq[InputEncoder[A]]) extends InputEncoder[A] {
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

  final case class Const[A](inner: InputEncoder[A], const: A) extends InputEncoder[Any] {
    override val size: Int = inner.size
    override def unsafeEncode(writer: InputWriter, value: Any): Unit = inner.unsafeEncode(writer, const)
  }

  case object Empty extends InputEncoder[Any] {
    override val size: Int = 0
    override def unsafeEncode(writer: InputWriter, value: Any): Unit = ()
  }

  val isNullEncoder: InputEncoder[Option[Any]] =
    RowRepr.boolean.encoder.contramap[Option[Any]] { _.isEmpty }

  trait CustomEncoder[A] extends InputEncoder[A]

  final case class BatchChunkEncoder[A](inner: InputEncoder[A], expSize: Int) extends InputEncoder[Chunk[A]] {

    override val size: Int = inner.size * expSize

    override def unsafeEncode(writer: InputWriter, value: Chunk[A]): Unit = {
      if (value.length != expSize)
        throw new RuntimeException(s"BatchChunkEncoder(_, $expSize) received chunk input of size = ${value.length}")

      value.foreach(inner.unsafeEncode(writer, _))
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given fromRowRepr: [A: RowRepr as repr] => InputEncoder[A] = repr.encoder

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override protected def productDeriver[A](using Quotes, Type[InputEncoder], Type[A], K0.ProductGeneric[A], K0.Derivable[InputEncoder]): K0.Derivable.ProductDeriver[InputEncoder, A] =
    K0.Derivable.ProductDeriver.withInstances { DeriveProductInputEncoder(_) }

  override protected def sumDeriver[A](using Quotes, Type[InputEncoder], Type[A], K0.SumGeneric[A], K0.Derivable[InputEncoder]): K0.Derivable.SumDeriver[InputEncoder, A] =
    K0.Derivable.SumDeriver.notSupported

  override inline def derived[A]: InputEncoder[A] = ${ derivedImpl[A] }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

}
