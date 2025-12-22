package oxygen.sql.schema

import oxygen.meta.k0.*
import oxygen.predef.core.*
import oxygen.sql.error.QueryError
import oxygen.sql.generic.typeclass.*
import scala.quoted.*

trait ResultDecoder[+A] {

  val size: Int
  def __decodeInternal(offset: Int, values: ArraySeq[Matchable]): Either[QueryError.UnableToDecodeRow, A]

  final def decode(values: ArraySeq[Any]): Either[QueryError.UnableToDecodeRow, A] =
    if values.length != size then
      this match {
        case ResultDecoder.WithColumns(_, columns) => QueryError.UnableToDecodeRow.InvalidRowSize(values, size, columns.some).asLeft
        case _                                     => QueryError.UnableToDecodeRow.InvalidRowSize(values, size, None).asLeft
      }
    else __decodeInternal(0, values.asInstanceOf[ArraySeq[Matchable]])

  final def map[B](ab: A => B): ResultDecoder[B] = this match
    case ResultDecoder.ColumnDecoder(column, decodeSingle) => ResultDecoder.ColumnDecoder(column, decodeSingle(_).map(ab))
    case ResultDecoder.SingleDecoder(decodeSingle)         => ResultDecoder.SingleDecoder(decodeSingle(_).map(ab))
    case ResultDecoder.MapDecoder(a, ab2)                  => ResultDecoder.MapDecoder(a, aValue => ab(ab2(aValue)))
    case ResultDecoder.MapOrFailDecoder(a, ab2)            => ResultDecoder.MapOrFailDecoder(a, aValue => ab2(aValue).map(ab))
    case _                                                 => ResultDecoder.MapDecoder(this, ab)

  final def mapOrFail[B](ab: A => Either[String, B]): ResultDecoder[B] = this match
    case ResultDecoder.ColumnDecoder(column, decodeSingle) => ResultDecoder.ColumnDecoder(column, decodeSingle(_).flatMap(ab))
    case ResultDecoder.SingleDecoder(decodeSingle)         => ResultDecoder.SingleDecoder(decodeSingle(_).flatMap(ab))
    case ResultDecoder.MapDecoder(a, ab2)                  => ResultDecoder.MapOrFailDecoder(a, aValue => ab(ab2(aValue)))
    case ResultDecoder.MapOrFailDecoder(a, ab2)            => ResultDecoder.MapOrFailDecoder(a, aValue => ab2(aValue).flatMap(ab))
    case _                                                 => ResultDecoder.MapOrFailDecoder(this, ab)

  final def optional: ResultDecoder[Option[A]] = ResultDecoder.OptionalDecoder(this)

}
object ResultDecoder extends Derivable[ResultDecoder] {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class SingleDecoder[A] private[ResultDecoder] (
      decodeSingle: Matchable => Either[String, A],
  ) extends ResultDecoder[A] {

    override val size: Int = 1

    override def __decodeInternal(offset: Int, values: ArraySeq[Matchable]): Either[QueryError.UnableToDecodeRow, A] = {
      val value: Matchable = values(offset)
      decodeSingle(value).leftMap(QueryError.UnableToDecodeRow.MapOrFail(values, offset, 1, value, _))
    }

  }
  object SingleDecoder {

    def simplePF[A](decode: PartialFunction[Matchable, A]): SingleDecoder[A] = {
      val lifted: Matchable => Option[A] = decode.lift
      SingleDecoder(lifted(_).toRight("Invalid type"))
    }

  }

  final case class ColumnDecoder[A] private[ResultDecoder] (
      column: Column,
      decodeSingle: Matchable => Either[String, A],
  ) extends ResultDecoder[A] {

    override val size: Int = 1

    override def __decodeInternal(offset: Int, values: ArraySeq[Matchable]): Either[QueryError.UnableToDecodeRow, A] =
      decodeSingle(values(offset)).leftMap(QueryError.UnableToDecodeRow.AtColumn(values, column, offset, _))

    private[sql] def withColumn(column: Column): ColumnDecoder[A] = copy(column = column)

  }
  object ColumnDecoder {

    private[sql] def makeSimplePF[A](column: Column, decodeSingle: PartialFunction[Matchable, A]): ColumnDecoder[A] = {
      val lifted: Matchable => Option[A] = decodeSingle.lift
      ColumnDecoder(column, i => lifted(i).toRight(s"Invalid type (${i.getClass.getName}), expected: ${column.columnType}"))
    }

  }

  final case class OptionalDecoder[A](inner: ResultDecoder[A]) extends ResultDecoder[Option[A]] {

    override val size: Int = inner.size

    override def __decodeInternal(offset: Int, values: ArraySeq[Matchable]): Either[QueryError.UnableToDecodeRow, Option[A]] =
      if isAllNull(offset, offset + size, values) then None.asRight
      else inner.__decodeInternal(offset, values).map(_.some)

  }

  final case class ArraySeqDecoder[A](inner: ResultDecoder[A]) extends ResultDecoder[ArraySeq[A]] {

    override val size: Int = inner.size

    override def __decodeInternal(offset: Int, values: ArraySeq[Matchable]): Either[QueryError.UnableToDecodeRow, ArraySeq[A]] = {
      val innerArray: Array[Any] = values(offset) match
        case arr: java.sql.Array => arr.getArray.asInstanceOf[Array[Any]]
        case arr: Array[Any]     => arr
        case res                 => throw new RuntimeException(s"not an sql arr: $res")

      val typedArray: ArraySeq[Matchable] = ArraySeq.unsafeWrapArray(innerArray).asInstanceOf[ArraySeq[Matchable]]

      ArraySeq.from(typedArray.indices).traverse(inner.__decodeInternal(_, typedArray))
    }

  }

  final case class WithColumns[A](inner: ResultDecoder[A], columns: Columns[A]) extends ResultDecoder[A] {
    override val size: Int = inner.size
    override def __decodeInternal(offset: Int, values: ArraySeq[Matchable]): Either[QueryError.UnableToDecodeRow, A] =
      inner.__decodeInternal(offset, values)
  }

  final case class MapDecoder[A, B] private[ResultDecoder] (a: ResultDecoder[A], ab: A => B) extends ResultDecoder[B] {
    override val size: Int = a.size
    override def __decodeInternal(offset: Int, values: ArraySeq[Matchable]): Either[QueryError.UnableToDecodeRow, B] =
      a.__decodeInternal(offset, values).map(ab)
  }

  final case class MapOrFailDecoder[A, B] private[ResultDecoder] (a: ResultDecoder[A], ab: A => Either[String, B]) extends ResultDecoder[B] {
    override val size: Int = a.size
    override def __decodeInternal(offset: Int, values: ArraySeq[Matchable]): Either[QueryError.UnableToDecodeRow, B] =
      a.__decodeInternal(offset, values).flatMap { aValue =>
        ab(aValue).leftMap { e => QueryError.UnableToDecodeRow.MapOrFail(values, offset, size, aValue, e) }
      }
  }

  case object Empty extends ResultDecoder[Unit] {
    override val size: Int = 0
    override def __decodeInternal(offset: Int, values: ArraySeq[Matchable]): Either[QueryError.UnableToDecodeRow, Unit] = ().asRight
  }

  trait CustomDecoder[A] extends ResultDecoder[A]

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given fromRowRepr: [A: RowRepr as repr] => ResultDecoder[A] = repr.decoder

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override protected def productDeriver[A](using Quotes, Type[ResultDecoder], Type[A], ProductGeneric[A], Derivable[ResultDecoder]): Derivable.ProductDeriver[ResultDecoder, A] =
    Derivable.ProductDeriver.withInstances { DeriveProductResultDecoder(_) }

  override protected def sumDeriver[A](using Quotes, Type[ResultDecoder], Type[A], SumGeneric[A], Derivable[ResultDecoder]): Derivable.SumDeriver[ResultDecoder, A] =
    Derivable.SumDeriver.notSupported

  override inline def derived[A]: ResultDecoder[A] = ${ derivedImpl[A] }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def isAllNull(fromIndexInclusive: Int, toIndexExclusive: Int, values: ArraySeq[Matchable]): Boolean = {
    var i: Int = fromIndexInclusive
    while i < toIndexExclusive do {
      if values(i) != null then return false
      i = i + 1
    }
    true
  }

}
