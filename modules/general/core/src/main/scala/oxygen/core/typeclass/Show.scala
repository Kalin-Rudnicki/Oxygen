package oxygen.core.typeclass

import oxygen.predef.core.*
import scala.collection.mutable

trait Show[A] {

  def show(value: A): String

  def writeTo(builder: mutable.StringBuilder, value: A): Unit = builder.append(show(value))

}
object Show extends ShowLowPriority.LowPriority1 {

  inline def apply[A: Show as ev]: Show[A] = ev

  trait PreferBuffer[A] extends Show[A] {

    override def show(value: A): String = {
      val builder = mutable.StringBuilder()
      writeTo(builder, value)
      builder.result()
    }

    override def writeTo(builder: mutable.StringBuilder, value: A): Unit

  }

  def usingToString[A]: Show[A] = _.toString

  given string: Show[String] = _.unesc
  given boolean: Show[Boolean] = usingToString
  given byte: Show[Byte] = usingToString
  given short: Show[Short] = usingToString
  given int: Show[Int] = usingToString
  given long: Show[Long] = usingToString
  given bigInt: Show[BigInt] = usingToString
  given float: Show[Float] = usingToString
  given double: Show[Double] = usingToString
  given bigDecimal: Show[BigDecimal] = usingToString

  def showOption[A: Show as showA](
      somePrefix: String,
      someSuffix: String,
      none: String,
  ): Show[Option[A]] =
    new PreferBuffer[Option[A]] {
      override def writeTo(builder: mutable.StringBuilder, value: Option[A]): Unit =
        value match {
          case Some(value) =>
            builder.append(somePrefix)
            showA.writeTo(builder, value)
            builder.append(someSuffix)
          case None =>
            builder.append(none)
        }
    }

  given option: [A: Show as showA] => Show[Option[A]] =
    showOption("", "", "<none>")

  def showSeq[S[_]: SeqRead as seqOps, A: Show as showA](
      prefix: String,
      join: String,
      suffix: String,
  ): Show[S[A]] =
    new PreferBuffer[S[A]] {
      override def writeTo(builder: mutable.StringBuilder, value: S[A]): Unit = {
        val iter = seqOps.newIterator(value)
        builder.append(prefix)
        iter.nextOption() match {
          case Some(value) =>
            showA.writeTo(builder, value)
          case None =>
        }
        while (iter.hasNext) {
          builder.append(join)
          showA.writeTo(builder, iter.next())
        }
        builder.append(suffix)
      }
    }

}

object ShowLowPriority {

  trait LowPriority1 {

    given seq: [S[_]: SeqRead as seqOps, A: Show as showA] => Show[S[A]] =
      Show.showSeq[S, A]("[", ", ", "]")

  }

}
