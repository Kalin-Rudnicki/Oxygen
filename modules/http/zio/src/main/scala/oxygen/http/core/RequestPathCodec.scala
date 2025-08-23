package oxygen.http.core

import oxygen.http.core.partial.PartialPathCodec
import oxygen.http.schema.partial.{PartiallyAppliedPathSchema, RequestSchemaAggregator}
import oxygen.predef.core.*
import scala.annotation.tailrec
import scala.reflect.TypeTest

sealed trait RequestPathCodec[A] {

  val partiallyAppliedPathSchemas: NonEmptyList[Growable[PartiallyAppliedPathSchema]]

  final lazy val schemaAggregator: RequestSchemaAggregator = RequestSchemaAggregator.paths(partiallyAppliedPathSchemas)

  def decode(paths: List[String]): Option[(A, List[String])]
  def encode(value: A): Growable[String]

  final def <||[B >: A](that: RequestPathCodec[B])(using ct: TypeTest[B, A]): RequestPathCodec[B] = RequestPathCodec.OrElse(this, that, ct)
  final def ||>[B <: A](that: RequestPathCodec[B])(using ct: TypeTest[A, B]): RequestPathCodec[A] = RequestPathCodec.OrElse(that, this, ct)

  final def transform[B](ab: A => B, ba: B => A): RequestPathCodec[B] = RequestPathCodec.Transform(this, ab, ba)
  final def transformDecode[B](ab: A => Option[B], ba: B => A): RequestPathCodec[B] = RequestPathCodec.TransformDecode(this, ab, ba)

}
object RequestPathCodec {

  final case class ConstSingle(path: String) extends RequestPathCodec[Unit] {

    private val encoded: Growable[String] = Growable.single(path)

    override val partiallyAppliedPathSchemas: NonEmptyList[Growable[PartiallyAppliedPathSchema]] =
      NonEmptyList.one(Growable.single(PartiallyAppliedPathSchema.Const(path)))

    override def decode(paths: List[String]): Option[(Unit, List[String])] = paths match
      case `path` :: rest => ((), rest).some
      case _              => None

    override def encode(value: Unit): Growable[String] = encoded

  }

  final case class ConstMany(paths: NonEmptyList[String]) extends RequestPathCodec[Unit] {

    private val encoded: Growable[String] = Growable.many(paths)

    override val partiallyAppliedPathSchemas: NonEmptyList[Growable[PartiallyAppliedPathSchema]] =
      NonEmptyList.one(Growable.many(paths).map(PartiallyAppliedPathSchema.Const(_)))

    @tailrec
    private def loop(
        actualPaths: List[String],
        expPaths: List[String],
    ): Option[(Unit, List[String])] =
      expPaths match {
        case expHead :: expTail =>
          actualPaths match {
            case `expHead` :: actualTail => loop(actualTail, expTail)
            case _                       => None
          }
        case Nil =>
          ((), actualPaths).some
      }

    override def decode(paths: List[String]): Option[(Unit, List[String])] = loop(paths, this.paths.toList)
    override def encode(value: Unit): Growable[String] = encoded

  }

  final case class ApplyPartial[A](partial: PartialPathCodec[A], name: String, doc: Option[String]) extends RequestPathCodec[A] {

    override val partiallyAppliedPathSchemas: NonEmptyList[Growable[PartiallyAppliedPathSchema]] =
      NonEmptyList.one(Growable.single(PartiallyAppliedPathSchema.Param(name, partial.partialPathSchema.tpe, partial.partialPathSchema.schema, doc)))

    override def decode(paths: List[String]): Option[(A, List[String])] = partial.decode(paths)
    override def encode(value: A): Growable[String] = partial.encode(value)

  }

  final case class AndThen[A, B, C](a: RequestPathCodec[A], b: RequestPathCodec[B], zip: Zip.Out[A, B, C]) extends RequestPathCodec[C] {

    override val partiallyAppliedPathSchemas: NonEmptyList[Growable[PartiallyAppliedPathSchema]] =
      (a.partiallyAppliedPathSchemas, b.partiallyAppliedPathSchemas) match {
        case (NonEmptyList(aPaths, Nil), NonEmptyList(bPaths, Nil)) => NonEmptyList.one(aPaths ++ bPaths)
        case _                                                      =>
          for {
            aPaths <- a.partiallyAppliedPathSchemas
            bPaths <- b.partiallyAppliedPathSchemas
          } yield aPaths ++ bPaths
      }

    override def decode(paths: List[String]): Option[(C, List[String])] =
      for {
        (aValue, aRest) <- a.decode(paths)
        (bValue, bRest) <- b.decode(aRest)
      } yield (zip.zip(aValue, bValue), bRest)

    override def encode(value: C): Growable[String] = {
      val (aValue, bValue) = zip.unzip(value)
      a.encode(aValue) ++ b.encode(bValue)
    }

  }

  final case class OrElse[A, B >: A](a: RequestPathCodec[A], b: RequestPathCodec[B], tt: TypeTest[B, A]) extends RequestPathCodec[B] {

    override val partiallyAppliedPathSchemas: NonEmptyList[Growable[PartiallyAppliedPathSchema]] =
      a.partiallyAppliedPathSchemas ++ b.partiallyAppliedPathSchemas

    override def decode(paths: List[String]): Option[(B, List[String])] =
      a.decode(paths).orElse(b.decode(paths))

    override def encode(value: B): Growable[String] = value match
      case tt(aValue) => a.encode(aValue)
      case _          => b.encode(value)

  }

  final case class Transform[A, B](a: RequestPathCodec[A], ab: A => B, ba: B => A) extends RequestPathCodec[B] {

    override val partiallyAppliedPathSchemas: NonEmptyList[Growable[PartiallyAppliedPathSchema]] =
      a.partiallyAppliedPathSchemas

    override def decode(paths: List[String]): Option[(B, List[String])] =
      a.decode(paths).map { case (aValue, rest) => (ab(aValue), rest) }

    override def encode(value: B): Growable[String] =
      a.encode(ba(value))

  }

  final case class TransformDecode[A, B](a: RequestPathCodec[A], ab: A => Option[B], ba: B => A) extends RequestPathCodec[B] {

    override val partiallyAppliedPathSchemas: NonEmptyList[Growable[PartiallyAppliedPathSchema]] =
      a.partiallyAppliedPathSchemas

    override def decode(paths: List[String]): Option[(B, List[String])] =
      for {
        (aValue, rest) <- a.decode(paths)
        bValue <- ab(aValue)
      } yield (bValue, rest)

    override def encode(value: B): Growable[String] =
      a.encode(ba(value))

  }

}
