package oxygen.http.core

import oxygen.http.core.partial.PartialPathCodec
import oxygen.http.schema.partial.{PartiallyAppliedPathSchema, RequestSchemaAggregator}
import oxygen.meta.*
import oxygen.predef.core.*
import scala.annotation.tailrec
import scala.quoted.*
import scala.reflect.TypeTest

sealed trait RequestPathCodec[A] {

  lazy val partiallyAppliedPathSchemas: NonEmptyList[Growable[PartiallyAppliedPathSchema]]

  final lazy val schemaAggregator: RequestSchemaAggregator = RequestSchemaAggregator.paths(partiallyAppliedPathSchemas)

  def decode(paths: List[String]): Option[(A, List[String])]
  def encode(value: A): Growable[String]

  final def <||[B >: A](that: RequestPathCodec[B])(using ct: TypeTest[B, A]): RequestPathCodec[B] = RequestPathCodec.OrElse(this, that, ct)
  final def ||>[B <: A](that: RequestPathCodec[B])(using ct: TypeTest[A, B]): RequestPathCodec[A] = RequestPathCodec.OrElse(that, this, ct)

  final def /[B](that: RequestPathCodec[B])(using zip: Zip[A, B]): RequestPathCodec[zip.Out] =
    RequestPathCodec.AndThen(this, that, zip)

  final def transform[B](ab: A => B, ba: B => A): RequestPathCodec[B] = RequestPathCodec.Transform(this, ab, ba)
  final def transformDecode[B](ab: A => Option[B], ba: B => A): RequestPathCodec[B] = RequestPathCodec.TransformDecode(this, ab, ba)

  inline final def autoTransform[B]: RequestPathCodec[B] = {
    val (ab, ba) = K0.ProductGeneric.deriveTransform[A, B]
    transform(ab, ba)
  }

}
object RequestPathCodec {

  def const(path: String): RequestPathCodec[Unit] = ConstSingle(path)

  def applied[A: PartialPathCodec as c](name: String, doc: Option[String] = None): RequestPathCodec[A] = ApplyPartial(c, name, doc)
  def plain[A: PartialPathCodec.Plain as c](name: String, doc: Option[String] = None): RequestPathCodec[A] = ApplyPartial(c, name, doc)
  def json[A: PartialPathCodec.Json as c](name: String, doc: Option[String] = None): RequestPathCodec[A] = ApplyPartial(c, name, doc)

  given stringToPathCodec: Conversion[String, RequestPathCodec[Unit]] = const(_)

  final case class ConstSingle(path: String) extends RequestPathCodec[Unit] {

    private val encoded: Growable[String] = Growable.single(path)

    override lazy val partiallyAppliedPathSchemas: NonEmptyList[Growable[PartiallyAppliedPathSchema]] =
      NonEmptyList.one(Growable.single(PartiallyAppliedPathSchema.Const(path)))

    override def decode(paths: List[String]): Option[(Unit, List[String])] = paths match
      case `path` :: rest => ((), rest).some
      case _              => None

    override def encode(value: Unit): Growable[String] = encoded

  }

  final case class ConstMany(paths: NonEmptyList[String]) extends RequestPathCodec[Unit] {

    private val encoded: Growable[String] = Growable.many(paths)

    override lazy val partiallyAppliedPathSchemas: NonEmptyList[Growable[PartiallyAppliedPathSchema]] =
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

    override lazy val partiallyAppliedPathSchemas: NonEmptyList[Growable[PartiallyAppliedPathSchema]] =
      NonEmptyList.one(Growable.single(PartiallyAppliedPathSchema.Param(name, partial.partialPathSchema.tpe, partial.partialPathSchema.schema, doc)))

    override def decode(paths: List[String]): Option[(A, List[String])] = partial.decode(paths)
    override def encode(value: A): Growable[String] = partial.encode(value)

  }

  final case class AndThen[A, B, C](a: RequestPathCodec[A], b: RequestPathCodec[B], zip: Zip.Out[A, B, C]) extends RequestPathCodec[C] {

    override lazy val partiallyAppliedPathSchemas: NonEmptyList[Growable[PartiallyAppliedPathSchema]] =
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

    override lazy val partiallyAppliedPathSchemas: NonEmptyList[Growable[PartiallyAppliedPathSchema]] =
      a.partiallyAppliedPathSchemas ++ b.partiallyAppliedPathSchemas

    override def decode(paths: List[String]): Option[(B, List[String])] =
      a.decode(paths).orElse(b.decode(paths))

    override def encode(value: B): Growable[String] = value match
      case tt(aValue) => a.encode(aValue)
      case _          => b.encode(value)

  }

  final case class Transform[A, B](a: RequestPathCodec[A], ab: A => B, ba: B => A) extends RequestPathCodec[B] {

    override lazy val partiallyAppliedPathSchemas: NonEmptyList[Growable[PartiallyAppliedPathSchema]] =
      a.partiallyAppliedPathSchemas

    override def decode(paths: List[String]): Option[(B, List[String])] =
      a.decode(paths).map { case (aValue, rest) => (ab(aValue), rest) }

    override def encode(value: B): Growable[String] =
      a.encode(ba(value))

  }

  final case class TransformDecode[A, B](a: RequestPathCodec[A], ab: A => Option[B], ba: B => A) extends RequestPathCodec[B] {

    override lazy val partiallyAppliedPathSchemas: NonEmptyList[Growable[PartiallyAppliedPathSchema]] =
      a.partiallyAppliedPathSchemas

    override def decode(paths: List[String]): Option[(B, List[String])] =
      for {
        (aValue, rest) <- a.decode(paths)
        bValue <- ab(aValue)
      } yield (bValue, rest)

    override def encode(value: B): Growable[String] =
      a.encode(ba(value))

  }

  trait FromDecodeCases[A] extends RequestPathCodec[A] {

    val underlying: NonEmptyList[RequestPathCodec[? <: A]]

    override final lazy val partiallyAppliedPathSchemas: NonEmptyList[Growable[PartiallyAppliedPathSchema]] =
      underlying.flatMap(_.partiallyAppliedPathSchemas)

    @tailrec
    private def decodeRec(paths: List[String], codecs: List[RequestPathCodec[? <: A]]): Option[(A, List[String])] =
      codecs match {
        case codec :: rest =>
          codec.decode(paths) match {
            case some @ Some(_) => some
            case None           => decodeRec(paths, rest)
          }
        case Nil =>
          None
      }

    override final def decode(paths: List[String]): Option[(A, List[String])] =
      decodeRec(paths, underlying.toList)

  }
  object FromDecodeCases {

    private def derivedImpl[A: Type](using Quotes): Expr[FromDecodeCases[A]] = {
      val gen: K0.SumGeneric[A] = K0.SumGeneric.of[A]
      gen.cacheVals.summonTypeClasses[RequestPathCodec]().defineAndUse { instances =>
        def schemas: List[Expr[RequestPathCodec[? <: A]]] =
          gen.mapChildren.mapExpr[RequestPathCodec[? <: A]] { [b <: A] => (_, _) ?=> (kase: gen.Case[b]) => kase.getExpr(instances) }.to[List]

        def encodeImpl(value: Expr[A]): Expr[Growable[String]] =
          gen.matcher.instance(value) { [b <: A] => (_, _) ?=> (kase: gen.Case[b]) =>
            kase.caseExtractor.withRHS { value => '{ ${ kase.getExpr(instances) }.encode($value) } }
          }

        '{
          new FromDecodeCases[A] {

            override val underlying: NonEmptyList[RequestPathCodec[? <: A]] =
              NonEmptyList.unsafeFromList { ${ schemas.seqToExpr } }

            override def encode(value: A): Growable[String] = ${ encodeImpl('value) }

          }
        }
      }
    }

    // currently requires that you have derived an implicit instance for each case already
    inline def derived[A]: FromDecodeCases[A] = ${ derivedImpl[A] }

  }

}
