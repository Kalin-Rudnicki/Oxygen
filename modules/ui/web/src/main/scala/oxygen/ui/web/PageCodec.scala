package oxygen.ui.web

import oxygen.http.core.RequestDecodingFailure
import oxygen.http.core.partial.{PartialParamCodec, PartialPathCodec}
import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.schema.*
import scala.annotation.tailrec
import scala.quoted.*
import zio.Chunk
import zio.http.{Path, QueryParams}

trait PageCodec[A] {

  def decodeInternal(paths: List[String], queryParams: QueryParams): PageCodec.ParseResult[(A, List[String])]

  def encodeInternal(value: A): (Growable[String], Growable[(String, Chunk[String])])

  final def decode(url: PageURL): PageCodec.ParseResult[A] =
    decodeInternal(url.path.segments.toList, url.queryParams).flatMap {
      case (value, Nil) => PageCodec.ParseResult.Success(value)
      case _            => PageCodec.ParseResult.InvalidPath
    }

  final def encode(value: A): PageURL = {
    val (path, queryParams) = encodeInternal(value)
    PageURL(path.toArraySeq.foldLeft(Path.empty)(_ / _), QueryParams(queryParams.to[Seq]*))
  }

  final def /[B](that: PageCodec[B])(using zip: Zip[A, B]): PageCodec[zip.Out] = PageCodec.Zipped(this, that, zip)

  final def transform[B](ab: A => B, ba: B => A): PageCodec[B] = PageCodec.Transform(this, ab, ba)

  inline final def autoTransform[B]: PageCodec[B] = {
    val (ab, ba) = K0.ProductGeneric.deriveTransform[A, B]
    transform(ab, ba)
  }

}
object PageCodec {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  val empty: PageCodec[Unit] = Empty

  extension [A](self: => PageCodec[A])
    def suspended: PageCodec[A] =
      suspend(self)

  def suspend[A](codec: => PageCodec[A]): PageCodec[A] =
    PageCodec.Suspend { Lazy { codec } }

  object path {

    def const(p: String): PageCodec[Unit] = PageCodec.ConstPath(p)
    def const(ps: List[String]): PageCodec[Unit] = PageCodec.ConstPaths(ps)
    def apply[A: PartialPathCodec as c]: PageCodec[A] = PageCodec.FromPathCodec(c)
    def plain[A: PlainTextSchema]: PageCodec[A] = PageCodec.FromPathCodec(PartialPathCodec.Plain.singleEncoded[A])
    def json[A: JsonSchema]: PageCodec[A] = PageCodec.FromPathCodec(PartialPathCodec.Json.singleEncoded[A])

    object rest {
      def string: PageCodec[List[String]] = PageCodec.FromPathCodec(PartialPathCodec.Plain.restString)
      def plain[A: PlainTextSchema]: PageCodec[List[A]] = PageCodec.FromPathCodec(PartialPathCodec.Plain.restEncoded[A])
      def json[A: JsonSchema]: PageCodec[List[A]] = PageCodec.FromPathCodec(PartialPathCodec.Json.restEncoded[A])
    }

    object nonEmptyRest {
      def plain[A: PlainTextSchema]: PageCodec[NonEmptyList[A]] = PageCodec.FromPathCodec(PartialPathCodec.Json.nonEmptyRestEncoded[A])
      def json[A: JsonSchema]: PageCodec[NonEmptyList[A]] = PageCodec.FromPathCodec(PartialPathCodec.Json.nonEmptyRestEncoded[A])
    }

  }

  object query {

    object plain {
      def required[A: PlainTextSchema](key: String): PageCodec[A] = PageCodec.FromParamCodec(key, PartialParamCodec.Plain.required[A])
      def optional[A: PlainTextSchema](key: String): PageCodec[Option[A]] = PageCodec.FromParamCodec(key, PartialParamCodec.Plain.optional[A])
      def manyRequired[A: PlainTextSchema](key: String): PageCodec[List[A]] = PageCodec.FromParamCodec(key, PartialParamCodec.Plain.many[List, A])
      def manyOptional[A: PlainTextSchema](key: String): PageCodec[NonEmptyList[A]] = PageCodec.FromParamCodec(key, PartialParamCodec.Plain.manyNonEmpty[A])
    }

    object json {
      def required[A: JsonSchema](key: String): PageCodec[A] = PageCodec.FromParamCodec(key, PartialParamCodec.Json.required[A])
      def optional[A: JsonSchema](key: String): PageCodec[Option[A]] = PageCodec.FromParamCodec(key, PartialParamCodec.Json.optional[A])
      def manyRequired[A: JsonSchema](key: String): PageCodec[List[A]] = PageCodec.FromParamCodec(key, PartialParamCodec.Json.many[List, A])
      def manyOptional[A: JsonSchema](key: String): PageCodec[NonEmptyList[A]] = PageCodec.FromParamCodec(key, PartialParamCodec.Json.manyNonEmpty[A])
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Impls
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  case object Empty extends PageCodec[Unit] {
    override def decodeInternal(paths: List[String], queryParams: QueryParams): ParseResult[(Unit, List[String])] = ParseResult.Success(((), paths))
    override def encodeInternal(value: Unit): (Growable[String], Growable[(String, Chunk[String])]) = (Growable.empty, Growable.empty)
  }

  final case class ConstPath(p: String) extends PageCodec[Unit] {

    override def decodeInternal(paths: List[String], queryParams: QueryParams): ParseResult[(Unit, List[String])] =
      paths match
        case `p` :: tail => ParseResult.Success(((), tail))
        case _           => ParseResult.InvalidPath

    override def encodeInternal(value: Unit): (Growable[String], Growable[(String, Chunk[String])]) =
      (Growable.single(p), Growable.empty)

  }

  final case class ConstPaths(p: List[String]) extends PageCodec[Unit] {

    @tailrec
    private def loop(
        exp: List[String],
        act: List[String],
    ): ParseResult[(Unit, List[String])] =
      exp match {
        case expHead :: expTail =>
          act match {
            case `expHead` :: actTail => loop(expTail, actTail)
            case _                    => ParseResult.InvalidPath
          }
        case Nil =>
          ParseResult.Success(((), act))
      }

    override def decodeInternal(paths: List[String], queryParams: QueryParams): ParseResult[(Unit, List[String])] =
      loop(p, paths)

    override def encodeInternal(value: Unit): (Growable[String], Growable[(String, Chunk[String])]) =
      (Growable.many(p), Growable.empty)

  }

  final case class FromPathCodec[A](c: PartialPathCodec[A]) extends PageCodec[A] {

    override def decodeInternal(paths: List[String], queryParams: QueryParams): ParseResult[(A, List[String])] =
      ParseResult.fromOption(c.decode(paths))

    override def encodeInternal(value: A): (Growable[String], Growable[(String, Chunk[String])]) =
      (c.encode(value), Growable.empty)

  }

  final case class FromParamCodec[A](key: String, c: PartialParamCodec[A]) extends PageCodec[A] {

    private val sources: List[RequestDecodingFailure.Source] = RequestDecodingFailure.Source.QueryParam(key) :: Nil

    override def decodeInternal(paths: List[String], queryParams: QueryParams): ParseResult[(A, List[String])] =
      ParseResult.fromEither(c.decode(queryParams.getAll(key).toList).bimap(RequestDecodingFailure(sources, _), (_, paths)))

    override def encodeInternal(value: A): (Growable[String], Growable[(String, Chunk[String])]) = {
      val tmp = c.encode(value)
      if tmp.nonEmpty then (Growable.empty, Growable.single((key, Chunk.from(tmp))))
      else (Growable.empty, Growable.empty)
    }

  }

  final case class Zipped[A, B, C](a: PageCodec[A], b: PageCodec[B], zip: Zip.Out[A, B, C]) extends PageCodec[C] {

    override def decodeInternal(paths: List[String], queryParams: QueryParams): ParseResult[(C, List[String])] =
      for
        (aValue, aRest) <- a.decodeInternal(paths, queryParams)
        (bValue, bRest) <- b.decodeInternal(aRest, queryParams)
      yield (zip.zip(aValue, bValue), bRest)

    override def encodeInternal(value: C): (Growable[String], Growable[(String, Chunk[String])]) = {
      val (aValue, bValue) = zip.unzip(value)
      val (aPath, aQuery) = a.encodeInternal(aValue)
      val (bPath, bQuery) = b.encodeInternal(bValue)
      (aPath ++ bPath, aQuery ++ bQuery)
    }

  }

  final case class Suspend[A](inner: Lazy[PageCodec[A]]) extends PageCodec[A] {

    override def decodeInternal(paths: List[String], queryParams: QueryParams): ParseResult[(A, List[String])] =
      inner.value.decodeInternal(paths, queryParams)

    override def encodeInternal(value: A): (Growable[String], Growable[(String, Chunk[String])]) =
      inner.value.encodeInternal(value)

  }

  trait FromDecodeCases[A] extends PageCodec[A] {

    val underlying: NonEmptyList[PageCodec[? <: A]]

    @tailrec
    private def decodeRec(paths: List[String], queryParams: QueryParams, queue: List[PageCodec[? <: A]]): ParseResult[(A, List[String])] =
      queue match {
        case codec :: rest =>
          codec.decodeInternal(paths, queryParams) match {
            case success @ ParseResult.Success(_) => success
            case ParseResult.InvalidPath          => decodeRec(paths, queryParams, rest)
            case error @ ParseResult.Error(_)     => error
          }
        case Nil =>
          ParseResult.InvalidPath
      }

    override final def decodeInternal(paths: List[String], queryParams: QueryParams): ParseResult[(A, List[String])] =
      decodeRec(paths, queryParams, underlying.toList)

  }
  object FromDecodeCases {

    private def derivedImpl[A: Type](using Quotes): Expr[FromDecodeCases[A]] = {
      val gen: K0.SumGeneric[A] = K0.SumGeneric.of[A]
      gen.cacheVals.summonTypeClasses[PageCodec]().defineAndUse { instances =>
        def schemas: List[Expr[PageCodec[? <: A]]] =
          gen.mapChildren.mapExpr[PageCodec[? <: A]] { [b <: A] => (_, _) ?=> (kase: gen.Case[b]) => kase.getExpr(instances) }.to[List]

        def encodeImpl(value: Expr[A]): Expr[(Growable[String], Growable[(String, Chunk[String])])] =
          gen.matcher.instance(value) { [b <: A] => (_, _) ?=> (kase: gen.Case[b]) =>
            kase.caseExtractor.withRHS { value => '{ ${ kase.getExpr(instances) }.encodeInternal($value) } }
          }

        '{
          new FromDecodeCases[A] {

            override val underlying: NonEmptyList[PageCodec[? <: A]] =
              NonEmptyList.unsafeFromList { ${ schemas.seqToExpr } }

            override def encodeInternal(value: A): (Growable[String], Growable[(String, Chunk[String])]) = ${ encodeImpl('value) }

          }
        }
      }
    }

    // currently requires that you have derived an implicit instance for each case already
    inline def derived[A]: FromDecodeCases[A] = ${ derivedImpl[A] }

  }

  final case class Transform[A, B](a: PageCodec[A], ab: A => B, ba: B => A) extends PageCodec[B] {

    override def decodeInternal(paths: List[String], queryParams: QueryParams): ParseResult[(B, List[String])] =
      a.decodeInternal(paths, queryParams).map { case (aValue, rest) => (ab(aValue), rest) }

    override def encodeInternal(value: B): (Growable[String], Growable[(String, Chunk[String])]) =
      a.encodeInternal(ba(value))

  }

  // TODO (KR) : transform option/either

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ParseResult
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait ParseResult[+A] {

    final def map[B](f: A => B): ParseResult[B] = this match
      case ParseResult.Success(value) => ParseResult.Success(f(value))
      case ns: ParseResult.NonSuccess => ns

    final def flatMap[B](f: A => ParseResult[B]): ParseResult[B] = this match
      case ParseResult.Success(value) => f(value)
      case ns: ParseResult.NonSuccess => ns

  }
  object ParseResult {

    final case class Success[+A](value: A) extends ParseResult[A]
    sealed trait NonSuccess extends ParseResult[Nothing]
    final case class Error(error: RequestDecodingFailure) extends NonSuccess
    case object InvalidPath extends NonSuccess

    def fromOption[A](value: Option[A]): ParseResult[A] = value match
      case Some(value) => Success(value)
      case None        => InvalidPath

    def fromEither[A](value: Either[RequestDecodingFailure, A]): ParseResult[A] = value match
      case Right(value) => Success(value)
      case Left(value)  => Error(value)

  }

}

given stringToParamCodec: Conversion[String, PageCodec[Unit]] = PageCodec.ConstPath(_)
