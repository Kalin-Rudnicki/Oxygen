package oxygen.http.core

import oxygen.http.client.SendRequest
import oxygen.http.core.partial.*
import oxygen.http.model.internal.*
import oxygen.http.schema.*
import oxygen.http.schema.partial.*
import oxygen.meta.*
import oxygen.meta.k0.*
import oxygen.predef.core.*
import oxygen.schema.*
import scala.annotation.tailrec
import scala.quoted.*
import zio.*
import zio.http.*

sealed trait RequestCodec[A] {

  lazy val sources: List[RequestDecodingFailure.Source]
  lazy val schemaAggregator: RequestSchemaAggregator

  protected final def makeDecodingFailure(cause: DecodingFailureCause): RequestDecodingFailure = RequestDecodingFailure(sources, cause)
  protected final def makeDecodingFailureInputs(error: String, inputs: List[String]): RequestDecodingFailure =
    RequestDecodingFailure(sources, DecodingFailureCause.DecodeError(error, DecodingFailureCause.DecodeInput.InputValues(inputs)))
  protected final def makeDecodingFailureBody(error: String, body: Option[String]): RequestDecodingFailure = body match
    case Some(body) => RequestDecodingFailure(sources, DecodingFailureCause.DecodeError(error, DecodingFailureCause.DecodeInput.Body(body)))
    case None       => RequestDecodingFailure(sources, DecodingFailureCause.DecodeError(error, DecodingFailureCause.DecodeInput.BodyNoValue))
  protected final def makeDecodingFailurePreviousValue(error: String, previous: Any): RequestDecodingFailure =
    RequestDecodingFailure(sources, DecodingFailureCause.DecodeError(error, DecodingFailureCause.DecodeInput.PreviousValue(previous.toString)))

  def matchesPathPatternInternal(path: List[String]): Option[List[String]]
  final def matchesPathPattern(path: List[String]): Boolean = matchesPathPatternInternal(path) match
    case Some(Nil) => true
    case _         => false

  def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[A]

  final def decode(request: ReceivedRequest): FinalRequestParseResult[A] =
    decodeInternal(request.fullPath, request).evalFinalResult

  def encodeInternal(value: A, acc: RequestBuilder): RequestBuilder

  final def encode(value: A): SendRequest = {
    val out = encodeInternal(value, RequestBuilder.empty)
    SendRequest(
      method = out.method,
      path = out.paths.toArraySeq.foldLeft(Path.empty) { _ / _ },
      queryParams = QueryParams(out.queryParams.to[Seq]*),
      headers = Headers(out.headers.toArraySeq),
      body = out.body,
    )
  }

  final def applied(value: A): RequestCodec[Unit] = RequestCodec.internal.Applied(this, value)

}
object RequestCodec {

  inline def apply[A](using ev: RequestCodec[A]): RequestCodec[A] = ev

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def anyMethod(name: String, doc: Option[String] = None): RequestCodec.NonPathLike[Method] = RequestCodec.internal.AnyMethod(name, doc)
  def anyMethod: RequestCodec.NonPathLike[Method] = RequestCodec.anyMethod("method")
  def constMethod(method: Method): RequestCodec.NonPathLike[Unit] = RequestCodec.internal.ConstMethod(method)

  object path {

    def empty: RequestCodec.PathLike[Unit] = RequestCodec.internal.EmptyPath
    def const(p: String): RequestCodec.PathLike[Unit] = RequestCodec.internal.ConstSinglePath(p)
    def const(p0: String, p1: String, pN: String*): RequestCodec.PathLike[Unit] = RequestCodec.path.const(p0 :: p1 :: pN.toList)
    def const(p: NonEmptyList[String]): RequestCodec.PathLike[Unit] = RequestCodec.path.const(p.toList)
    def const(p: List[String]): RequestCodec.PathLike[Unit] = p match
      case p :: Nil => RequestCodec.internal.ConstSinglePath(p)
      case p0 :: pN => RequestCodec.internal.ConstManyPath(NonEmptyList(p0, pN))
      case Nil      => RequestCodec.internal.EmptyPath

    def plain[A: PartialPathCodec.Plain as c](name: String, doc: Option[String] = None): RequestCodec.PathLike[A] = RequestCodec.internal.AppliedPartialPath(c, name, doc)
    def json[A: PartialPathCodec.Json as c](name: String, doc: Option[String] = None): RequestCodec.PathLike[A] = RequestCodec.internal.AppliedPartialPath(c, name, doc)

    def rest(name: String, doc: Option[String] = None): RequestCodec.PathLike[List[String]] = RequestCodec.path.fromPartial(PartialPathCodec.Plain.RestString, name, doc)
    def restNonEmpty(name: String, doc: Option[String] = None): RequestCodec.PathLike[NonEmptyList[String]] = RequestCodec.path.fromPartial(PartialPathCodec.Plain.NonEmptyRestString, name, doc)

    def fromPartial[A](codec: PartialPathCodec[A], name: String, doc: Option[String] = None): RequestCodec.PathLike[A] =
      RequestCodec.internal.AppliedPartialPath(codec, name, doc)

    private def deriveSumImpl[A: Type](using Quotes): Expr[RequestCodec.internal.PathOr[A]] = {
      val gen: SumGeneric[A] = SumGeneric.of[A]
      gen.cacheVals.summonTypeClasses[RequestCodec.PathLike]().defineAndUse { instances =>
        def schemas: List[Expr[RequestCodec.PathLike[? <: A]]] =
          gen.mapChildren.mapExpr[RequestCodec.PathLike[? <: A]] { [b <: A] => (_, _) ?=> (kase: gen.Case[b]) => kase.getExpr(instances) }.to[List]

        def encodeImpl(value: Expr[A], accExpr: Expr[RequestBuilder]): Expr[RequestBuilder] =
          gen.matcher.instance(value) { [b <: A] => (_, _) ?=> (kase: gen.Case[b]) =>
            kase.caseExtractor.withRHS { value => '{ ${ kase.getExpr(instances) }.encodeInternal($value, $accExpr) } }
          }

        '{
          new RequestCodec.internal.PathOr[A] {
            override val underlying: NonEmptyList[RequestCodec.PathLike[? <: A]] = NonEmptyList.unsafeFromList { ${ schemas.seqToExpr } }
            override def encodeInternal(value: A, acc: RequestBuilder): RequestBuilder = ${ encodeImpl('value, 'acc) }
          }
        }
      }
    }

    inline def deriveSum[A]: RequestCodec.PathLike[A] = ${ deriveSumImpl[A] }

    def customSum[A](case0: RequestCodec.PathLike[? <: A], caseN: RequestCodec.PathLike[? <: A]*)(encodeCase: A => RequestCodec.PathLike[? <: A]): RequestCodec.PathLike[A] =
      new RequestCodec.internal.PathOr[A] {
        override val underlying: NonEmptyList[PathLike[? <: A]] = NonEmptyList(case0, caseN.toList)
        override def encodeInternal(value: A, acc: RequestBuilder): RequestBuilder = {
          val unsafeCase: RequestCodec.PathLike[A] = encodeCase(value).asInstanceOf[RequestCodec.PathLike[A]]
          unsafeCase.encodeInternal(value, acc)
        }
      }

  }

  object nonPath {

    private def deriveSumImpl[A: Type](using Quotes): Expr[RequestCodec.internal.NonPathOr[A]] = {
      val gen: SumGeneric[A] = SumGeneric.of[A]
      gen.cacheVals.summonTypeClasses[RequestCodec.NonPathLike]().defineAndUse { instances =>
        def schemas: List[Expr[RequestCodec.NonPathLike[? <: A]]] =
          gen.mapChildren.mapExpr[RequestCodec.NonPathLike[? <: A]] { [b <: A] => (_, _) ?=> (kase: gen.Case[b]) => kase.getExpr(instances) }.to[List]

        def encodeImpl(value: Expr[A], accExpr: Expr[RequestBuilder]): Expr[RequestBuilder] =
          gen.matcher.instance(value) { [b <: A] => (_, _) ?=> (kase: gen.Case[b]) =>
            kase.caseExtractor.withRHS { value => '{ ${ kase.getExpr(instances) }.encodeInternal($value, $accExpr) } }
          }

        '{
          new RequestCodec.internal.NonPathOr[A] {
            override val underlying: NonEmptyList[RequestCodec.NonPathLike[? <: A]] = NonEmptyList.unsafeFromList { ${ schemas.seqToExpr } }
            override def encodeInternal(value: A, acc: RequestBuilder): RequestBuilder = ${ encodeImpl('value, 'acc) }
          }
        }
      }
    }

    inline def deriveSum[A]: RequestCodec.NonPathLike[A] = ${ deriveSumImpl[A] }

    def customSum[A](case0: RequestCodec.NonPathLike[? <: A], caseN: RequestCodec.NonPathLike[? <: A]*)(encodeCase: A => RequestCodec.NonPathLike[? <: A]): RequestCodec.NonPathLike[A] =
      new RequestCodec.internal.NonPathOr[A] {
        override val underlying: NonEmptyList[NonPathLike[? <: A]] = NonEmptyList(case0, caseN.toList)
        override def encodeInternal(value: A, acc: RequestBuilder): RequestBuilder = {
          val unsafeCase: RequestCodec.NonPathLike[A] = encodeCase(value).asInstanceOf[RequestCodec.NonPathLike[A]]
          unsafeCase.encodeInternal(value, acc)
        }
      }

  }

  object query {

    def raw(name: String, doc: Option[String] = None): RequestCodec.NonPathLike[QueryParams] = RequestCodec.internal.RawQueryParams(name, doc)
    def raw: RequestCodec.NonPathLike[QueryParams] = RequestCodec.query.raw("queryParams")

    def fromPartial[A](codec: PartialParamCodec[A], name: String, doc: Option[String] = None): RequestCodec.NonPathLike[A] =
      RequestCodec.internal.AppliedPartialQueryParam(codec, name, doc)

    object plain {

      def required[A: PlainTextSchema](name: String, doc: Option[String] = None): RequestCodec.NonPathLike[A] =
        RequestCodec.internal.AppliedPartialQueryParam(PartialParamCodec.Plain.required[A], name, doc)

      def optional[A: PlainTextSchema](name: String, doc: Option[String] = None): RequestCodec.NonPathLike[Option[A]] =
        RequestCodec.internal.AppliedPartialQueryParam(PartialParamCodec.Plain.optional[A], name, doc)

      def many[S[_]: SeqOps, A: PlainTextSchema](name: String, doc: Option[String] = None): RequestCodec.NonPathLike[S[A]] =
        RequestCodec.internal.AppliedPartialQueryParam(PartialParamCodec.Plain.many[S, A], name, doc)

      def manyNonEmpty[A: PlainTextSchema](name: String, doc: Option[String] = None): RequestCodec.NonPathLike[NonEmptyList[A]] =
        RequestCodec.internal.AppliedPartialQueryParam(PartialParamCodec.Plain.manyNonEmpty[A], name, doc)

    }

    object json {

      def required[A: JsonSchema](name: String, doc: Option[String] = None): RequestCodec.NonPathLike[A] =
        RequestCodec.internal.AppliedPartialQueryParam(PartialParamCodec.Json.required[A], name, doc)

      def optional[A: JsonSchema](name: String, doc: Option[String] = None): RequestCodec.NonPathLike[Option[A]] =
        RequestCodec.internal.AppliedPartialQueryParam(PartialParamCodec.Json.optional[A], name, doc)

      def many[S[_]: SeqOps, A: JsonSchema](name: String, doc: Option[String] = None): RequestCodec.NonPathLike[S[A]] =
        RequestCodec.internal.AppliedPartialQueryParam(PartialParamCodec.Json.many[S, A], name, doc)

      def manyNonEmpty[A: JsonSchema](name: String, doc: Option[String] = None): RequestCodec.NonPathLike[NonEmptyList[A]] =
        RequestCodec.internal.AppliedPartialQueryParam(PartialParamCodec.Json.manyNonEmpty[A], name, doc)

    }

  }

  object header {

    /**
      * Returns raw [[Headers]], but filters out standard headers like content length.
      * The filtered headers should then be fine to include verbatim in another request.
      */
    def rawFiltered(name: String, doc: Option[String] = None): RequestCodec.NonPathLike[Headers] =
      rawUnfiltered(name, doc).transform(ZioHttpCompat.filterStandardHeaders, ZioHttpCompat.filterStandardHeaders)
    def rawFiltered: RequestCodec.NonPathLike[Headers] = RequestCodec.header.rawFiltered("headers")

    /**
      * Returns raw [[Headers]], without filtering out standard headers like content length.
      * Useful if you want the raw headers verbatim, but without filtering, these headers are not safe to pass into another request.
      */
    def rawUnfiltered(name: String, doc: Option[String] = None): RequestCodec.NonPathLike[Headers] = RequestCodec.internal.RawHeaders(name, doc)
    def rawUnfiltered: RequestCodec.NonPathLike[Headers] = RequestCodec.header.rawUnfiltered("headers")

    def raw(name: String, doc: Option[String] = None): RequestCodec.NonPathLike[Headers] = RequestCodec.header.rawFiltered(name, doc)
    def raw: RequestCodec.NonPathLike[Headers] = RequestCodec.header.raw("headers")

    def fromPartial[A](codec: PartialParamCodec[A], name: String, doc: Option[String] = None): RequestCodec.NonPathLike[A] =
      RequestCodec.internal.AppliedPartialHeader(codec, name, doc)

    object plain {

      def required[A: PlainTextSchema](name: String, doc: Option[String] = None): RequestCodec.NonPathLike[A] =
        RequestCodec.internal.AppliedPartialHeader(PartialParamCodec.Plain.required[A], name, doc)

      def optional[A: PlainTextSchema](name: String, doc: Option[String] = None): RequestCodec.NonPathLike[Option[A]] =
        RequestCodec.internal.AppliedPartialHeader(PartialParamCodec.Plain.optional[A], name, doc)

      def many[S[_]: SeqOps, A: PlainTextSchema](name: String, doc: Option[String] = None): RequestCodec.NonPathLike[S[A]] =
        RequestCodec.internal.AppliedPartialHeader(PartialParamCodec.Plain.many[S, A], name, doc)

      def manyNonEmpty[A: PlainTextSchema](name: String, doc: Option[String] = None): RequestCodec.NonPathLike[NonEmptyList[A]] =
        RequestCodec.internal.AppliedPartialHeader(PartialParamCodec.Plain.manyNonEmpty[A], name, doc)

    }

    object json {

      def required[A: JsonSchema](name: String, doc: Option[String] = None): RequestCodec.NonPathLike[A] =
        RequestCodec.internal.AppliedPartialHeader(PartialParamCodec.Json.required[A], name, doc)

      def optional[A: JsonSchema](name: String, doc: Option[String] = None): RequestCodec.NonPathLike[Option[A]] =
        RequestCodec.internal.AppliedPartialHeader(PartialParamCodec.Json.optional[A], name, doc)

      def many[S[_]: SeqOps, A: JsonSchema](name: String, doc: Option[String] = None): RequestCodec.NonPathLike[S[A]] =
        RequestCodec.internal.AppliedPartialHeader(PartialParamCodec.Json.many[S, A], name, doc)

      def manyNonEmpty[A: JsonSchema](name: String, doc: Option[String] = None): RequestCodec.NonPathLike[NonEmptyList[A]] =
        RequestCodec.internal.AppliedPartialHeader(PartialParamCodec.Json.manyNonEmpty[A], name, doc)

    }

  }

  object body {

    def raw(name: String, doc: Option[String] = None): RequestCodec.NonPathLike[ReadOnlyCachedHttpBody] = RequestCodec.internal.RawBody(name, doc)
    def raw: RequestCodec.NonPathLike[ReadOnlyCachedHttpBody] = RequestCodec.body.raw("body")

    def fromPartial[A](codec: PartialBodyCodec[A], name: String, doc: Option[String] = None): RequestCodec.NonPathLike[A] =
      RequestCodec.internal.AppliedPartialBody(codec, name, doc)

    def plain[A: PlainTextSchema as s](name: String, doc: Option[String] = None): RequestCodec.NonPathLike[A] = RequestCodec.internal.AppliedPartialBody(PartialBodyCodec.fromPlain[A], name, doc)
    def plain[A: PlainTextSchema]: RequestCodec.NonPathLike[A] = RequestCodec.body.plain[A]("body", None)

    def json[A: JsonSchema as s](name: String, doc: Option[String] = None): RequestCodec.NonPathLike[A] = RequestCodec.internal.AppliedPartialBody(PartialBodyCodec.fromJson[A], name, doc)
    def json[A: JsonSchema]: RequestCodec.NonPathLike[A] = RequestCodec.body.json[A]("body", None)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Roots
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait PathLike[A] extends RequestCodec[A] {

    final def transform[B](ab: A => B, ba: B => A): RequestCodec.PathLike[B] = RequestCodec.internal.TransformPathLike(this, ab, ba)
    final def transformOpt[B](ab: A => Option[B], ba: B => A): RequestCodec.PathLike[B] = RequestCodec.internal.TransformOrFailPathLike(this, ab(_).asRight, ba)
    final def transformOrFail[B](ab: A => Either[String, B], ba: B => A): RequestCodec.PathLike[B] = RequestCodec.internal.TransformOrFailPathLike(this, ab(_).map(_.some), ba)
    final def transformOrFailOpt[B](ab: A => Either[String, Option[B]], ba: B => A): RequestCodec.PathLike[B] = RequestCodec.internal.TransformOrFailPathLike(this, ab, ba)

    inline final def autoTransform[B]: RequestCodec.PathLike[B] = {
      val (ab, ba) = ProductGeneric.deriveTransform[A, B]
      transform(ab, ba)
    }

    final def /[B](that: PathLike[B])(using zip: Zip[A, B]): PathLike[zip.Out] =
      RequestCodec.internal.Then_Path_Path(this, that, zip)

    final def ++[B](that: NonPathLike[B])(using zip: Zip[A, B]): PathLike[zip.Out] =
      RequestCodec.internal.Then_Path_NonPath(this, that, zip)

  }
  object PathLike {

    // TODO (KR) : givens

  }

  sealed trait NonPathLike[A] extends RequestCodec[A] {

    final def transform[B](ab: A => B, ba: B => A): RequestCodec.NonPathLike[B] = RequestCodec.internal.TransformNonPathLike(this, ab, ba)
    final def transformOrFail[B](ab: A => Either[String, B], ba: B => A): RequestCodec.NonPathLike[B] = RequestCodec.internal.TransformOrFailNonPathLike(this, ab, ba)

    override final def matchesPathPatternInternal(path: List[String]): Option[List[String]] = path.some

    inline final def autoTransform[B]: RequestCodec.NonPathLike[B] = {
      val (ab, ba) = ProductGeneric.deriveTransform[A, B]
      transform(ab, ba)
    }

    final def /[B](that: PathLike[B])(using zip: Zip[A, B]): PathLike[zip.Out] =
      RequestCodec.internal.Then_NonPath_Path(this, that, zip)

    final def ++[B](that: NonPathLike[B])(using zip: Zip[A, B]): NonPathLike[zip.Out] =
      RequestCodec.internal.And_NonPath(this, that, zip)

  }
  object NonPathLike {

    // TODO (KR) : givens

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // TODO (KR) : limit access?
  trait Custom[A] extends RequestCodec[A]

  final case class CustomWithConstMethod[A](
      method: Method,
      underlying: RequestCodec[A],
  ) extends RequestCodec[A] {

    override lazy val sources: List[RequestDecodingFailure.Source] = underlying.sources
    override lazy val schemaAggregator: RequestSchemaAggregator = RequestSchemaAggregator.method(method) >>> underlying.schemaAggregator

    override def matchesPathPatternInternal(path: List[String]): Option[List[String]] = underlying.matchesPathPatternInternal(path)

    override def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[A] =
      if request.method == method then underlying.decodeInternal(remainingPaths, request)
      else IntermediateRequestParseResult.NotFound

    override def encodeInternal(value: A, acc: RequestBuilder): RequestBuilder =
      underlying.encodeInternal(value, acc.copy(method = method))

  }

  private[http] object internal {

    final case class Applied[A](underlying: RequestCodec[A], value: A) extends RequestCodec.Custom[Unit] {
      override lazy val sources: List[RequestDecodingFailure.Source] = underlying.sources
      override lazy val schemaAggregator: RequestSchemaAggregator = underlying.schemaAggregator
      override def matchesPathPatternInternal(path: List[String]): Option[List[String]] = underlying.matchesPathPatternInternal(path)
      override def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[Unit] = // TODO (KR) : better way to do this?
        IntermediateRequestParseResult.Success((), remainingPaths) // TODO (KR) : note, this wont work with paths
      override def encodeInternal(value: Unit, acc: RequestBuilder): RequestBuilder = underlying.encodeInternal(this.value, acc)
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Shared
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    sealed trait ThenLike[A, B, C] { self: RequestCodec[C] =>

      val a: RequestCodec[A]
      val b: RequestCodec[B]
      val zip: Zip.Out[A, B, C]

      override final lazy val sources: List[RequestDecodingFailure.Source] = a.sources ++ b.sources
      override final lazy val schemaAggregator: RequestSchemaAggregator = a.schemaAggregator >>> b.schemaAggregator

      override final def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[C] =
        for {
          (aValue, pathsA) <- a.decodeInternal(remainingPaths, request)
          bValue <- b.decodeInternal(pathsA, request)
        } yield zip.zip(aValue, bValue)

      override final def encodeInternal(value: C, acc: RequestBuilder): RequestBuilder = {
        val (aValue, bValue) = zip.unzip(value)
        b.encodeInternal(bValue, a.encodeInternal(aValue, acc))
      }

    }

    sealed trait OrLike[A] { self: RequestCodec[A] =>

      val underlying: NonEmptyList[RequestCodec[? <: A]]

      override final lazy val sources: List[RequestDecodingFailure.Source] = underlying.toList.flatMap(_.sources)
      override final lazy val schemaAggregator: RequestSchemaAggregator = underlying.map(_.schemaAggregator).reduceLeft(_ || _)

      override final def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[A] =
        underlying.tail.foldLeft[IntermediateRequestParseResult[A]](underlying.head.decodeInternal(remainingPaths, request)) { (acc, pl) =>
          acc || pl.decodeInternal(remainingPaths, request)
        }

    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      PathLike
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    final case class Then_Path_NonPath[A, B, C](a: PathLike[A], b: NonPathLike[B], zip: Zip.Out[A, B, C]) extends PathLike[C], ThenLike[A, B, C] {
      override def matchesPathPatternInternal(path: List[String]): Option[List[String]] = a.matchesPathPatternInternal(path).flatMap(b.matchesPathPatternInternal)
    }

    final case class Then_NonPath_Path[A, B, C](a: NonPathLike[A], b: PathLike[B], zip: Zip.Out[A, B, C]) extends PathLike[C], ThenLike[A, B, C] {
      override def matchesPathPatternInternal(path: List[String]): Option[List[String]] = a.matchesPathPatternInternal(path).flatMap(b.matchesPathPatternInternal)
    }

    final case class Then_Path_Path[A, B, C](a: PathLike[A], b: PathLike[B], zip: Zip.Out[A, B, C]) extends PathLike[C], ThenLike[A, B, C] {
      override def matchesPathPatternInternal(path: List[String]): Option[List[String]] = a.matchesPathPatternInternal(path).flatMap(b.matchesPathPatternInternal)
    }

    trait PathOr[A] extends PathLike[A], OrLike[A] {

      override val underlying: NonEmptyList[RequestCodec.PathLike[? <: A]]

      @tailrec
      private def canDecodePathInternalLoop(
          path: List[String],
          queue: List[RequestCodec.PathLike[?]],
      ): Option[List[String]] =
        queue match {
          case head :: tail =>
            head.matchesPathPatternInternal(path) match {
              case some @ Some(_) => some
              case None           => canDecodePathInternalLoop(path, tail)
            }
          case Nil =>
            None
        }

      override final def matchesPathPatternInternal(path: List[String]): Option[List[String]] =
        canDecodePathInternalLoop(path, underlying.toList)

    }

    final case class TransformPathLike[A, B](a: PathLike[A], ab: A => B, ba: B => A) extends PathLike[B] {

      override lazy val sources: List[RequestDecodingFailure.Source] = a.sources
      override lazy val schemaAggregator: RequestSchemaAggregator = a.schemaAggregator

      override def matchesPathPatternInternal(path: List[String]): Option[List[String]] = a.matchesPathPatternInternal(path)

      override def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[B] =
        a.decodeInternal(remainingPaths, request).map(ab)

      override def encodeInternal(value: B, acc: RequestBuilder): RequestBuilder =
        a.encodeInternal(ba(value), acc)

    }

    final case class TransformOrFailPathLike[A, B](a: PathLike[A], ab: A => Either[String, Option[B]], ba: B => A) extends PathLike[B] {

      override lazy val sources: List[RequestDecodingFailure.Source] = a.sources
      override lazy val schemaAggregator: RequestSchemaAggregator = a.schemaAggregator

      override def matchesPathPatternInternal(path: List[String]): Option[List[String]] = a.matchesPathPatternInternal(path)

      override def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[B] =
        a.decodeInternal(remainingPaths, request).flatMap { (value, rest) =>
          ab(value) match {
            case Right(Some(value)) => IntermediateRequestParseResult.Success(value, rest)
            case Right(None)        => IntermediateRequestParseResult.NotFound
            case Left(error)        => IntermediateRequestParseResult.Error(makeDecodingFailurePreviousValue(error, value))
          }
        }

      override def encodeInternal(value: B, acc: RequestBuilder): RequestBuilder =
        a.encodeInternal(ba(value), acc)

    }

    case object EmptyPath extends PathLike[Unit] {

      override lazy val sources: List[RequestDecodingFailure.Source] = Nil
      override lazy val schemaAggregator: RequestSchemaAggregator = RequestSchemaAggregator.empty

      override def matchesPathPatternInternal(path: List[String]): Option[List[String]] = path.some

      override def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[Unit] =
        IntermediateRequestParseResult.Success((), remainingPaths)

      override def encodeInternal(value: Unit, acc: RequestBuilder): RequestBuilder =
        acc

    }

    final case class ConstSinglePath(const: String) extends PathLike[Unit] {

      override lazy val sources: List[RequestDecodingFailure.Source] = Nil
      override lazy val schemaAggregator: RequestSchemaAggregator =
        RequestSchemaAggregator.paths(NonEmptyList.one(Growable.single(PartiallyAppliedPathSchema.Const(const))))

      override def matchesPathPatternInternal(path: List[String]): Option[List[String]] = path match
        case `const` :: tail => tail.some
        case _               => None

      override def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[Unit] =
        remainingPaths match
          case `const` :: rest => IntermediateRequestParseResult.Success((), rest)
          case _               => IntermediateRequestParseResult.NotFound

      override def encodeInternal(value: Unit, acc: RequestBuilder): RequestBuilder =
        acc.copy(paths = acc.paths :+ const)

    }

    final case class ConstManyPath(const: NonEmptyList[String]) extends PathLike[Unit] {

      override lazy val sources: List[RequestDecodingFailure.Source] = Nil
      override lazy val schemaAggregator: RequestSchemaAggregator =
        RequestSchemaAggregator.paths(NonEmptyList.one(Growable.many(const).map(PartiallyAppliedPathSchema.Const(_))))

      @tailrec
      private def decodeLoop(remainingPaths: List[String], exp: List[String]): IntermediateRequestParseResult[Unit] =
        exp match {
          case expHead :: expTail =>
            remainingPaths match {
              case `expHead` :: rest => decodeLoop(rest, expTail)
              case _                 => IntermediateRequestParseResult.NotFound
            }
          case Nil =>
            IntermediateRequestParseResult.Success((), remainingPaths)
        }

      @tailrec
      private def matchLoop(remainingPaths: List[String], exp: List[String]): Option[List[String]] =
        exp match {
          case expHead :: expTail =>
            remainingPaths match {
              case `expHead` :: rest => matchLoop(rest, expTail)
              case _                 => None
            }
          case Nil =>
            remainingPaths.some
        }

      override def matchesPathPatternInternal(path: List[String]): Option[List[String]] =
        matchLoop(path, const.toList)

      override def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[Unit] =
        decodeLoop(remainingPaths, const.toList)

      override def encodeInternal(value: Unit, acc: RequestBuilder): RequestBuilder =
        acc.copy(paths = acc.paths :++ Growable.many(const))

    }

    final case class AppliedPartialPath[A](part: PartialPathCodec[A], name: String, doc: Option[String]) extends PathLike[A] {

      override lazy val sources: List[RequestDecodingFailure.Source] = RequestDecodingFailure.Source.Path(name) :: Nil
      override lazy val schemaAggregator: RequestSchemaAggregator =
        RequestSchemaAggregator.simplePath(PartiallyAppliedPathSchema.Param(name, part.partialPathSchema.tpe, part.partialPathSchema.schema, doc))

      override def matchesPathPatternInternal(path: List[String]): Option[List[String]] =
        part.decode(path).map(_._2)

      override def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[A] =
        part.decode(remainingPaths) match
          case Some((value, rest)) => IntermediateRequestParseResult.Success(value, rest)
          case None                => IntermediateRequestParseResult.NotFound

      override def encodeInternal(value: A, acc: RequestBuilder): RequestBuilder =
        acc.copy(paths = acc.paths :++ part.encode(value))

    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      NonPathLike
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    final case class AnyMethod(name: String, doc: Option[String]) extends NonPathLike[Method] {

      // TODO (KR) : put something here?
      override lazy val sources: List[RequestDecodingFailure.Source] = Nil
      override lazy val schemaAggregator: RequestSchemaAggregator = RequestSchemaAggregator.empty

      override def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[Method] =
        IntermediateRequestParseResult.Success(request.method, remainingPaths)

      override def encodeInternal(value: Method, acc: RequestBuilder): RequestBuilder =
        acc.copy(method = value)

    }

    final case class ConstMethod(method: Method) extends NonPathLike[Unit] {

      // TODO (KR) : put something here?
      override lazy val sources: List[RequestDecodingFailure.Source] = Nil
      override lazy val schemaAggregator: RequestSchemaAggregator = RequestSchemaAggregator.empty

      override def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[Unit] =
        if method == this.method then IntermediateRequestParseResult.Success((), remainingPaths)
        else IntermediateRequestParseResult.NotFound

      override def encodeInternal(value: Unit, acc: RequestBuilder): RequestBuilder =
        acc.copy(method = method)

    }

    final case class AppliedPartialQueryParam[A](part: PartialParamCodec[A], name: String, doc: Option[String]) extends NonPathLike[A] {

      override lazy val sources: List[RequestDecodingFailure.Source] = RequestDecodingFailure.Source.QueryParam(name) :: Nil
      override lazy val schemaAggregator: RequestSchemaAggregator =
        RequestSchemaAggregator.query(RequestQueryParamSchema(name, part.partialParamSchema.tpe, part.partialParamSchema.schema, doc))

      override def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[A] =
        part.decode(request.queryParams.getAll(name).toList) match
          case Right(value) => IntermediateRequestParseResult.Success(value, remainingPaths)
          case Left(error)  => IntermediateRequestParseResult.Error(makeDecodingFailure(error))

      override def encodeInternal(value: A, acc: RequestBuilder): RequestBuilder =
        part.encode(value) match
          case Nil     => acc
          case encoded => acc.copy(queryParams = acc.queryParams :+ (name, Chunk.from(encoded)))

    }

    final case class AppliedPartialHeader[A](part: PartialParamCodec[A], name: String, doc: Option[String]) extends NonPathLike[A] {

      override lazy val sources: List[RequestDecodingFailure.Source] = RequestDecodingFailure.Source.Header(name) :: Nil
      override lazy val schemaAggregator: RequestSchemaAggregator =
        RequestSchemaAggregator.header(RequestHeaderSchema(name, part.partialParamSchema.tpe, part.partialParamSchema.schema, doc))

      override def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[A] =
        part.decode(request.headers.rawHeaders(name).toList) match
          case Right(value) => IntermediateRequestParseResult.Success(value, remainingPaths)
          case Left(error)  => IntermediateRequestParseResult.Error(makeDecodingFailure(error))

      override def encodeInternal(value: A, acc: RequestBuilder): RequestBuilder =
        part.encode(value) match
          case Nil     => acc
          case encoded => acc.copy(headers = acc.headers :++ Growable.many(encoded).map(Header.Custom(name, _)))

    }

    final case class AppliedPartialBody[A](part: PartialBodyCodec[A], name: String, doc: Option[String]) extends NonPathLike[A] {

      override lazy val sources: List[RequestDecodingFailure.Source] = RequestDecodingFailure.Source.Body :: Nil
      override lazy val schemaAggregator: RequestSchemaAggregator =
        part.partialBodySchema match
          case PartialBodySchema.Empty               => RequestSchemaAggregator.body(RequestBodySchema.Empty)
          case PartialBodySchema.Single(schema)      => RequestSchemaAggregator.body(RequestBodySchema.Single(name, schema, doc))
          case _: PartialBodySchema.ServerSentEvents => throw new RuntimeException("Not supported: ServerSentEvents body in request")

      override def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[A] =
        IntermediateRequestParseResult.Effect(part.decode(request.body).mapBoth(e => makeDecodingFailure(e).some, (_, remainingPaths)))

      override def encodeInternal(value: A, acc: RequestBuilder): RequestBuilder =
        acc.copy(body = part.encode(value))

    }

    final case class TransformNonPathLike[A, B](a: NonPathLike[A], ab: A => B, ba: B => A) extends NonPathLike[B] {

      override lazy val sources: List[RequestDecodingFailure.Source] = a.sources
      override lazy val schemaAggregator: RequestSchemaAggregator = a.schemaAggregator

      override def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[B] =
        a.decodeInternal(remainingPaths, request).map(ab)

      override def encodeInternal(value: B, acc: RequestBuilder): RequestBuilder =
        a.encodeInternal(ba(value), acc)

    }

    final case class TransformOrFailNonPathLike[A, B](a: NonPathLike[A], ab: A => Either[String, B], ba: B => A) extends NonPathLike[B] {

      override lazy val sources: List[RequestDecodingFailure.Source] = a.sources
      override lazy val schemaAggregator: RequestSchemaAggregator = a.schemaAggregator

      override def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[B] =
        a.decodeInternal(remainingPaths, request).flatMap { (value, rest) =>
          ab(value) match {
            case Right(value) => IntermediateRequestParseResult.Success(value, rest)
            case Left(error)  => IntermediateRequestParseResult.Error(makeDecodingFailurePreviousValue(error, value))
          }
        }

      override def encodeInternal(value: B, acc: RequestBuilder): RequestBuilder =
        a.encodeInternal(ba(value), acc)

    }

    final case class And_NonPath[A, B, C](a: NonPathLike[A], b: NonPathLike[B], zip: Zip.Out[A, B, C]) extends NonPathLike[C], ThenLike[A, B, C]

    trait NonPathOr[A] extends NonPathLike[A], OrLike[A] {
      override val underlying: NonEmptyList[RequestCodec.NonPathLike[? <: A]]
    }

    final case class RawQueryParams(name: String, doc: Option[String]) extends NonPathLike[QueryParams] {

      override lazy val sources: List[RequestDecodingFailure.Source] = Nil
      override lazy val schemaAggregator: RequestSchemaAggregator = RequestSchemaAggregator.empty

      override def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[QueryParams] =
        IntermediateRequestParseResult.Success(request.queryParams, remainingPaths)

      override def encodeInternal(value: QueryParams, acc: RequestBuilder): RequestBuilder =
        acc.copy(queryParams = acc.queryParams ++ Growable.many(value.map.toSeq))

    }

    final case class RawHeaders(name: String, doc: Option[String]) extends NonPathLike[Headers] {

      override lazy val sources: List[RequestDecodingFailure.Source] = Nil
      override lazy val schemaAggregator: RequestSchemaAggregator = RequestSchemaAggregator.empty

      override def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[Headers] =
        IntermediateRequestParseResult.Success(request.headers, remainingPaths)

      override def encodeInternal(value: Headers, acc: RequestBuilder): RequestBuilder =
        acc.copy(headers = acc.headers ++ Growable.many(value.toSeq))

    }

    final case class RawBody(name: String, doc: Option[String]) extends NonPathLike[ReadOnlyCachedHttpBody] {

      override lazy val sources: List[RequestDecodingFailure.Source] = Nil
      override lazy val schemaAggregator: RequestSchemaAggregator = RequestSchemaAggregator.empty

      override def decodeInternal(remainingPaths: List[String], request: ReceivedRequest): IntermediateRequestParseResult[ReadOnlyCachedHttpBody] =
        IntermediateRequestParseResult.Success(request.body, remainingPaths)

      override def encodeInternal(value: ReadOnlyCachedHttpBody, acc: RequestBuilder): RequestBuilder =
        acc.copy(body = value)

    }

  }

}

given stringToRequestCodec: Conversion[String, RequestCodec.PathLike[Unit]] = RequestCodec.path.const(_)
