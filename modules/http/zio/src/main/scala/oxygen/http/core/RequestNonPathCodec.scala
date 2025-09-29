package oxygen.http.core

import oxygen.http.core.partial.{PartialBodyCodec, PartialParamCodec}
import oxygen.http.schema.{RequestBodySchema, RequestHeaderSchema, RequestQueryParamSchema}
import oxygen.http.schema.partial.RequestSchemaAggregator
import oxygen.meta.K0
import oxygen.predef.core.*
import oxygen.schema.*
import zio.*
import zio.http.{Body, Header, Headers, QueryParams}

sealed trait RequestNonPathCodec[A] {

  val sources: NonEmptyList[RequestDecodingFailure.Source]
  val schemaAggregator: RequestSchemaAggregator

  def decode(queryParams: QueryParams, headers: Headers, body: Body): ZIO[Scope, RequestDecodingFailure, A]

  /**
    * @see [[RequestNonPathCodec.encode]]
    */
  private[http] def encodeInternal(value: A, acc: RequestNonPathCodec.Builder): RequestNonPathCodec.Builder

  final def ++[B](that: RequestNonPathCodec[B])(using zip: Zip[A, B]): RequestNonPathCodec[zip.Out] = RequestNonPathCodec.And(this, that, zip)

  final def transform[B](ab: A => B, ba: B => A): RequestNonPathCodec[B] = RequestNonPathCodec.Transform(this, ab, ba)
  final def transformOrFail[B](ab: A => Either[String, B], ba: B => A): RequestNonPathCodec[B] = RequestNonPathCodec.TransformOrFail(this, ab, ba)

  inline final def autoTransform[B]: RequestNonPathCodec[B] = {
    val (ab, ba) = K0.ProductGeneric.deriveTransform[A, B]
    transform(ab, ba)
  }

}
object RequestNonPathCodec {

  object query {

    object plain {

      def required[A: PlainTextSchema](name: String, doc: Option[String] = None): RequestNonPathCodec[A] =
        RequestNonPathCodec.ApplyPartialQueryParam(PartialParamCodec.Plain.required[A], name, doc)

      def optional[A: PlainTextSchema](name: String, doc: Option[String] = None): RequestNonPathCodec[Option[A]] =
        RequestNonPathCodec.ApplyPartialQueryParam(PartialParamCodec.Plain.optional[A], name, doc)

      def many[S[_]: SeqOps, A: PlainTextSchema](name: String, doc: Option[String] = None): RequestNonPathCodec[S[A]] =
        RequestNonPathCodec.ApplyPartialQueryParam(PartialParamCodec.Plain.many[S, A], name, doc)

      def manyNonEmpty[A: PlainTextSchema](name: String, doc: Option[String] = None): RequestNonPathCodec[NonEmptyList[A]] =
        RequestNonPathCodec.ApplyPartialQueryParam(PartialParamCodec.Plain.manyNonEmpty[A], name, doc)

    }

    object json {

      def required[A: JsonSchema](name: String, doc: Option[String] = None): RequestNonPathCodec[A] =
        RequestNonPathCodec.ApplyPartialQueryParam(PartialParamCodec.Json.required[A], name, doc)

      def optional[A: JsonSchema](name: String, doc: Option[String] = None): RequestNonPathCodec[Option[A]] =
        RequestNonPathCodec.ApplyPartialQueryParam(PartialParamCodec.Json.optional[A], name, doc)

      def many[S[_]: SeqOps, A: JsonSchema](name: String, doc: Option[String] = None): RequestNonPathCodec[S[A]] =
        RequestNonPathCodec.ApplyPartialQueryParam(PartialParamCodec.Json.many[S, A], name, doc)

      def manyNonEmpty[A: JsonSchema](name: String, doc: Option[String] = None): RequestNonPathCodec[NonEmptyList[A]] =
        RequestNonPathCodec.ApplyPartialQueryParam(PartialParamCodec.Json.manyNonEmpty[A], name, doc)

    }

  }

  object header {

    object plain {

      def required[A: PlainTextSchema](name: String, doc: Option[String] = None): RequestNonPathCodec[A] =
        RequestNonPathCodec.ApplyPartialHeader(PartialParamCodec.Plain.required[A], name, doc)

      def optional[A: PlainTextSchema](name: String, doc: Option[String] = None): RequestNonPathCodec[Option[A]] =
        RequestNonPathCodec.ApplyPartialHeader(PartialParamCodec.Plain.optional[A], name, doc)

      def many[S[_]: SeqOps, A: PlainTextSchema](name: String, doc: Option[String] = None): RequestNonPathCodec[S[A]] =
        RequestNonPathCodec.ApplyPartialHeader(PartialParamCodec.Plain.many[S, A], name, doc)

      def manyNonEmpty[A: PlainTextSchema](name: String, doc: Option[String] = None): RequestNonPathCodec[NonEmptyList[A]] =
        RequestNonPathCodec.ApplyPartialHeader(PartialParamCodec.Plain.manyNonEmpty[A], name, doc)

    }

    object json {

      def required[A: JsonSchema](name: String, doc: Option[String] = None): RequestNonPathCodec[A] =
        RequestNonPathCodec.ApplyPartialHeader(PartialParamCodec.Json.required[A], name, doc)

      def optional[A: JsonSchema](name: String, doc: Option[String] = None): RequestNonPathCodec[Option[A]] =
        RequestNonPathCodec.ApplyPartialHeader(PartialParamCodec.Json.optional[A], name, doc)

      def many[S[_]: SeqOps, A: JsonSchema](name: String, doc: Option[String] = None): RequestNonPathCodec[S[A]] =
        RequestNonPathCodec.ApplyPartialHeader(PartialParamCodec.Json.many[S, A], name, doc)

      def manyNonEmpty[A: JsonSchema](name: String, doc: Option[String] = None): RequestNonPathCodec[NonEmptyList[A]] =
        RequestNonPathCodec.ApplyPartialHeader(PartialParamCodec.Json.manyNonEmpty[A], name, doc)

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builder
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private[http] final case class Builder(
      queryParams: Growable[(String, Chunk[String])],
      headers: Growable[Header.Custom],
      body: Body,
  ) {
    def build: (QueryParams, Headers, Body) = (QueryParams(queryParams.to[Seq]*), Headers(headers.toArraySeq), body)
  }
  private[http] object Builder {
    val empty: Builder = Builder(Growable.empty, Growable.empty, Body.empty)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Impls
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class ApplyPartialQueryParam[A](partial: PartialParamCodec[A], name: String, doc: Option[String]) extends RequestNonPathCodec[A] {

    override val sources: NonEmptyList[RequestDecodingFailure.Source] = NonEmptyList.one(RequestDecodingFailure.Source.QueryParam(name))
    override val schemaAggregator: RequestSchemaAggregator = RequestSchemaAggregator.query(RequestQueryParamSchema(name, partial.partialParamSchema.tpe, partial.partialParamSchema.schema, doc))

    override def decode(queryParams: QueryParams, headers: Headers, body: Body): ZIO[Scope, RequestDecodingFailure, A] =
      ZIO.fromEither { partial.decode(queryParams.getAll(name).toList).leftMap(RequestDecodingFailure(sources.toList, _)) }

    override private[http] def encodeInternal(value: A, acc: Builder): Builder = {
      val encoded = partial.encode(value)
      if (encoded.nonEmpty) acc.copy(queryParams = acc.queryParams :+ (name, Chunk.from(encoded)))
      else acc
    }

  }

  final case class ApplyPartialHeader[A](partial: PartialParamCodec[A], name: String, doc: Option[String]) extends RequestNonPathCodec[A] {

    override val sources: NonEmptyList[RequestDecodingFailure.Source] = NonEmptyList.one(RequestDecodingFailure.Source.Header(name))
    override val schemaAggregator: RequestSchemaAggregator = RequestSchemaAggregator.header(RequestHeaderSchema(name, partial.partialParamSchema.tpe, partial.partialParamSchema.schema, doc))

    override def decode(queryParams: QueryParams, headers: Headers, body: Body): ZIO[Scope, RequestDecodingFailure, A] =
      ZIO.fromEither { partial.decode(headers.rawHeaders(name).toList).leftMap(RequestDecodingFailure(sources.toList, _)) }

    override private[http] def encodeInternal(value: A, acc: Builder): Builder = {
      val encoded = partial.encode(value)
      if (encoded.nonEmpty) acc.copy(headers = acc.headers ++ Growable.many(encoded).map(Header.Custom(name, _)))
      else acc
    }

  }

  sealed trait ApplyPartialBody[A] extends RequestNonPathCodec[A]
  object ApplyPartialBody {

    def apply[A](partial: PartialBodyCodec[A], name: String, doc: Option[String]): ApplyPartialBody[A] =
      partial match {
        case PartialBodyCodec.Empty              => ApplyPartialBodyEmpty(PartialBodyCodec.Empty)
        case partial: PartialBodyCodec.Single[A] => ApplyPartialBodySingle(partial, name, doc)
      }

  }

  final case class ApplyPartialBodyEmpty(partial: PartialBodyCodec.Empty.type) extends ApplyPartialBody[Unit] {

    override val sources: NonEmptyList[RequestDecodingFailure.Source] = NonEmptyList.one(RequestDecodingFailure.Source.Body)
    override val schemaAggregator: RequestSchemaAggregator = RequestSchemaAggregator.body(RequestBodySchema.Empty)

    override def decode(queryParams: QueryParams, headers: Headers, body: Body): ZIO[Scope, RequestDecodingFailure, Unit] =
      ZIO.unit

    override private[http] def encodeInternal(value: Unit, acc: Builder): Builder =
      acc

  }

  final case class ApplyPartialBodySingle[A](partial: PartialBodyCodec.Single[A], name: String, doc: Option[String]) extends ApplyPartialBody[A] {

    override val sources: NonEmptyList[RequestDecodingFailure.Source] = NonEmptyList.one(RequestDecodingFailure.Source.Body)
    override val schemaAggregator: RequestSchemaAggregator = RequestSchemaAggregator.body(RequestBodySchema.Single(name, partial.partialBodySchema.schema, doc))

    override def decode(queryParams: QueryParams, headers: Headers, body: Body): ZIO[Scope, RequestDecodingFailure, A] =
      partial.decode(body).mapError(RequestDecodingFailure(sources.toList, _))

    override private[http] def encodeInternal(value: A, acc: Builder): Builder =
      acc.copy(body = partial.encode(value))

  }

  final case class And[A, B, C](a: RequestNonPathCodec[A], b: RequestNonPathCodec[B], zip: Zip.Out[A, B, C]) extends RequestNonPathCodec[C] {

    override val sources: NonEmptyList[RequestDecodingFailure.Source] = a.sources ++ b.sources
    override val schemaAggregator: RequestSchemaAggregator = a.schemaAggregator >>> b.schemaAggregator

    override def decode(queryParams: QueryParams, headers: Headers, body: Body): ZIO[Scope, RequestDecodingFailure, C] =
      for {
        aValue <- a.decode(queryParams, headers, body)
        bValue <- b.decode(queryParams, headers, body)
      } yield zip.zip(aValue, bValue)

    override private[http] def encodeInternal(value: C, acc: Builder): Builder = {
      val (aValue, bValue) = zip.unzip(value)
      b.encodeInternal(bValue, a.encodeInternal(aValue, acc))
    }

  }

  final case class Transform[A, B](a: RequestNonPathCodec[A], ab: A => B, ba: B => A) extends RequestNonPathCodec[B] {

    override val sources: NonEmptyList[RequestDecodingFailure.Source] = a.sources
    override val schemaAggregator: RequestSchemaAggregator = a.schemaAggregator

    override def decode(queryParams: QueryParams, headers: Headers, body: Body): ZIO[Scope, RequestDecodingFailure, B] =
      a.decode(queryParams, headers, body).map(ab)

    override private[http] def encodeInternal(value: B, acc: Builder): Builder =
      a.encodeInternal(ba(value), acc)

  }

  final case class TransformOrFail[A, B](a: RequestNonPathCodec[A], ab: A => Either[String, B], ba: B => A) extends RequestNonPathCodec[B] {

    override val sources: NonEmptyList[RequestDecodingFailure.Source] = a.sources
    override val schemaAggregator: RequestSchemaAggregator = a.schemaAggregator

    override def decode(queryParams: QueryParams, headers: Headers, body: Body): ZIO[Scope, RequestDecodingFailure, B] =
      a.decode(queryParams, headers, body).flatMap { aValue => ZIO.fromEither(ab(aValue).leftMap(e => RequestDecodingFailure(sources.toList, DecodingFailureCause.DecodeError(e)))) }

    override private[http] def encodeInternal(value: B, acc: Builder): Builder =
      a.encodeInternal(ba(value), acc)

  }

}
