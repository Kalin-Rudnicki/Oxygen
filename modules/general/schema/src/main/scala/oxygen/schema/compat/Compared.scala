package oxygen.schema.compat

import oxygen.json.Json
import oxygen.predef.core.*
import oxygen.schema.compiled.*
import scala.annotation.tailrec

sealed trait Compared[+A] {

  private[Compared] def evalInternal(
      recursive: Set[RefPair],
      calculated: Map[RefPair, ComparisonResult],
      fromSchemas: FullCompiledSchemas,
      toSchemas: FullCompiledSchemas,
  ): Compared.Result[A]

  final def eval(fromSchemas: FullCompiledSchemas, toSchemas: FullCompiledSchemas): Compared.Result[A] =
    evalInternal(Set.empty, Map.empty, fromSchemas, toSchemas)

  final def map[A2](f: A => A2): Compared[A2] = Compared.internal.Mapped(this, f)
  final def flatMap[A2](f: A => Compared[A2]): Compared[A2] = Compared.internal.FlatMapped(this, f)

}
object Compared {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def done[A](value: A): Compared[A] = Compared.internal.Done(value)

  private[Compared] def exactEqual(resolvedFrom: FullCompiledSchema, resolvedTo: FullCompiledSchema): Compared[ComparisonResult.ExactEqual] =
    Compared.done(ComparisonResult.ExactEqual(resolvedFrom, resolvedTo))
  private[Compared] def notComparable(resolvedFrom: FullCompiledSchema, resolvedTo: FullCompiledSchema): Compared[ComparisonResult.NotComparable] =
    Compared.done(ComparisonResult.NotComparable(resolvedFrom, resolvedTo))
  private[Compared] def fromIsMoreSpecific(resolvedFrom: FullCompiledSchema, resolvedTo: FullCompiledSchema): Compared[ComparisonResult.FromIsMoreSpecific] =
    Compared.done(ComparisonResult.FromIsMoreSpecific(resolvedFrom, resolvedTo))
  private[Compared] def toIsMoreSpecific(resolvedFrom: FullCompiledSchema, resolvedTo: FullCompiledSchema): Compared[ComparisonResult.ToIsMoreSpecific] =
    Compared.done(ComparisonResult.ToIsMoreSpecific(resolvedFrom, resolvedTo))

  def compareRoot(fromRef: CompiledSchemaRef, toRef: CompiledSchemaRef): Compared[ComparisonResult.Concrete] = Compared.internal.CompareRoot(fromRef, toRef)
  private[compat] def compareNonRoot(fromRef: CompiledSchemaRef, toRef: CompiledSchemaRef): Compared[ComparisonResult] = Compared.internal.CompareNonRoot(fromRef, toRef)
  private[compat] def compareNonRoot(from: FullCompiledSchema, to: FullCompiledSchema): Compared[ComparisonResult] = Compared.compareNonRoot(from.ref, to.ref)
  private[compat] def compareNonRoot(from: Lazy[? <: FullCompiledSchema], to: Lazy[? <: FullCompiledSchema]): Compared[ComparisonResult] = Compared.compareNonRoot(from.value, to.value)

  def traverse[S[_]: SeqOps, A, B](in: S[A])(f: A => Compared[B]): Compared[S[B]] = Compared.internal.Traverse(in, f)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Result
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Result[+A](result: A, roots: Set[RefPair], calculated: Map[RefPair, ComparisonResult]) {

    def rootMap: Map[RefPair, ComparisonResult] =
      roots.iterator.map { pair => (pair, calculated(pair)) }.toMap

    def map[B](f: A => B): Result[B] = copy(result = f(result))
    def flatMap[B](f: (Map[RefPair, ComparisonResult], A) => Result[B]): Result[B] = {
      val tmp: Result[B] = f(this.calculated, this.result)
      Result(tmp.result, this.roots ++ tmp.roots, tmp.calculated)
    }

    private[Compared] def addRoot(fromRef: CompiledSchemaRef, toRef: CompiledSchemaRef): Result[A] = {
      val newPair: RefPair = (fromRef, toRef)
      copy(roots = roots + newPair)
    }

    private[Compared] def addCalculated(pair: RefPair, res: ComparisonResult): Result[A] = copy(calculated = calculated.updated(pair, res))
    private[Compared] def addCalculated(pair: RefPair)(using ev: A <:< ComparisonResult): Result[A] = addCalculated(pair, ev(result))

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private object internal {

    final case class Done[+A](value: A) extends Compared[A] {

      override private[Compared] def evalInternal(
          recursive: Set[RefPair],
          calculated: Map[RefPair, ComparisonResult],
          fromSchemas: FullCompiledSchemas,
          toSchemas: FullCompiledSchemas,
      ): Result[A] =
        Result(value, Set.empty, Map.empty)

    }

    final case class CompareNonRoot(fromRef: CompiledSchemaRef, toRef: CompiledSchemaRef) extends Compared[ComparisonResult] {

      override private[Compared] def evalInternal(
          recursive: Set[RefPair],
          calculated: Map[RefPair, ComparisonResult],
          fromSchemas: FullCompiledSchemas,
          toSchemas: FullCompiledSchemas,
      ): Result[ComparisonResult] =
        Comparison.comparePreCheck(fromRef, toRef)(recursive, calculated, fromSchemas, toSchemas)

    }

    final case class CompareRoot(fromRef: CompiledSchemaRef, toRef: CompiledSchemaRef) extends Compared[ComparisonResult.Concrete] {

      override private[Compared] def evalInternal(
          recursive: Set[RefPair],
          calculated: Map[RefPair, ComparisonResult],
          fromSchemas: FullCompiledSchemas,
          toSchemas: FullCompiledSchemas,
      ): Result[ComparisonResult.Concrete] =
        CompareNonRoot(fromRef, toRef).evalInternal(recursive, calculated, fromSchemas, toSchemas).addRoot(fromRef, toRef).map {
          case res: ComparisonResult.Concrete => res
          case res                            => throw new RuntimeException(s"Internal defect : Compared.ComparedRoot return non-concrete result\n\n$res")
        }

    }

    final case class Traverse[S[_]: SeqOps as seqOps, A, B](values: S[A], f: A => Compared[B]) extends Compared[S[B]] {

      override private[Compared] def evalInternal(
          recursive: Set[RefPair],
          calculated: Map[RefPair, ComparisonResult],
          fromSchemas: FullCompiledSchemas,
          toSchemas: FullCompiledSchemas,
      ): Result[S[B]] =
        seqOps
          .newIterator(values)
          .foldLeft(Result(Growable.empty[B], Set.empty, calculated)) { (acc, a) =>
            for {
              (accCalculated, accGrow) <- acc
              aResult <- f(a).evalInternal(recursive, accCalculated, fromSchemas, toSchemas)
            } yield accGrow :+ aResult
          }
          .map { _.to[S] }

    }

    final case class Mapped[A, B](a: Compared[A], f: A => B) extends Compared[B] {

      override private[Compared] def evalInternal(
          recursive: Set[RefPair],
          calculated: Map[RefPair, ComparisonResult],
          fromSchemas: FullCompiledSchemas,
          toSchemas: FullCompiledSchemas,
      ): Result[B] =
        a.evalInternal(recursive, calculated, fromSchemas, toSchemas).map(f)

    }

    final case class FlatMapped[A, B](a: Compared[A], f: A => Compared[B]) extends Compared[B] {

      override private[Compared] def evalInternal(
          recursive: Set[RefPair],
          calculated: Map[RefPair, ComparisonResult],
          fromSchemas: FullCompiledSchemas,
          toSchemas: FullCompiledSchemas,
      ): Result[B] =
        a.evalInternal(recursive, calculated, fromSchemas, toSchemas).flatMap { (newCalculated, aResult) =>
          f(aResult).evalInternal(recursive, newCalculated, fromSchemas, toSchemas)
        }

    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Comparison
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    object Comparison {

      private type RootPlain =
        FullCompiledPlainSchema.PlainText | FullCompiledPlainSchema.FormattedText | FullCompiledPlainSchema.Enum | FullCompiledPlainSchema.EncodedText | FullCompiledPlainSchema.BearerToken

      private type RootJson =
        FullCompiledJsonSchema.JsonNumber | FullCompiledJsonSchema.JsonAST | FullCompiledJsonSchema.JsonString | FullCompiledJsonSchema.JsonArray | FullCompiledJsonSchema.JsonMap |
          FullCompiledJsonSchema.JsonProduct | FullCompiledJsonSchema.JsonSum

      @tailrec
      private def getRoot(schema: FullCompiledSchema, rTransforms: List[RawCompiledSchema.SourceFile]): (transforms: List[RawCompiledSchema.SourceFile], root: RootPlain | RootJson) =
        schema match
          case schema: RootPlain                                        => (rTransforms.reverse, schema)
          case schema: RootJson                                         => (rTransforms.reverse, schema)
          case FullCompiledPlainSchema.JsonEncoded(_, underlying)       => getRoot(underlying.value, rTransforms)
          case FullCompiledPlainSchema.Transformed(_, repr, underlying) => getRoot(underlying.value, repr.sourceFile :: rTransforms)
          case FullCompiledJsonSchema.Transformed(_, repr, underlying)  => getRoot(underlying.value, repr.sourceFile :: rTransforms)

      def comparePreCheck(
          fromRef: CompiledSchemaRef,
          toRef: CompiledSchemaRef,
      )(
          recursive: Set[RefPair],
          calculated: Map[RefPair, ComparisonResult],
          fromSchemas: FullCompiledSchemas,
          toSchemas: FullCompiledSchemas,
      ): Compared.Result[ComparisonResult] = {
        val refPair: RefPair = (fromRef, toRef)

        if recursive.contains(refPair) then Compared.Result(ComparisonResult.RecursiveReference(fromRef, toRef), Set.empty, calculated)
        else
          calculated.get(refPair) match
            case Some(value) => Compared.Result(value, Set.empty, calculated)
            case None        => comparePostCheck(fromRef, toRef)(recursive + refPair, calculated, fromSchemas, toSchemas).addCalculated(refPair)
      }

      private def comparePostCheck(
          fromRef: CompiledSchemaRef,
          toRef: CompiledSchemaRef,
      )(
          recursive: Set[RefPair],
          calculated: Map[RefPair, ComparisonResult],
          fromSchemas: FullCompiledSchemas,
          toSchemas: FullCompiledSchemas,
      ): Compared.Result[ComparisonResult] = {
        val rawFrom = fromSchemas.resolve(fromRef)
        val rawTo = fromSchemas.resolve(toRef)
        val fromRoot = getRoot(rawFrom, Nil)
        val toRoot = getRoot(rawTo, Nil)

        if fromRoot.transforms.nonEmpty || toRoot.transforms.nonEmpty then //
          Compared
            .compareNonRoot(fromRoot.root, toRoot.root)
            .map { res =>
              ComparisonResult.Transformed(
                from = rawFrom,
                to = rawTo,
                transforms = FromToValues(fromRoot.transforms, toRoot.transforms),
                underlying = res,
              )
            }
            .evalInternal(recursive, calculated, fromSchemas, toSchemas)
        else {
          val compared: Compared[ComparisonResult] =
            (fromRoot.root, toRoot.root) match
              case (resolvedFrom: RootPlain, resolvedTo: RootPlain) => plainPlain(resolvedFrom, resolvedTo)
              case (resolvedFrom: RootJson, resolvedTo: RootJson)   => jsonJson(resolvedFrom, resolvedTo)
              case (resolvedFrom: RootPlain, resolvedTo: RootJson)  => plainJson(resolvedFrom, resolvedTo)
              case (resolvedFrom: RootJson, resolvedTo: RootPlain)  => jsonPlain(resolvedFrom, resolvedTo)

          compared.evalInternal(recursive, calculated, fromSchemas, toSchemas)
        }
      }

      private object plainPlain {

        def apply(
            resolvedFrom: RootPlain,
            resolvedTo: RootPlain,
        ): Compared[ComparisonResult] =
          resolvedFrom match {
            case resolvedFrom: FullCompiledPlainSchema.PlainText     => diffPlainText(resolvedFrom, resolvedTo)
            case resolvedFrom: FullCompiledPlainSchema.FormattedText => diffFormattedText(resolvedFrom, resolvedTo)
            case resolvedFrom: FullCompiledPlainSchema.Enum          => diffEnum(resolvedFrom, resolvedTo)
            case resolvedFrom: FullCompiledPlainSchema.EncodedText   => diffEncodedText(resolvedFrom, resolvedTo)
            case resolvedFrom: FullCompiledPlainSchema.BearerToken   => diffBearerToken(resolvedFrom, resolvedTo)
          }

        private def diffPlainText(resolvedFrom: FullCompiledPlainSchema.PlainText, resolvedTo: RootPlain): Compared[ComparisonResult] =
          resolvedTo match {
            case resolvedTo: FullCompiledPlainSchema.PlainText => Compared.exactEqual(resolvedFrom, resolvedTo)
            case _                                             => Compared.toIsMoreSpecific(resolvedFrom, resolvedTo)
          }

        private def diffFormattedText(resolvedFrom: FullCompiledPlainSchema.FormattedText, resolvedTo: RootPlain): Compared[ComparisonResult] =
          resolvedTo match {
            case resolvedTo: FullCompiledPlainSchema.FormattedText =>
              for {
                underlying <- Compared.compareNonRoot(resolvedFrom.underlyingType, resolvedTo.underlyingType)
                formats = AddedRemovedBoth.Many.simpleSortedSet(resolvedFrom.formats.toSet, resolvedTo.formats.toSet)
              } yield ComparisonResult.FormattedText(resolvedFrom, resolvedTo, formats, underlying)
            case resolvedTo: FullCompiledPlainSchema.PlainText => Compared.fromIsMoreSpecific(resolvedFrom, resolvedTo)
            case _                                             => Compared.notComparable(resolvedFrom, resolvedTo)
          }

        private def diffEnum(resolvedFrom: FullCompiledPlainSchema.Enum, resolvedTo: RootPlain): Compared[ComparisonResult] =
          resolvedTo match {
            case resolvedTo: FullCompiledPlainSchema.Enum =>
              val caseSensitive = FromToValues(resolvedFrom.caseSensitive, resolvedTo.caseSensitive)
              val exhaustive = FromToValues(resolvedFrom.exhaustive, resolvedTo.exhaustive)
              val values = AddedRemovedBoth.Many.simpleSortedSet(resolvedFrom.values.toSet, resolvedTo.values.toSet)
              Compared.done(ComparisonResult.Enum(resolvedFrom, resolvedTo, caseSensitive, exhaustive, values))
            case _: FullCompiledPlainSchema.PlainText => Compared.fromIsMoreSpecific(resolvedFrom, resolvedTo)
            case _                                    => Compared.notComparable(resolvedFrom, resolvedTo)
          }

        private def diffEncodedText(resolvedFrom: FullCompiledPlainSchema.EncodedText, resolvedTo: RootPlain): Compared[ComparisonResult] =
          resolvedTo match {
            case resolvedTo: FullCompiledPlainSchema.EncodedText =>
              for {
                underlying <- Compared.compareNonRoot(resolvedFrom.underlyingType, resolvedTo.underlyingType)
                encoding = FromToValues(resolvedFrom.encoding, resolvedTo.encoding)
              } yield ComparisonResult.EncodedText(resolvedFrom, resolvedTo, encoding, underlying)
            case _: FullCompiledPlainSchema.PlainText => Compared.fromIsMoreSpecific(resolvedFrom, resolvedTo)
            case _                                    => Compared.notComparable(resolvedFrom, resolvedTo)
          }

        private def diffBearerToken(resolvedFrom: FullCompiledPlainSchema.BearerToken, resolvedTo: RootPlain): Compared[ComparisonResult] =
          resolvedTo match {
            case resolvedTo: FullCompiledPlainSchema.BearerToken =>
              for {
                underlying <- Compared.compareNonRoot(resolvedFrom.payloadType, resolvedTo.payloadType)
              } yield ComparisonResult.BearerToken(resolvedFrom, resolvedTo, underlying)
            case _: FullCompiledPlainSchema.PlainText => Compared.fromIsMoreSpecific(resolvedFrom, resolvedTo)
            case _                                    => Compared.notComparable(resolvedFrom, resolvedTo)
          }

      }

      private object jsonJson {

        def apply(
            resolvedFrom: RootJson,
            resolvedTo: RootJson,
        ): Compared[ComparisonResult] =
          resolvedFrom match {
            case resolvedFrom: FullCompiledJsonSchema.JsonNumber  => diffJsonNumber(resolvedFrom, resolvedTo)
            case resolvedFrom: FullCompiledJsonSchema.JsonAST     => diffJsonAST(resolvedFrom, resolvedTo)
            case resolvedFrom: FullCompiledJsonSchema.JsonString  => diffJsonString(resolvedFrom, resolvedTo)
            case resolvedFrom: FullCompiledJsonSchema.JsonArray   => diffJsonArray(resolvedFrom, resolvedTo)
            case resolvedFrom: FullCompiledJsonSchema.JsonMap     => diffJsonMap(resolvedFrom, resolvedTo)
            case resolvedFrom: FullCompiledJsonSchema.JsonProduct => diffJsonProduct(resolvedFrom, resolvedTo)
            case resolvedFrom: FullCompiledJsonSchema.JsonSum     => diffJsonSum(resolvedFrom, resolvedTo)
          }

        private def diffJsonNumber(resolvedFrom: FullCompiledJsonSchema.JsonNumber, resolvedTo: RootJson): Compared[ComparisonResult] =
          resolvedTo match {
            case resolvedTo: FullCompiledJsonSchema.JsonNumber =>
              val numberFormat = FromToValues(resolvedFrom.numberFormat, resolvedTo.numberFormat)
              Compared.done(ComparisonResult.JsonNumber(resolvedFrom, resolvedTo, numberFormat))
            case resolvedTo: FullCompiledJsonSchema.JsonAST if resolvedTo.jsonTypeAnyOr(Json.Type.Number) => Compared.fromIsMoreSpecific(resolvedFrom, resolvedTo)
            case _                                                                                        => Compared.notComparable(resolvedFrom, resolvedTo)
          }

        private def diffJsonAST(resolvedFrom: FullCompiledJsonSchema.JsonAST, resolvedTo: RootJson): Compared[ComparisonResult] =
          resolvedTo match {
            case resolvedTo: FullCompiledJsonSchema.JsonAST =>
              (resolvedFrom.jsonType, resolvedTo.jsonType) match {
                case (Some(fromType), Some(toType)) => Compared.done(ComparisonResult.JsonAST(resolvedFrom, resolvedTo, FromToValues(fromType, toType)))
                case (Some(_), None)                => Compared.fromIsMoreSpecific(resolvedFrom, resolvedTo)
                case (None, Some(_))                => Compared.toIsMoreSpecific(resolvedFrom, resolvedTo)
                case (None, None)                   => Compared.exactEqual(resolvedFrom, resolvedTo)
              }
            case _: FullCompiledJsonSchema.JsonString if resolvedFrom.jsonTypeAnyOr(Json.Type.String)  => Compared.toIsMoreSpecific(resolvedFrom, resolvedTo)
            case _: FullCompiledJsonSchema.JsonArray if resolvedFrom.jsonTypeAnyOr(Json.Type.Array)    => Compared.toIsMoreSpecific(resolvedFrom, resolvedTo)
            case _: FullCompiledJsonSchema.JsonMap if resolvedFrom.jsonTypeAnyOr(Json.Type.Object)     => Compared.toIsMoreSpecific(resolvedFrom, resolvedTo)
            case _: FullCompiledJsonSchema.JsonProduct if resolvedFrom.jsonTypeAnyOr(Json.Type.Object) => Compared.toIsMoreSpecific(resolvedFrom, resolvedTo)
            case _: FullCompiledJsonSchema.JsonSum if resolvedFrom.jsonTypeAnyOr(Json.Type.Object)     => Compared.toIsMoreSpecific(resolvedFrom, resolvedTo)
            case _                                                                                     => Compared.notComparable(resolvedFrom, resolvedTo)
          }

        private def diffJsonString(resolvedFrom: FullCompiledJsonSchema.JsonString, resolvedTo: RootJson): Compared[ComparisonResult] =
          resolvedTo match {
            case resolvedTo: FullCompiledJsonSchema.JsonString =>
              for {
                underlying <- Compared.compareNonRoot(resolvedFrom.elemType, resolvedTo.elemType)
              } yield ComparisonResult.JsonString(resolvedFrom, resolvedTo, underlying)
            case resolvedTo: FullCompiledJsonSchema.JsonAST if resolvedTo.jsonTypeAnyOr(Json.Type.String) => Compared.fromIsMoreSpecific(resolvedFrom, resolvedTo)
            case _                                                                                        => Compared.notComparable(resolvedFrom, resolvedTo)
          }

        private def diffJsonArray(resolvedFrom: FullCompiledJsonSchema.JsonArray, resolvedTo: RootJson): Compared[ComparisonResult] =
          resolvedTo match {
            case resolvedTo: FullCompiledJsonSchema.JsonArray =>
              for {
                underlying <- Compared.compareNonRoot(resolvedFrom.elemType, resolvedTo.elemType)
              } yield ComparisonResult.JsonArray(resolvedFrom, resolvedTo, underlying)
            case resolvedTo: FullCompiledJsonSchema.JsonAST if resolvedTo.jsonTypeAnyOr(Json.Type.Array) => Compared.fromIsMoreSpecific(resolvedFrom, resolvedTo)
            case _                                                                                       => Compared.notComparable(resolvedFrom, resolvedTo)
          }

        private def diffJsonMap(resolvedFrom: FullCompiledJsonSchema.JsonMap, resolvedTo: RootJson): Compared[ComparisonResult] =
          resolvedTo match {
            case resolvedTo: FullCompiledJsonSchema.JsonMap =>
              for {
                keyUnderlying <- Compared.compareNonRoot(resolvedFrom.keyType, resolvedTo.keyType)
                valueUnderlying <- Compared.compareNonRoot(resolvedFrom.valueType, resolvedTo.valueType)
              } yield ComparisonResult.JsonMap(resolvedFrom, resolvedTo, keyUnderlying, valueUnderlying)
            case resolvedTo: FullCompiledJsonSchema.JsonAST if resolvedTo.jsonTypeAnyOr(Json.Type.Object) => Compared.fromIsMoreSpecific(resolvedFrom, resolvedTo)
            case _                                                                                        => Compared.notComparable(resolvedFrom, resolvedTo)
          }

        private def compareFields(from: FullCompiledJsonSchema.ProductField, to: FullCompiledJsonSchema.ProductField): Compared[ComparisonResult.FieldComparison] =
          for {
            underlying <- Compared.compareNonRoot(from.fieldType, to.fieldType)
            nullable = FromToValues(from.nullable, to.nullable)
            onMissing = FromToValues(from.onMissing, to.onMissing)
          } yield ComparisonResult.FieldComparison(from.fieldName, nullable, onMissing, underlying)

        private def compareCases(from: FullCompiledJsonSchema.SumCase, to: FullCompiledJsonSchema.SumCase): Compared[ComparisonResult.CaseComparison] =
          for {
            underlying <- Compared.compareNonRoot(from.caseType, to.caseType)
          } yield ComparisonResult.CaseComparison(from.caseName, underlying)

        // TODO (KR) : handle case between product<->sum which uses discriminator
        private def diffJsonProduct(resolvedFrom: FullCompiledJsonSchema.JsonProduct, resolvedTo: RootJson): Compared[ComparisonResult] =
          resolvedTo match {
            case resolvedTo: FullCompiledJsonSchema.JsonProduct =>
              for {
                fields <- AddedRemovedBoth.Many.fromSeqs(resolvedFrom.fields, resolvedTo.fields, _.fieldName)(compareFields)
              } yield ComparisonResult.JsonProduct(resolvedFrom, resolvedTo, fields)
            case resolvedTo: FullCompiledJsonSchema.JsonAST if resolvedTo.jsonTypeAnyOr(Json.Type.Object) => Compared.fromIsMoreSpecific(resolvedFrom, resolvedTo)
            case _                                                                                        => Compared.notComparable(resolvedFrom, resolvedTo)
          }

        // TODO (KR) : handle case between product<->sum which uses discriminator
        private def diffJsonSum(resolvedFrom: FullCompiledJsonSchema.JsonSum, resolvedTo: RootJson): Compared[ComparisonResult] =
          resolvedTo match {
            case resolvedTo: FullCompiledJsonSchema.JsonSum =>
              for {
                cases <- AddedRemovedBoth.Many.fromSeqs(resolvedFrom.cases, resolvedTo.cases, _.caseName)(compareCases)
                discriminator = FromToValues(resolvedFrom.discriminator, resolvedTo.discriminator)
              } yield ComparisonResult.JsonSum(resolvedFrom, resolvedTo, discriminator, cases)
            case resolvedTo: FullCompiledJsonSchema.JsonAST if resolvedTo.jsonTypeAnyOr(Json.Type.Object) => Compared.fromIsMoreSpecific(resolvedFrom, resolvedTo)
            case _                                                                                        => Compared.notComparable(resolvedFrom, resolvedTo)
          }

      }

      private def plainJson(
          resolvedFrom: RootPlain,
          resolvedTo: RootJson,
      ): Compared[ComparisonResult] =
        resolvedFrom match
          case _: FullCompiledPlainSchema.PlainText => Compared.toIsMoreSpecific(resolvedFrom, resolvedTo)
          case _                                    => Compared.toIsMoreSpecific(resolvedFrom, resolvedTo)

      private def jsonPlain(
          resolvedFrom: RootJson,
          resolvedTo: RootPlain,
      ): Compared[ComparisonResult] =
        resolvedTo match
          case _: FullCompiledPlainSchema.PlainText => Compared.fromIsMoreSpecific(resolvedFrom, resolvedTo)
          case _                                    => Compared.notComparable(resolvedFrom, resolvedTo)

    }

  }

}
