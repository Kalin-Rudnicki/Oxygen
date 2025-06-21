package oxygen.sql.generic

import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.schema.*
import scala.quoted.*

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      GeneratedSql
//////////////////////////////////////////////////////////////////////////////////////////////////////

private[generic] final case class GeneratedSql private (sqls: Growable[Expr[String]]) {

  def ++(that: GeneratedSql): GeneratedSql = GeneratedSql(this.sqls ++ that.sqls)

  def parens(using Quotes): GeneratedSql = GeneratedSql.of("(", this, ")")
  def parensIf(cond: Boolean)(using Quotes): GeneratedSql = if (cond) this.parens else this

  def buildExpr(using Quotes): Expr[String] =
    sqls.exprMkString

}
private[generic] object GeneratedSql {

  val empty: GeneratedSql = GeneratedSql(Growable.empty)
  def single(expr: Expr[String]): GeneratedSql = GeneratedSql(Growable.single(expr))
  def const(str: String)(using Quotes): GeneratedSql = single(Expr(str))

  def option(sql: Option[GeneratedSql]): GeneratedSql = sql.getOrElse(empty)
  def seq(sqls: Seq[GeneratedSql]): GeneratedSql = GeneratedSql(Growable.many(sqls).flatMap(_.sqls))
  def nel(sqls: NonEmptyList[GeneratedSql]): GeneratedSql = seq(sqls.toList)
  def of(sqls: (String | GeneratedSql)*)(using Quotes): GeneratedSql =
    seq(
      sqls.map {
        case sql: GeneratedSql => sql
        case str: String       => const(str)
      },
    )

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      GeneratedInputEncoder
//////////////////////////////////////////////////////////////////////////////////////////////////////

private[generic] final case class GeneratedInputEncoder(inputs: Growable[GeneratedInputEncoder.Elem]) {

  def ++(that: GeneratedInputEncoder): GeneratedInputEncoder =
    GeneratedInputEncoder(this.inputs ++ that.inputs)

  def buildExpr[I: Type](using Quotes): Expr[InputEncoder[I]] =
    inputs.toContiguous match {
      case Contiguous() =>
        '{ InputEncoder.Empty }
      case Contiguous(single) =>
        single.buildExpr[I]
      case many =>
        '{ InputEncoder.ConcatAll[I](${ many.map(_.buildExpr[I]).seqToExpr }) }
    }

}
private[generic] object GeneratedInputEncoder {

  final case class Elem(
      typeRepr: TypeRepr,
      fromInput: Option[Expr[Any] => Expr[Any]],
      encoder: Expr[InputEncoder[?]],
  ) {

    def buildExpr[I: Type](using Quotes): Expr[InputEncoder[I]] =
      fromInput match {
        case Some(fromInput) =>
          type MyI
          given Type[MyI] = typeRepr.asTypeOf
          val typedEncoder: Expr[InputEncoder[MyI]] = encoder.asExprOf[InputEncoder[MyI]]

          val f: Expr[I => MyI] =
            '{ (i: I) => ${ fromInput('i).asExprOf[MyI] } }

          '{ $typedEncoder.contramap[I]($f) }
        case None =>
          encoder.asExprOf[InputEncoder[I]]
      }

  }

  val empty: GeneratedInputEncoder = GeneratedInputEncoder(Growable.empty)
  def single(typeRepr: TypeRepr, fromInput: Option[Expr[Any] => Expr[Any]], encoder: Expr[InputEncoder[?]]): GeneratedInputEncoder =
    GeneratedInputEncoder(Growable.single(Elem(typeRepr, fromInput, encoder)))
  def option(opt: Option[GeneratedInputEncoder]): GeneratedInputEncoder = opt.getOrElse(empty)
  def seq(seq: Seq[GeneratedInputEncoder]): GeneratedInputEncoder = GeneratedInputEncoder(Growable.many(seq).flatMap(_.inputs))
  def nel(nel: NonEmptyList[GeneratedInputEncoder]): GeneratedInputEncoder = seq(nel.toList)

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Decoding Result
//////////////////////////////////////////////////////////////////////////////////////////////////////

private[generic] final case class GeneratedResultDecoder private (decoders: Growable[K0.Expressions.Elem[ResultDecoder, ?]]) {

  def ++(that: GeneratedResultDecoder): GeneratedResultDecoder = GeneratedResultDecoder(this.decoders ++ that.decoders)

  def buildExpr[O: Type](using Quotes): Expr[ResultDecoder[O]] =
    decoders.toContiguous match {
      case Contiguous() =>
        '{ ResultDecoder.Empty }.asExprOf[ResultDecoder[O]]
      case Contiguous(single) =>
        single.expr.asExprOf[ResultDecoder[O]]
      case many =>
        type O2
        given tupGeneric: K0.ProductGeneric[O2] = K0.ProductGeneric.ofTuple[O2](many.map(_.bTpe.toTypeRepr).toList)
        given Type[O2] = tupGeneric.tpe
        val exprs: K0.Expressions[ResultDecoder, O2] = K0.Expressions(Type.of[ResultDecoder], tupGeneric.tpe, many)
        val instanceExpr: Expr[ResultDecoder[O2]] = DeriveProductResultDecoder(exprs).derive

        instanceExpr.asExprOf[ResultDecoder[O]]
    }

}
private[generic] object GeneratedResultDecoder {

  val empty: GeneratedResultDecoder = GeneratedResultDecoder(Growable.empty)
  def single(tpe: TypeRepr, expr: Expr[ResultDecoder[?]]): GeneratedResultDecoder = GeneratedResultDecoder(Growable.single(K0.Expressions.Elem(tpe.asTypeOf, expr)))
  def option(decoder: Option[GeneratedResultDecoder]): GeneratedResultDecoder = decoder.getOrElse(GeneratedResultDecoder.empty)
  def seq(decoders: Seq[GeneratedResultDecoder]): GeneratedResultDecoder = GeneratedResultDecoder(Growable.many(decoders).flatMap(_.decoders))
  def nel(decoders: NonEmptyList[GeneratedResultDecoder]): GeneratedResultDecoder = GeneratedResultDecoder.seq(decoders.toList)

}
