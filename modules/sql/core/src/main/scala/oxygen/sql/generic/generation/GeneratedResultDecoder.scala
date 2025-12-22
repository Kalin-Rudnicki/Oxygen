package oxygen.sql.generic.generation

import oxygen.meta.k0.*
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.TypeclassExpr
import oxygen.sql.generic.typeclass.*
import oxygen.sql.schema.*
import scala.quoted.*

final class GeneratedResultDecoder private (
    private val parts: Growable[GeneratedResultDecoder.Part],
) {

  def ++(that: GeneratedResultDecoder): GeneratedResultDecoder =
    GeneratedResultDecoder(this.parts ++ that.parts)

  def buildExpr(using quotes: Quotes): GeneratedResultDecoder.Built = {
    val many: ArraySeq[GeneratedResultDecoder.Part] = parts.toArraySeq
    many.length match {
      case 0 =>
        GeneratedResultDecoder.Built(
          TypeRepr.of[Unit],
          false,
          '{ ResultDecoder.Empty },
        )
      case 1 =>
        val single = many(0)
        GeneratedResultDecoder.Built(
          single.tpe,
          true,
          single.buildExpr.expr,
        )
      case _ =>
        type A
        given tupGeneric: ProductGeneric[A] = ProductGeneric.ofTuple[A](many.map(_.tpe).toList)
        val fTpe: Type[ResultDecoder] = Type.of[ResultDecoder]
        val instances: Expressions[ResultDecoder, A] = Expressions(fTpe, tupGeneric.tpe, many.map(_.toElem))

        GeneratedResultDecoder.Built(
          tupGeneric.typeRepr,
          true,
          DeriveProductResultDecoder[A](instances)(using quotes, fTpe, tupGeneric.tpe, tupGeneric).derive,
        )
    }
  }

}
object GeneratedResultDecoder {

  private final case class Part(buildExpr: TypeclassExpr.ResultDecoder, tpe: TypeRepr) {

    def toElem[A]: Expressions.Elem[ResultDecoder, A] = {
      given typed: Type[A] = tpe.asTypeOf
      Expressions.Elem[ResultDecoder, A](
        typed,
        buildExpr.expr.asExprOf[ResultDecoder[A]],
      )
    }

  }

  final case class Built(
      tpe: TypeRepr,
      nonEmpty: Boolean,
      expr: Expr[ResultDecoder[?]],
  )

  val empty: GeneratedResultDecoder = GeneratedResultDecoder(Growable.empty)

  def single(dec: TypeclassExpr.ResultDecoder, tpe: TypeRepr): GeneratedResultDecoder = GeneratedResultDecoder(Growable.single(Part(dec, tpe)))

  def flatten[S[_]: SeqOps](all: S[GeneratedResultDecoder]): GeneratedResultDecoder = GeneratedResultDecoder(Growable.manyFlatMapped(all)(_.parts))

}
