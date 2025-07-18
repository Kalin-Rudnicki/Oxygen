package oxygen.sql.generic.generation

import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.typeclass.*
import oxygen.sql.schema.*
import scala.quoted.*

final class GeneratedResultDecoder private (
    private val parts: Growable[GeneratedResultDecoder.Part],
) {

  def ++(that: GeneratedResultDecoder): GeneratedResultDecoder =
    GeneratedResultDecoder(this.parts ++ that.parts)

  def buildExpr(using quotes: Quotes): GeneratedResultDecoder.Built =
    parts.toContiguous match {
      case Contiguous() =>
        GeneratedResultDecoder.Built(
          TypeRepr.of[Unit],
          false,
          '{ ResultDecoder.Empty },
        )
      case Contiguous(single) =>
        GeneratedResultDecoder.Built(
          single.tpe,
          true,
          single.buildExpr,
        )
      case many =>
        type A
        given tupGeneric: K0.ProductGeneric[A] = K0.ProductGeneric.ofTuple[A](many.map(_.tpe).toList)
        val fTpe: Type[ResultDecoder] = Type.of[ResultDecoder]
        val instances: K0.Expressions[ResultDecoder, A] = K0.Expressions(Type.of[ResultDecoder], tupGeneric.tpe, many.map(_.toElem))

        GeneratedResultDecoder.Built(
          tupGeneric.typeRepr,
          true,
          DeriveProductResultDecoder[A](instances)(using quotes, fTpe, tupGeneric.tpe, tupGeneric).derive,
        )
    }

}
object GeneratedResultDecoder {

  private final case class Part(buildExpr: Expr[ResultDecoder[?]], tpe: TypeRepr) {

    def toElem[A](using Quotes): K0.Expressions.Elem[ResultDecoder, A] = {
      given typed: Type[A] = tpe.asTypeOf
      K0.Expressions.Elem[ResultDecoder, A](
        typed,
        buildExpr.asExprOf[ResultDecoder[A]],
      )
    }

  }

  final case class Built(
      tpe: TypeRepr,
      nonEmpty: Boolean,
      expr: Expr[ResultDecoder[?]],
  )

  val empty: GeneratedResultDecoder = GeneratedResultDecoder(Growable.empty)

  def single(dec: Expr[ResultDecoder[?]], tpe: TypeRepr): GeneratedResultDecoder = GeneratedResultDecoder(Growable.single(Part(dec, tpe)))

  def flatten[S[_]: SeqOps](all: S[GeneratedResultDecoder]): GeneratedResultDecoder = GeneratedResultDecoder(Growable.manyFlatMapped(all)(_.parts))

}
