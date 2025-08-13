package oxygen.sql.generic.generation

import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.schema.InputEncoder
import scala.quoted.*

final class GeneratedInputEncoder private (
    private val parts: Growable[GeneratedInputEncoder.Part],
) {

  def ++(that: GeneratedInputEncoder): GeneratedInputEncoder =
    GeneratedInputEncoder(this.parts ++ that.parts)

  def buildExpr(using Quotes): GeneratedInputEncoder.Built = {
    val many: ArraySeq[GeneratedInputEncoder.Part] = parts.toArraySeq
    many.length match {
      case 0 =>
        GeneratedInputEncoder.Built(
          TypeRepr.of[Any],
          false,
          false,
          '{ InputEncoder.Empty },
        )
      case 1 =>
        val single = many(0)
        GeneratedInputEncoder.Built(
          single.tpe,
          !single.isConst,
          true,
          single.buildExpr,
        )
      case _ =>
        type A
        val repr = TypeRepr.contravariantJoin(many.map(_.tpe))
        given Type[A] = repr.asTypeOf

        val encs: ArraySeq[Expr[InputEncoder[A]]] = many.map(_.buildExpr.asExprOf[InputEncoder[A]])

        GeneratedInputEncoder.Built(
          repr,
          many.exists(!_.isConst),
          true,
          '{ InputEncoder.ConcatAll[A](${ encs.seqToArraySeqExpr }) },
        )
    }
  }

}
object GeneratedInputEncoder {

  private final case class Part(buildExpr: Expr[InputEncoder[?]], isConst: Boolean, tpe: TypeRepr)

  final case class Built(
      tpe: TypeRepr,
      hasNonConstParams: Boolean,
      hasParams: Boolean,
      expr: Expr[InputEncoder[?]],
  )

  val empty: GeneratedInputEncoder = GeneratedInputEncoder(Growable.empty)

  def const(inputEncoder: Expr[InputEncoder[Any]])(using Quotes): GeneratedInputEncoder = GeneratedInputEncoder(Growable.single(Part(inputEncoder, true, TypeRepr.of[Any])))

  def nonConst(inputEncoder: Expr[InputEncoder[?]], tpe: TypeRepr): GeneratedInputEncoder = GeneratedInputEncoder(Growable.single(Part(inputEncoder, false, tpe)))

  def option(fragment: Option[GeneratedInputEncoder]): GeneratedInputEncoder = fragment.getOrElse(GeneratedInputEncoder.empty)

  def flatten[S[_]: SeqOps](all: S[GeneratedInputEncoder]): GeneratedInputEncoder = GeneratedInputEncoder(Growable.manyFlatMapped(all)(_.parts))

}
