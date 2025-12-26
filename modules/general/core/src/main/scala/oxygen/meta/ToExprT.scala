package oxygen.meta

import oxygen.core.*
import oxygen.core.syntax.all.*
import oxygen.core.typeclass.*
import oxygen.meta.InternalHelpers.*
import oxygen.meta.k0.*
import oxygen.quoted.*
import scala.collection.immutable.ArraySeq
import scala.quoted.*

trait ToExprT[A] {
  def apply(x: A)(using Type[A], Quotes): Expr[A]
}
object ToExprT extends Derivable[ToExprT] {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class AppliedType[A](tpe: Type[A], tet: ToExprT[A]) extends ToExpr[A] {
    override def apply(x: A)(using quotes: Quotes): Expr[A] = tet.apply(x)(using tpe, quotes)
  }

  private final case class Wrapped[A](wrapped: ToExpr[A]) extends ToExprT[A] {
    override def apply(x: A)(using Type[A], Quotes): Expr[A] = wrapped.apply(x)
  }

  def applied[A](tpe: Type[A], tet: ToExprT[A]): ToExpr[A] = tet match
    case Wrapped(wrapped) => wrapped
    case _                => AppliedType(tpe, tet)

  given string: ToExprT[String] = Wrapped(ToExpr.StringToExpr)
  given char: ToExprT[Char] = Wrapped(ToExpr.CharToExpr)
  given boolean: ToExprT[Boolean] = Wrapped(ToExpr.BooleanToExpr)
  given byte: ToExprT[Byte] = Wrapped(ToExpr.ByteToExpr)
  given short: ToExprT[Short] = Wrapped(ToExpr.ShortToExpr)
  given int: ToExprT[Int] = Wrapped(ToExpr.IntToExpr)
  given long: ToExprT[Long] = Wrapped(ToExpr.LongToExpr)
  given float: ToExprT[Float] = Wrapped(ToExpr.FloatToExpr)
  given double: ToExprT[Double] = Wrapped(ToExpr.DoubleToExpr)
  given `class`: [T <: Class[?]] => ToExprT[T] = Wrapped(ToExpr.ClassToExpr)
  // TODO (KR) : ClassTag[?]

  given option: [A: ToExprT as aToExpr] => ToExprT[Option[A]] =
    new ToExprT[Option[A]] {

      override def apply(x: Option[A])(using parentTpe: Type[Option[A]], quotes: Quotes): Expr[Option[A]] = {
        val _aType: TypeRepr = parentTpe match
          case '[Option[a]] => TypeRepr.of[a]
          case _            => report.errorAndAbort(s"Unable to extract A type from Option[A]: ${parentTpe.toTypeRepr.showAnsiCode}")
        given Type[A] = _aType.asTypeOf

        x match
          case Some(x) => '{ Some(${ aToExpr.apply(x) }) }
          case None    => '{ None }
      }

    }

  given either: [A: ToExprT as aToExpr, B: ToExprT as bToExpr] => ToExprT[Either[A, B]] =
    new ToExprT[Either[A, B]] {

      override def apply(x: Either[A, B])(using parentTpe: Type[Either[A, B]], quotes: Quotes): Expr[Either[A, B]] = {
        val (_aType, _bType): (TypeRepr, TypeRepr) = parentTpe match
          case '[Either[a, b]] => (TypeRepr.of[a], TypeRepr.of[b])
          case _               => report.errorAndAbort(s"Unable to extract A/B type from Either[A, B]: ${parentTpe.toTypeRepr.showAnsiCode}")
        given Type[A] = _aType.asTypeOf
        given Type[B] = _bType.asTypeOf

        x match
          case Right(value) => '{ Right(${ bToExpr.apply(value) }) }
          case Left(value)  => '{ Left(${ aToExpr.apply(value) }) }
      }

    }

  given map: [A: ToExprT as aToExpr, B: ToExprT as bToExpr] => ToExprT[Map[A, B]] =
    new ToExprT[Map[A, B]] {

      override def apply(x: Map[A, B])(using Type[Map[A, B]], Quotes): Expr[Map[A, B]] = {
        val (_, aType, bType) = Type.unwrap2[Map, A, B]
        given Type[A] = aType
        given Type[B] = bType

        val tmp1: Seq[(A, B)] = x.toSeq
        val tmp2: Seq[Expr[(A, B)]] = tmp1.map { case (a, b) => '{ (${ aToExpr.apply(a) }, ${ bToExpr.apply(b) }) } }
        val tmp3: Expr[Seq[(A, B)]] = Expr.ofSeq(tmp2)

        '{ Map($tmp3*) }
      }

    }

  given seq: [S[_]: SeqOps, A] => (aToExpr: ToExprT[A]) => ToExprT[S[A]] =
    new ToExprT[S[A]] {
      override def apply(x: S[A])(using Type[S[A]], Quotes): Expr[S[A]] = {
        val (sType, aType) = Type.unwrap1[S, A]
        given Type[S] = sType
        given Type[A] = aType

        val tmp1: S[Expr[A]] = x.map(aToExpr.apply(_))

        tmp1.seqToExpr
      }
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override protected def productDeriver[A](using
      stage1Quotes: Quotes,
      toExprT: Type[ToExprT],
      stage1Type: Type[A],
      stage1Generic: ProductGeneric[A],
      der: Derivable[ToExprT],
  ): Derivable.ProductDeriver[ToExprT, A] =
    Derivable.ProductDeriver.withInstances[ToExprT, A] { instances =>
      new Derivable.ProductDeriver[ToExprT, A] {

        private def applyImpl(
            stage2Value: Stage1.Expr[A],
            stage2Generic: Stage1.Expr[ProductGeneric[A]],
            stage2Quotes: Stage1.Expr[Stage2.Quotes],
        ): Stage1.Expr[Stage2.Expr[A]] = {
          val arraySeqExprExpr: ArraySeq[Stage1.Expr[Stage2.Expr[?]]] =
            stage1Generic.mapChildren
              .mapExpr[Stage2.Expr[?]] { [a] => (_, _) ?=> (field: stage1Generic.Field[a]) =>
                val toExprExpr: Stage1.Expr[ToExprT[a]] = field.getExpr(instances)
                val stage2ValueAExpr: Stage1.Expr[a] = field.fromParent(stage2Value)
                '{ $toExprExpr.apply($stage2ValueAExpr)(using Type.of[a](using $stage2Quotes), $stage2Quotes) }
              }
              .toArraySeq

          val exprArraySeqExpr: Stage1.Expr[ArraySeq[Stage2.Expr[?]]] =
            arraySeqExprExpr.seqToArraySeqExpr

          '{
            val s2g: ProductGeneric[A] = $stage2Generic
            given s2q: Quotes = $stage2Quotes
            s2g.fieldsToInstance($exprArraySeqExpr)
          }
        }

        override def derive: Expr[ToExprT[A]] =
          '{
            new ToExprT[A] {

              override def apply(x: A)(using stage2Type: Stage2.Type[A], stage2Quotes: Stage2.Quotes): Stage2.Expr[A] = {
                val stage2Generic: ProductGeneric[A] = ProductGeneric.of[A](using stage2Type, stage2Quotes)
                ${ applyImpl('x, 'stage2Generic, 'stage2Quotes) }
              }

            }
          }

      }
    }

  override protected def sumDeriver[A](using
      stage1Quotes: Quotes,
      toExprT: Type[ToExprT],
      stage1Type: Type[A],
      stage1Generic: SumGeneric[A],
      der: Derivable[ToExprT],
  ): Derivable.SumDeriver[ToExprT, A] =
    Derivable.SumDeriver.withInstances[ToExprT, A] { instances =>
      new Derivable.SumDeriver[ToExprT, A] {

        private def applyImpl(
            stage2Value: Stage1.Expr[A],
            stage2Quotes: Stage1.Expr[Stage2.Quotes],
        ): Stage1.Expr[Stage2.Expr[A]] =
          stage1Generic.matcher.instance(stage2Value) { [a <: A] => (_, _) ?=> (kase: stage1Generic.Case[a]) =>
            kase.caseExtractor.withRHS { stage2ValueA =>
              '{
                ${ kase.getExpr(instances) }.apply($stage2ValueA)(using Type.of[a](using $stage2Quotes), $stage2Quotes)
              }
            }
          }

        override def derive: Expr[ToExprT[A]] =
          '{
            new ToExprT[A] {

              override def apply(x: A)(using stage2Type: Stage2.Type[A], stage2Quotes: Stage2.Quotes): Stage2.Expr[A] =
                ${ applyImpl('x, 'stage2Quotes) }

            }
          }

      }
    }

  override inline def derived[A]: ToExprT[A] = ${ derivedImpl[A] }

}

given toExprT_to_toExpr: [A: {Type as tpe, ToExprT as tet}] => ToExpr[A] = ToExprT.applied(tpe, tet)
