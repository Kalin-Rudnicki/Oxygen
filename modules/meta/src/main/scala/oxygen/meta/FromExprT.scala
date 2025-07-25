package oxygen.meta

import oxygen.meta.InternalHelpers.*
import oxygen.meta.K0.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

trait FromExprT[A] { self =>

  def unapply(x: Expr[A])(using Type[A], Quotes): Option[A]

  object fromTerm {

    def unapply(x: Term)(using Type[A], Quotes): Option[A] = {
      val expr: Expr[?] = x.asExpr
      if (expr.isExprOf[A]) self.unapply(expr.asExprOf[A])
      else None
    }

  }

}
object FromExprT extends Derivable[FromExprT] {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class AppliedType[A](tpe: Type[A], fet: FromExprT[A]) extends FromExpr[A] {
    override def unapply(x: Expr[A])(using quotes: Quotes): Option[A] = fet.unapply(x)(using tpe, quotes)
  }

  private final case class Wrapped[A](wrapped: FromExpr[A]) extends FromExprT[A] {
    override def unapply(x: Expr[A])(using Type[A], Quotes): Option[A] = wrapped.unapply(x)
  }

  def applied[A](tpe: Type[A], fet: FromExprT[A]): FromExpr[A] = fet match
    case Wrapped(wrapped) => wrapped
    case _                => AppliedType(tpe, fet)

  given string: FromExprT[String] = Wrapped(FromExpr.StringFromExpr)
  given char: FromExprT[Char] = Wrapped(FromExpr.CharFromExpr)
  given boolean: FromExprT[Boolean] = Wrapped(FromExpr.BooleanFromExpr)
  given byte: FromExprT[Byte] = Wrapped(FromExpr.ByteFromExpr)
  given short: FromExprT[Short] = Wrapped(FromExpr.ShortFromExpr)
  given int: FromExprT[Int] = Wrapped(FromExpr.IntFromExpr)
  given long: FromExprT[Long] = Wrapped(FromExpr.LongFromExpr)
  given float: FromExprT[Float] = Wrapped(FromExpr.FloatFromExpr)
  given double: FromExprT[Double] = Wrapped(FromExpr.DoubleFromExpr)

  given option: [A: FromExprT as aFromExpr] => FromExprT[Option[A]] =
    new FromExprT[Option[A]] {

      override def unapply(x: Expr[Option[A]])(using Type[Option[A]], Quotes): Option[Option[A]] = {
        val (_, aType) = Type.unwrap1[Option, A]
        given Type[A] = aType

        x match
          case '{ Option[A](${ aFromExpr(a) }) }   => Option(a).some
          case '{ Some[A](${ aFromExpr(a) }) }     => a.some.some
          case '{ new Some[A](${ aFromExpr(a) }) } => a.some.some
          case '{ (${ aFromExpr(a) }: A).some }    => a.some.some
          case '{ None }                           => None.some
          case '{ Option.empty[A] }                => None.some
          case _                                   => None
      }

    }

  given either: [A: FromExprT as aFromExpr, B: FromExprT as bFromExpr] => FromExprT[Either[A, B]] =
    new FromExprT[Either[A, B]] {

      override def unapply(x: Expr[Either[A, B]])(using Type[Either[A, B]], Quotes): Option[Either[A, B]] = {
        val (_, aType, bType) = Type.unwrap2[Either, A, B]
        given Type[A] = aType
        given Type[B] = bType

        x match
          case '{ (${ bFromExpr(a) }: B).asRight }     => a.asRight.some
          case '{ Right[t, B](${ bFromExpr(a) }) }     => a.asRight.some
          case '{ new Right[t, B](${ bFromExpr(a) }) } => a.asRight.some
          case '{ (${ aFromExpr(a) }: A).asLeft }      => a.asLeft.some
          case '{ Left[A, t](${ aFromExpr(a) }) }      => a.asLeft.some
          case '{ new Left[A, t](${ aFromExpr(a) }) }  => a.asLeft.some
          case _                                       => None
      }

    }

  // TODO (KR) : Seq

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override protected def productDeriver[A](using
      stage1Quotes: Quotes,
      fromExprT: Type[FromExprT],
      stage1Type: Type[A],
      stage1Generic: ProductGeneric[A],
      der: Derivable[FromExprT],
  ): Derivable.ProductDeriver[FromExprT, A] =
    Derivable.ProductDeriver.withInstances[FromExprT, A] { instances =>
      new Derivable.ProductDeriver.Split[FromExprT, A] {

        // TODO (KR) : support default args and named args
        private def unapplyImpl(
            stage2Args: Stage1.Expr[Contiguous[Stage2.Term]],
            stage2Quotes: Stage1.Expr[Stage2.Quotes],
        ): Stage1.Expr[Option[A]] = {
          val tmp: Stage1.Expr[Option[A]] =
            stage1Generic.instantiate.option { [a] => (_, _) ?=> (field: stage1Generic.Field[a]) =>
              val instExpr: Expr[FromExprT[a]] = field.getExpr(instances)
              '{
                val term: Term = $stage2Args.at(${ Expr(field.idx) }).removeInlineAndTyped
                $instExpr.fromTerm.unapply(term)(using Type.of[a](using $stage2Quotes), $stage2Quotes)
              }
            }

          '{
            if ($stage2Args.length == ${ Expr(stage1Generic.fields.length) }) $tmp
            else None
          }
        }

        override def deriveCaseClass(generic: ProductGeneric.CaseClassGeneric[A]): Expr[FromExprT[A]] =
          '{
            new FromExprT[A] {

              override def unapply(x: Stage2.Expr[A])(using stage2Type: Type[A], stage2Quotes: Quotes): Option[A] = {
                val stage2Generic: ProductGeneric[A] = ProductGeneric.of[A]
                val stage2Value: Stage2.Term = x.toTerm.removeInlineAndTyped
                val stage2TypeRepr: TypeRepr = stage2Generic.typeRepr

                def parse(term: Term, args: Contiguous[Term]): Option[A] =
                  term match
                    case Select(sel, "apply") if sel.symbol.fullName == stage2TypeRepr.typeSymbol.fullName               => ${ unapplyImpl('args, 'stage2Quotes) }
                    case Select(New(tpt), "<init>") if tpt.tpe.typeSymbol.fullName == stage2TypeRepr.typeSymbol.fullName => ${ unapplyImpl('args, 'stage2Quotes) }
                    case _                                                                                               => None

                stage2Value match
                  case Apply(TypeApply(fun, _), args) => parse(fun, args.into[Contiguous])
                  case Apply(fun, args)               => parse(fun, args.into[Contiguous])
                  case _                              => None
              }

            }
          }

        override def deriveCaseObject(stage1Generic: ProductGeneric.CaseObjectGeneric[A]): Expr[FromExprT[A]] =
          '{
            new FromExprT[A] {

              override def unapply(x: Stage2.Expr[A])(using stage2Type: Type[A], stage2Quotes: Quotes): Option[A] = {
                val tmpGeneric: ProductGeneric[A] = ProductGeneric.of[A]
                val stage2Generic: ProductGeneric.CaseObjectGeneric[A] = tmpGeneric match
                  case stage2Generic: ProductGeneric.CaseObjectGeneric[A] => stage2Generic
                  case _                                                  => report.errorAndAbort("stages dont align?")
                val stage2Value: Stage2.Term = x.toTerm.removeInlineAndTyped

                if (stage2Value.symbol == stage2Generic.sym) ${ stage1Generic.instantiate.instance }.some
                else None
              }

            }
          }

      }
    }

  override protected def sumDeriver[A](using
      stage1Quotes: Quotes,
      fromExprT: Type[FromExprT],
      stage1Type: Type[A],
      stage1Generic: SumGeneric[A],
      der: Derivable[FromExprT],
  ): Derivable.SumDeriver[FromExprT, A] =
    Derivable.SumDeriver.withInstances[FromExprT, A] { instances =>
      new Derivable.SumDeriver[FromExprT, A] {

        private def unapplyImpl(
            stage2Value: Stage1.Expr[Term],
            stage2Quotes: Stage1.Expr[Stage2.Quotes],
        ): Stage1.Expr[Option[A]] = {
          val tmp: Seq[Stage1.Expr[Option[A]]] =
            stage1Generic.mapChildren
              .mapExpr[Option[A]] { [a <: A] => (_, _) ?=> (kase: stage1Generic.Case[a]) =>
                '{ ${ kase.getExpr(instances) }.fromTerm.unapply($stage2Value)(using Type.of[a](using $stage2Quotes), $stage2Quotes): Option[a] }
              }
              .to[Seq]

          tmp.reduceRight[Stage1.Expr[Option[A]]] { case (a, b) => '{ $a.orElse($b) } }
        }

        override def derive: Expr[FromExprT[A]] =
          '{
            new FromExprT[A] {

              override def unapply(x: Expr[A])(using stage2Type: Type[A], stage2Quotes: Quotes): Option[A] = {
                val stage2Value: Stage2.Term = x.toTerm.removeInlineAndTyped
                ${ unapplyImpl('stage2Value, 'stage2Quotes) }
              }

            }
          }

      }
    }

  override inline def derived[A]: FromExprT[A] = ${ derivedImpl[A] }

}
