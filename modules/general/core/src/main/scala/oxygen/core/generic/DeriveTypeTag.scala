package oxygen.core.generic

import oxygen.core.TypeTag
import oxygen.predef.base.*
import scala.annotation.tailrec
import scala.quoted.*
import scala.reflect.ClassTag

final class DeriveTypeTag(val quotes: Quotes) {
  private given Quotes = quotes
  import quotes.reflect as R

  def summonOrDerive[A <: AnyKind: {Type as aTpe}]: Expr[TypeTag[A]] = {
    type B
    given Type[B] = aTpe.asInstanceOf[Type[B]]

    val res: Expr[TypeTag[A]] =
      Expr.summon[TypeTag[A]].getOrElse {
        val refExpr: Expr[TypeTag.TypeRef] = deriveRef(R.TypeRepr.of[A])
        val classExpr: Expr[Class[?]] =
          Expr.summon[ClassTag[B]] match {
            case Some(classTag) => '{ $classTag.runtimeClass }
            case None           => '{ classOf[Any] }
          }

        '{ TypeTag[A, TypeTag.TypeRef]($refExpr, $classExpr) }
      }

    // R.report.info(res.show)

    res
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def notSupported(function: String, repr: R.TypeRepr): Nothing =
    R.report.errorAndAbort(
      s"""Unable to `$function` for repr:
         |
         |toString: $repr
         |    show: ${repr.show}
         |
         |toIndentedString:
         |${IndentedString.fromAny(repr)}
         |""".stripMargin,
    )

  private def summonOrDeriveRef(repr: R.TypeRepr): Expr[TypeTag.TypeRef] = {
    type _T
    @scala.annotation.unused
    given Type[_T] = repr.asType.asInstanceOf[Type[_T]]

    Expr.summon[TypeTag[_T]] match {
      case Some(typeTag) => '{ $typeTag.tag }
      case None          => deriveRef(repr)
    }
  }

  private def deriveRef(typeRepr: R.TypeRepr): Expr[TypeTag.TypeRef] =
    typeRepr.dealias.removeUnnecessary match {
      case R.TypeRef(R.NoPrefix(), _) if typeRepr.typeSymbol.flags.is(R.Flags.Param) =>
        R.report.errorAndAbort(s"Missing implicit TypeTag for ${typeRepr.show}")
      case R.AppliedType(fTypeRepr @ typeOrTerm(R.NoPrefix(), _), args) if typeRepr.typeSymbol.flags.is(R.Flags.Param) =>
        type F
        type FA
        given Type[F] = fTypeRepr.asType.asInstanceOf[Type[F]]
        given Type[FA] = typeRepr.asType.asInstanceOf[Type[FA]]
        Expr.summon[TypeTag[F]] match {
          case Some(fTypeTag) =>
            '{
              val hktTypeTag: TypeTag[FA] = $fTypeTag.asInstanceOf[TypeTag[FA]]
              val hktArgs: List[TypeTag.TypeRef] = ${ Expr.ofList(args.map(summonOrDeriveRef)) }
              hktTypeTag.applyHKTArgs(hktArgs)
            }
          case None =>
            R.report.errorAndAbort(s"Missing implicit TypeTag for ${fTypeRepr.show}")
        }
      case typeOrTerm(prefix, name)        => deriveSingle(prefix, name, Nil)
      case appliedType(prefix, name, args) => deriveSingle(prefix, name, args.map(summonOrDeriveRef))
      case andType(types)                  =>
        '{
          TypeTag.TypeRef.Intersection(Set(${ Expr.ofSeq(types.toList.map(summonOrDeriveRef)) }*))
        }
      case orType(types) =>
        '{
          TypeTag.TypeRef.Union(Set(${ Expr.ofSeq(types.toList.map(summonOrDeriveRef)) }*))
        }
      case R.TypeBounds(lo, hi) if lo =:= R.TypeRepr.of[Nothing] && hi =:= R.TypeRepr.of[AnyKind] =>
        '{ TypeTag.TypeRef.Wildcard }
      case lambda: R.LambdaType =>
        resolveLambda(lambda.resType, lambda, Set.empty)

      case typeRepr: R.NoPrefix      => notSupported("deriveRef", typeRepr)
      case typeRepr: R.MethodType    => notSupported("deriveRef", typeRepr)
      case typeRepr: R.PolyType      => notSupported("deriveRef", typeRepr)
      case typeRepr: R.ConstantType  => notSupported("deriveRef", typeRepr)
      case typeRepr: R.SuperType     => notSupported("deriveRef", typeRepr)
      case typeRepr: R.Refinement    => notSupported("deriveRef", typeRepr)
      case typeRepr: R.MatchType     => notSupported("deriveRef", typeRepr)
      case typeRepr: R.ByNameType    => notSupported("deriveRef", typeRepr)
      case typeRepr: R.ParamRef      => notSupported("deriveRef", typeRepr)
      case typeRepr: R.RecursiveThis => notSupported("deriveRef", typeRepr)
      case typeRepr: R.RecursiveType => notSupported("deriveRef", typeRepr)
      case typeRepr: R.MatchCase     => notSupported("deriveRef", typeRepr)
      case _                         => notSupported("deriveRef", typeRepr)
    }

  private object resolveLambda {

    type ResolveFunction = Expr[List[TypeTag.TypeRef]] => Expr[TypeTag.TypeRef]
    type Const = Expr[TypeTag.TypeRef]
    type EitherResolveConst = Either[ResolveFunction, Const]

    private def attemptConst(in: List[EitherResolveConst])(
        make: List[Const] => Const,
    ): EitherResolveConst = {
      @tailrec
      def loop(
          queue: List[EitherResolveConst],
          rConst: Option[List[Const]],
          rResolve: List[ResolveFunction],
      ): EitherResolveConst =
        queue match {
          case qHead :: qTail =>
            (qHead, rConst) match {
              case (Right(qHead), Some(rConst)) => loop(qTail, (qHead :: rConst).some, (_ => qHead) :: rResolve)
              case (Right(qHead), None)         => loop(qTail, None, (_ => qHead) :: rResolve)
              case (Left(qHead), _)             => loop(qTail, None, qHead :: rResolve)
            }
          case Nil =>
            rConst match {
              case Some(rConst) => make(rConst.reverse).asRight
              case None         => { (inputs: Expr[List[TypeTag.TypeRef]]) => make(rResolve.reverse.map(_(inputs))) }.asLeft
            }
        }

      loop(in, Nil.some, Nil)
    }

    private def summonOrDeriveRefBind(repr: R.TypeRepr, binds: Set[R.LambdaType]): EitherResolveConst = {
      type _T
      @scala.annotation.unused
      given Type[_T] = repr.asType.asInstanceOf[Type[_T]]

      Expr.summon[TypeTag[_T]] match {
        case Some(typeTag) => '{ $typeTag.tag }.asRight
        case None          => deriveRefBind(repr, binds)
      }
    }

    private def deriveRefBind(typeRepr: R.TypeRepr, binds: Set[R.LambdaType]): EitherResolveConst =
      typeRepr.dealias.removeUnnecessary match {
        case R.ParamRef(bindRef: R.LambdaType, idx) if binds.contains(bindRef) =>
          { (inputs: Expr[List[TypeTag.TypeRef]]) => '{ $inputs(${ Expr(idx) }) } }.asLeft
        case R.TypeRef(R.NoPrefix(), _) if typeRepr.typeSymbol.flags.is(R.Flags.Param) =>
          R.report.errorAndAbort(s"Missing implicit TypeTag for ${typeRepr.show}")
        case appliedType(R.NoPrefix(), _, _) if typeRepr.typeSymbol.flags.is(R.Flags.Param) =>
          R.report.errorAndAbort(s"Missing implicit TypeTag for HKT ${typeRepr.show}")
        case typeOrTerm(prefix, name)        => deriveSingle(prefix, name, Nil).asRight
        case appliedType(prefix, name, args) =>
          attemptConst(args.map(summonOrDeriveRefBind(_, binds)))(
            deriveSingle(prefix, name, _),
          )

        case andType(types) =>
          attemptConst(types.toList.map(summonOrDeriveRefBind(_, binds))) { children =>
            '{ TypeTag.TypeRef.Intersection(Set(${ Expr.ofSeq(children) }*)) }
          }
        case orType(types) =>
          attemptConst(types.toList.map(summonOrDeriveRefBind(_, binds))) { children =>
            '{ TypeTag.TypeRef.Union(Set(${ Expr.ofSeq(children) }*)) }
          }
        case R.TypeBounds(lo, hi) if lo =:= R.TypeRepr.of[Nothing] && hi =:= R.TypeRepr.of[AnyKind] =>
          '{ TypeTag.TypeRef.Wildcard }.asRight
        case lambda: R.LambdaType =>
          resolveLambda(lambda.resType, lambda, binds).asRight

        case R.ParamRef(_: R.LambdaType, _) =>
          R.report.errorAndAbort(s"found ParamRef which is missing a bind: ${typeRepr.show}")
        case _: R.ParamRef =>
          R.report.errorAndAbort(s"found ParamRef which does not reference a type lambda: ${typeRepr.show}")

        case typeRepr: R.NoPrefix      => notSupported("deriveRef", typeRepr)
        case typeRepr: R.MethodType    => notSupported("deriveRef", typeRepr)
        case typeRepr: R.PolyType      => notSupported("deriveRef", typeRepr)
        case typeRepr: R.ConstantType  => notSupported("deriveRef", typeRepr)
        case typeRepr: R.SuperType     => notSupported("deriveRef", typeRepr)
        case typeRepr: R.Refinement    => notSupported("deriveRef", typeRepr)
        case typeRepr: R.MatchType     => notSupported("deriveRef", typeRepr)
        case typeRepr: R.ByNameType    => notSupported("deriveRef", typeRepr)
        case typeRepr: R.RecursiveThis => notSupported("deriveRef", typeRepr)
        case typeRepr: R.RecursiveType => notSupported("deriveRef", typeRepr)
        case typeRepr: R.MatchCase     => notSupported("deriveRef", typeRepr)
        case _                         => notSupported("deriveRef", typeRepr)
      }

    def apply(repr: R.TypeRepr, bind: R.LambdaType, otherBinds: Set[R.LambdaType]): Expr[TypeTag.TypeRef] =
      summonOrDeriveRefBind(repr, otherBinds + bind) match {
        case Right(const)  => const
        case Left(resolve) => '{ TypeTag.TypeRef.HKT(${ Expr(bind.paramNames) }, args => ${ resolve('args) }) }
      }

  }

  private def deriveSingle(prefix: R.TypeRepr, name: String, args: List[Expr[TypeTag.TypeRef]]): Expr[TypeTag.TypeRef.Single] =
    '{ TypeTag.TypeRef.Single(${ Expr(name) }, ${ Expr.ofList(args) }, ${ deriveSinglePrefix(prefix) }) }

  private def deriveSinglePrefix(prefix: R.TypeRepr): Expr[Either[TypeTag.TypeRef.Single, List[String]]] =
    prefix.removeUnnecessary match {
      case R.NoPrefix()                        => '{ Nil.asRight }
      case _ if prefix.typeSymbol.isPackageDef => '{ ${ Expr(prefix.show.split('.').filter(_.nonEmpty).toList) }.asRight }
      case typeOrTerm(prefix, name)            => '{ ${ deriveSingle(prefix, name, Nil) }.asLeft }
      case appliedType(prefix, name, args)     => '{ ${ deriveSingle(prefix, name, args.map(summonOrDeriveRef)) }.asLeft }
      case _                                   => notSupported("deriveSinglePrefix", prefix)
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Matching
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension (repr: R.TypeRepr)
    private def removeUnnecessary: R.TypeRepr =
      repr match {
        case R.ThisType(underlying)         => underlying.removeUnnecessary
        case R.AnnotatedType(underlying, _) => underlying.removeUnnecessary
        case _                              => repr
      }

  private object typeOrTerm {

    def unapply(repr: R.TypeRepr): Option[(R.TypeRepr, String)] =
      repr match {
        case R.TypeRef(prefix, name) => (prefix, name.stripSuffix("$")).some
        case R.TermRef(prefix, name) => (prefix, name.stripSuffix("$")).some
        case _                       => None
      }

  }

  private object appliedType {

    def unapply(repr: R.TypeRepr): Option[(R.TypeRepr, String, List[R.TypeRepr])] =
      repr match {
        case R.AppliedType(typeOrTerm(prefix, name), args) => (prefix, name.stripSuffix("$"), args).some
        case _                                             => None
      }

  }

  private object andType {

    def unapply(typeRepr: R.TypeRepr): Option[Set[R.TypeRepr]] =
      typeRepr match {
        case R.AndType(a, b) => (unapply(a).getOrElse(Set(a)) ++ unapply(b).getOrElse(Set(b))).some
        case _               => None
      }

  }

  private object orType {

    def unapply(typeRepr: R.TypeRepr): Option[Set[R.TypeRepr]] =
      typeRepr match {
        case R.OrType(a, b) => (unapply(a).getOrElse(Set(a)) ++ unapply(b).getOrElse(Set(b))).some
        case _              => None
      }

  }

}
