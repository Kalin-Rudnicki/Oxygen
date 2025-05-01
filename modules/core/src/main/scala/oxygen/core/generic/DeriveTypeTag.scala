package oxygen.core.generic

import oxygen.core.TypeTag
import oxygen.predef.core.*
import scala.quoted.*
import scala.reflect.ClassTag

final class DeriveTypeTag(val quotes: Quotes) {
  private given Quotes = quotes
  import quotes.reflect as R

  def summonOrDerive[A: Type]: Expr[TypeTag[A]] = {
    val res =
      Expr.summon[TypeTag[A]].getOrElse {
        val refExpr: Expr[TypeTag.TypeRef] = deriveRef(R.TypeRepr.of[A])
        val classExpr: Expr[Class[?]] =
          Expr.summon[ClassTag[A]] match {
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
    R.report.errorAndAbort(s"Unable to `$function` for repr $repr")

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
    typeRepr.removeUnnecessary match {
      case R.TypeRef(R.NoPrefix(), name) if typeRepr.typeSymbol.flags.is(R.Flags.Param) =>
        R.report.errorAndAbort(s"Missing implicit typeTag for $name")
      case typeOrTerm(prefix, name)        => deriveSingle(prefix, name, Nil)
      case appliedType(prefix, name, args) => deriveSingle(prefix, name, args.map(summonOrDeriveRef))
      case andType(types) =>
        '{
          TypeTag.TypeRef.Intersection(Set(${ Expr.ofSeq(types.toList.map(summonOrDeriveRef)) }*))
        }
      case orType(types) =>
        '{
          TypeTag.TypeRef.Union(Set(${ Expr.ofSeq(types.toList.map(summonOrDeriveRef)) }*))
        }
      case typeRepr: R.NoPrefix      => notSupported("deriveRef", typeRepr)
      case typeRepr: R.MethodType    => notSupported("deriveRef", typeRepr)
      case typeRepr: R.PolyType      => notSupported("deriveRef", typeRepr)
      case typeRepr: R.TypeLambda    => notSupported("deriveRef", typeRepr)
      case typeRepr: R.ConstantType  => notSupported("deriveRef", typeRepr)
      case typeRepr: R.SuperType     => notSupported("deriveRef", typeRepr)
      case typeRepr: R.Refinement    => notSupported("deriveRef", typeRepr)
      case typeRepr: R.MatchType     => notSupported("deriveRef", typeRepr)
      case typeRepr: R.ByNameType    => notSupported("deriveRef", typeRepr)
      case typeRepr: R.ParamRef      => notSupported("deriveRef", typeRepr)
      case typeRepr: R.RecursiveThis => notSupported("deriveRef", typeRepr)
      case typeRepr: R.RecursiveType => notSupported("deriveRef", typeRepr)
      case typeRepr: R.MatchCase     => notSupported("deriveRef", typeRepr)
      case typeRepr: R.TypeBounds    => notSupported("deriveRef", typeRepr)
      case _                         => notSupported("deriveRef", typeRepr)
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

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private given singleTypeRefToExpr: ToExpr[TypeTag.TypeRef.Single] =
    new ToExpr[TypeTag.TypeRef.Single] {
      override def apply(x: TypeTag.TypeRef.Single)(using Quotes): Expr[TypeTag.TypeRef.Single] =
        x match {
          case TypeTag.TypeRef.Single(typeName, typeArgs, Left(prefix)) =>
            '{ TypeTag.TypeRef.Single(${ Expr(typeName) }, ${ Expr(typeArgs) }, Left(${ apply(prefix) })) }
          case TypeTag.TypeRef.Single(typeName, typeArgs, Right(prefix)) =>
            '{ TypeTag.TypeRef.Single(${ Expr(typeName) }, ${ Expr(typeArgs) }, Right(${ Expr(prefix) })) }
        }
    }

  private given typeRefToExpr: ToExpr[TypeTag.TypeRef] =
    new ToExpr[TypeTag.TypeRef] {
      override def apply(x: TypeTag.TypeRef)(using Quotes): Expr[TypeTag.TypeRef] =
        x match {
          case single: TypeTag.TypeRef.Single      => singleTypeRefToExpr.apply(single)
          case TypeTag.TypeRef.Union(cases)        => '{ TypeTag.TypeRef.Union(${ Expr(cases) }) }
          case TypeTag.TypeRef.Intersection(cases) => '{ TypeTag.TypeRef.Intersection(${ Expr(cases) }) }
          case TypeTag.TypeRef.Wildcard            => '{ TypeTag.TypeRef.Wildcard }
        }
    }

}
