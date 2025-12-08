package oxygen.quoted.companion

import oxygen.quoted.*
import scala.annotation.unused
import scala.quoted.*

final class TypeReprCompanion(using quotes: Quotes) {

  /** Returns the type or kind (TypeRepr) of T */
  def of[T <: AnyKind](using Type[T]): TypeRepr = TypeRepr.wrap(quotes.reflect.TypeRepr.of[T])

  /** Returns the type constructor of the runtime (erased) class */
  def typeConstructorOf(clazz: Class[?]): TypeRepr = TypeRepr.wrap(quotes.reflect.TypeRepr.typeConstructorOf(clazz))

  // =====| Added |=====

  def fromType(tpe: Type[?]): TypeRepr = {
    type T
    of[T](using tpe.asInstanceOf[Type[T]])
  }

  def emptyTuple: TypeRepr = TypeRepr.of[EmptyTuple]
  def unappliedTuplePrepend: TypeRepr = TypeRepr.of[? *: ?].narrow[AppliedType].tycon

  def tuplePreferTupleN(typeParams: List[TypeRepr]): TypeRepr = {
    val size = typeParams.size
    if size > 0 && size <= 22 then Symbol.tupleClass(size).typeRef.appliedTo(typeParams)
    else tupleUsingAppend(typeParams)
  }

  def tuplePreferTupleN(typeParam0: TypeRepr, typeParamN: TypeRepr*): TypeRepr =
    tuplePreferTupleN(typeParam0 :: typeParamN.toList)

  def tupleUsingAppend(typeParams: List[TypeRepr]): TypeRepr = {
    val pre = unappliedTuplePrepend
    typeParams.foldRight(emptyTuple) { pre.appliedTo(_, _) }
  }

  def tupleUsingAppend(typeParam0: TypeRepr, typeParamN: TypeRepr*): TypeRepr =
    tupleUsingAppend(typeParam0 :: typeParamN.toList)

  def extractTuple(typeRepr: TypeRepr): Option[List[TypeRepr]] = {
    val tmp: TypeRepr = typeRepr.dealias
    if tmp.isTupleN then Some(tmp.typeArgs)
    else
      typeRepr.asType match
        case '[h *: t]     => extractTuple(TypeRepr.of[t]).map { TypeRepr.of[h] :: _ }
        case '[EmptyTuple] => Some(Nil)
        case _             => None
  }

  /**
    * Useful for covariant types.
    *
    * ```scala
    * trait Dec[+V] { def decode(s: String): Either[String, V] }
    * def orElse[V](a: Dec[V], b: Dec[V]): Dec[V] = ???
    *
    * type A
    * type B <: A
    * type C <: A
    * val a: Dec[A] = ??? // can result in A, B, or C
    * val b: Dec[B] = ??? // can result in B
    * val c: Dec[C] = ??? // can result in C
    *
    * val ab: Dec[A] = orElse(a, b) // can result in A, B, or C
    * val ac: Dec[A] = orElse(a, c) // can result in A, B, or C
    * val bc: Dec[B | C] = orElse(b, c) // can result in B or C
    * ```
    */
  def covariantJoin(a: TypeRepr, b: TypeRepr): TypeRepr =
    if a <:< b then b
    else if b <:< a then a
    else OrType.companion.apply(a, b)
  def covariantJoin(all: Seq[TypeRepr]): TypeRepr =
    all.reduceLeft(covariantJoin(_, _))

  /**
    * Useful for contravariant types.
    *
    * ```scala
    * trait Enc[-V] { def encode(v: V): String }
    * def joinBoth[V](a: Enc[V], b: Enc[V]): Enc[V] = ab => s"${a.encode(ab)}${b.encode(ab)}"
    *
    * type A
    * type B <: A
    * type C <: A
    * val a: Enc[A] = ??? // can encode A, B, or C
    * val b: Enc[B] = ??? // can encode B
    * val c: Enc[C] = ??? // can encode C
    *
    * val ab: Enc[B] = joinBoth(a, b) // can encode B
    * val ac: Enc[C] = joinBoth(a, c) // can encode C
    * val bc: Enc[B & C] = joinBoth(b, c) // can encode only something that conforms to both B and C
    * ```
    */
  def contravariantJoin(a: TypeRepr, b: TypeRepr): TypeRepr =
    if a <:< b then a
    else if b <:< a then b
    else AndType.companion.apply(a, b)
  def contravariantJoin(all: Seq[TypeRepr]): TypeRepr =
    all.reduceLeft(contravariantJoin(_, _))

}

final class NamedTypeCompanion(using @unused quotes: Quotes) {}

final class TermRefCompanion(using quotes: Quotes) {

  def apply(qual: TypeRepr, name: String): TermRef =
    TermRef.wrap(quotes.reflect.TermRef.apply(qual.unwrapWithin, name))

}

final class TypeRefCompanion(using @unused quotes: Quotes) {}

final class ConstantTypeCompanion(using quotes: Quotes) {

  def apply(x: Constant): ConstantType =
    ConstantType.wrap(quotes.reflect.ConstantType.apply(x.unwrapWithin))

}

final class SuperTypeCompanion(using quotes: Quotes) {

  def apply(thistpe: TypeRepr, supertpe: TypeRepr): SuperType =
    SuperType.wrap(quotes.reflect.SuperType.apply(thistpe.unwrapWithin, supertpe.unwrapWithin))

}

final class RefinementCompanion(using quotes: Quotes) {

  def apply(parent: TypeRepr, name: String, info: TypeRepr): Refinement =
    Refinement.wrap(quotes.reflect.Refinement.apply(parent.unwrapWithin, name, info.unwrapWithin))

}

final class AppliedTypeCompanion(using quotes: Quotes) {

  /** Applied the type constructor `T` to a list of type arguments `T_1,..,T_n` to create `T[T_1,..,T_n]` */
  def apply(tycon: TypeRepr, args: List[TypeRepr]): AppliedType =
    AppliedType.wrap(quotes.reflect.AppliedType.apply(tycon.unwrapWithin, args.map(_.unwrapWithin)))

}

final class AnnotatedTypeCompanion(using quotes: Quotes) {

  def apply(underlying: TypeRepr, annot: Term): AnnotatedType =
    AnnotatedType.wrap(quotes.reflect.AnnotatedType.apply(underlying.unwrapWithin, annot.unwrapWithin))

}

final class AndOrTypeCompanion(using @unused quotes: Quotes) {}

final class AndTypeCompanion(using quotes: Quotes) {

  def apply(lhs: TypeRepr, rhs: TypeRepr): AndType =
    AndType.wrap(quotes.reflect.AndType.apply(lhs.unwrapWithin, rhs.unwrapWithin))

}

final class OrTypeCompanion(using quotes: Quotes) {

  def apply(lhs: TypeRepr, rhs: TypeRepr): OrType =
    OrType.wrap(quotes.reflect.OrType.apply(lhs.unwrapWithin, rhs.unwrapWithin))

}

final class MatchTypeCompanion(using quotes: Quotes) {

  def apply(bound: TypeRepr, scrutinee: TypeRepr, cases: List[TypeRepr]): MatchType =
    MatchType.wrap(quotes.reflect.MatchType.apply(bound.unwrapWithin, scrutinee.unwrapWithin, cases.map(_.unwrapWithin)))

}

final class ByNameTypeCompanion(using quotes: Quotes) {

  def apply(underlying: TypeRepr): TypeRepr =
    TypeRepr.wrap(quotes.reflect.ByNameType.apply(underlying.unwrapWithin))

}

final class ParamRefCompanion(using @unused quotes: Quotes) {}

final class ThisTypeCompanion(using @unused quotes: Quotes) {}

final class RecursiveThisCompanion(using @unused quotes: Quotes) {}

final class RecursiveTypeCompanion(using quotes: Quotes) {

  /**
    * Create a RecType, normalizing its contents. This means:
    *
    *   1. Nested Rec types on the type's spine are merged with the outer one.
    *   2. Any refinement of the form `type T = z.T` on the spine of the type
    *      where `z` refers to the created rec-type is replaced by
    *      `type T`. This avoids infinite recursions later when we
    *      try to follow these references.
    */
  def apply(parentExp: RecursiveType => TypeRepr): RecursiveType =
    RecursiveType.wrap(quotes.reflect.RecursiveType.apply(rt => parentExp(RecursiveType.wrap(rt)).unwrapWithin))

}

final class LambdaTypeCompanion(using @unused quotes: Quotes) {}

final class MethodOrPolyCompanion(using @unused quotes: Quotes) {}

final class MethodTypeCompanion(using quotes: Quotes) {

  def apply(paramNames: List[String])(paramInfosExp: MethodType => List[TypeRepr], resultTypeExp: MethodType => TypeRepr): MethodType =
    MethodType.wrap(
      quotes.reflect.MethodType.apply(
        paramNames,
      )(
        mt => paramInfosExp(MethodType.wrap(mt)).map(_.unwrapWithin),
        mt => resultTypeExp(MethodType.wrap(mt)).unwrapWithin,
      ),
    )

  def apply(kind: MethodTypeKind)(paramNames: List[String])(paramInfosExp: MethodType => List[TypeRepr], resultTypeExp: MethodType => TypeRepr): MethodType =
    MethodType.wrap(
      quotes.reflect.MethodType.apply(
        kind.unwrapWithin,
      )(paramNames)(
        mt => paramInfosExp(MethodType.wrap(mt)).map(_.unwrapWithin),
        mt => resultTypeExp(MethodType.wrap(mt)).unwrapWithin,
      ),
    )

}

final class PolyTypeCompanion(using quotes: Quotes) {

  def apply(paramNames: List[String])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => TypeRepr): PolyType =
    PolyType.wrap(
      quotes.reflect.PolyType.apply(
        paramNames,
      )(
        pt => paramBoundsExp(PolyType.wrap(pt)).map(_.unwrapWithin),
        pt => resultTypeExp(PolyType.wrap(pt)).unwrapWithin,
      ),
    )

}

final class TypeLambdaCompanion(using quotes: Quotes) {

  def apply(paramNames: List[String], boundsFn: TypeLambda => List[TypeBounds], bodyFn: TypeLambda => TypeRepr): TypeLambda =
    TypeLambda.wrap(
      quotes.reflect.TypeLambda.apply(
        paramNames,
        tl => boundsFn(TypeLambda.wrap(tl)).map(_.unwrapWithin),
        tl => bodyFn(TypeLambda.wrap(tl)).unwrapWithin,
      ),
    )

}

final class MatchCaseCompanion(using quotes: Quotes) {

  def apply(pattern: TypeRepr, rhs: TypeRepr): MatchCase =
    MatchCase.wrap(quotes.reflect.MatchCase.apply(pattern.unwrapWithin, rhs.unwrapWithin))

}

final class TypeBoundsCompanion(using quotes: Quotes) {

  def apply(low: TypeRepr, hi: TypeRepr): TypeBounds =
    TypeBounds.wrap(quotes.reflect.TypeBounds.apply(low.unwrapWithin, hi.unwrapWithin))

  def empty: TypeBounds =
    TypeBounds.wrap(quotes.reflect.TypeBounds.empty)

  def upper(hi: TypeRepr): TypeBounds =
    TypeBounds.wrap(quotes.reflect.TypeBounds.upper(hi.unwrapWithin))

  def lower(lo: TypeRepr): TypeBounds =
    TypeBounds.wrap(quotes.reflect.TypeBounds.lower(lo.unwrapWithin))

}

final class NoPrefixCompanion(using @unused quotes: Quotes) {}

final class FlexibleTypeCompanion(using quotes: Quotes) {

  def apply(tp: TypeRepr): FlexibleType =
    FlexibleType.wrap(quotes.reflect.FlexibleType.apply(tp.unwrapWithin))

}
