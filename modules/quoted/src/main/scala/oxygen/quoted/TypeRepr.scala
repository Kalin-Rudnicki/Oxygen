package oxygen.quoted

import oxygen.quoted.companion.*
import oxygen.quoted.error.UnknownCase
import scala.annotation.experimental
import scala.quoted.*

sealed trait TypeRepr extends Model {
  type This <: TypeRepr
  val quotes: Quotes
  val unwrap: quotes.reflect.TypeRepr
  def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.TypeRepr = unwrap.asInstanceOf[newQuotes.reflect.TypeRepr]
  given givenQuotes: quotes.type = quotes

  // =====| Direct from scala std-lib |=====

  /** Shows the type as a String */
  final def show(using printer: Printer[TypeRepr]): String = printer.show(this)

  final def show(f: PrinterCompanion => Printer[TypeRepr]): String =
    show(using f(Printer.companion))

  /**
    * Convert this `TypeRepr` to an `Type[?]`
    *
    *  Usage:
    *  ```scala
    *  //{
    *  import scala.quoted.*
    *  def f(using Quotes) = {
    *    val q: Quotes = summon[Quotes]
    *    import q.reflect.*
    *    val typeRepr: TypeRepr = ???
    *  //}
    *    typeRepr.asType match
    *      case '[t] =>
    *        '{ val x: t = ??? }
    *  //{
    *  }
    *  //}
    *  ```
    */
  final def asType: Type[?] = this.unwrap.asType

  /**
    * Is `self` type the same as `that` type?
    *  This is the case iff `self <:< that` and `that <:< self`.
    */
  final def =:=(that: TypeRepr): Boolean = this.unwrap =:= that.unwrapWithin

  /** Is this type a subtype of that type? */
  final def <:<(that: TypeRepr): Boolean = this.unwrap <:< that.unwrapWithin

  /**
    * Widen from singleton type to its underlying non-singleton
    *  base type by applying one or more `underlying` dereferences,
    *  Also go from => T to T.
    *  Identity for all other types. Example:
    *
    *  class Outer { class C ; val x: C }
    *  def o: Outer
    *  <o.x.type>.widen = o.C
    */
  final def widen: TypeRepr = TypeRepr.wrap(this.unwrap.widen)

  /**
    * Widen from TermRef to its underlying non-termref
    *  base type, while also skipping ByName types.
    */
  final def widenTermRefByName: TypeRepr = TypeRepr.wrap(this.unwrap.widenTermRefByName)

  /** Widen from ByName type to its result type. */
  final def widenByName: TypeRepr = TypeRepr.wrap(this.unwrap.widenByName)

  /** Follow aliases, annotated types until type is no longer alias type, annotated type. */
  final def dealias: TypeRepr = TypeRepr.wrap(this.unwrap.dealias)

  /** Follow non-opaque aliases, annotated types until type is no longer alias type, annotated type. */
  final def dealiasKeepOpaques: TypeRepr = TypeRepr.wrap(this.unwrap.dealiasKeepOpaques)

  /**
    * A simplified version of this type which is equivalent wrt =:= to this type.
    *  Reduces typerefs, applied match types, and and or types.
    */
  final def simplified: TypeRepr = TypeRepr.wrap(this.unwrap.simplified)

  final def classSymbol: Option[Symbol] = this.unwrap.classSymbol.map(Symbol.wrap(_))
  final def typeSymbol: Symbol = Symbol.wrap(this.unwrap.typeSymbol)
  final def termSymbol: Symbol = Symbol.wrap(this.unwrap.termSymbol)
  final def isSingleton: Boolean = this.unwrap.isSingleton

  /**
    * The type of `member` as seen from prefix `self`.
    *
    *  Also see `typeRef` and `termRef`
    */
  final def memberType(member: Symbol): TypeRepr = TypeRepr.wrap(this.unwrap.memberType(member.unwrapWithin))

  /** The base classes of this type with the class itself as first element. */
  final def baseClasses: List[Symbol] = this.unwrap.baseClasses.map(Symbol.wrap(_))

  /**
    * The least type instance of given class which is a super-type
    *  of this type.  Example:
    *  {{{
    *    class D[T]
    *    class C extends p.D[Int]
    *    ThisType(C).baseType(D) = p.D[Int]
    * }}}
    */
  final def baseType(cls: Symbol): TypeRepr = TypeRepr.wrap(this.unwrap.baseType(cls.unwrapWithin))

  /** Is this type an instance of a non-bottom subclass of the given class `cls`? */
  final def derivesFrom(cls: Symbol): Boolean = this.unwrap.derivesFrom(cls.unwrapWithin)

  /**
    * Is this type a function type?
    *
    *  @return true if the dealiased type of `self` without refinement is `FunctionN[T1, T2, ..., Tn]`
    *
    *  @note The function
    *
    *     - returns true for `given Int => Int` and `erased Int => Int`
    *     - returns false for `List[Int]`, despite that `List[Int] <:< Int => Int`.
    */
  final def isFunctionType: Boolean = this.unwrap.isFunctionType

  /**
    * Is this type an context function type?
    *
    *  @see `isFunctionType`
    */
  final def isContextFunctionType: Boolean = this.unwrap.isContextFunctionType

  /**
    * Is this type a function type with erased parameters?
    *
    *  @see `isFunctionType`
    */
  final def isErasedFunctionType: Boolean = this.unwrap.isErasedFunctionType

  /**
    * Is this type a dependent function type?
    *
    *  @see `isFunctionType`
    */
  final def isDependentFunctionType: Boolean = this.unwrap.isDependentFunctionType

  /**
    * Is this type a `TupleN` type?
    *
    * @return true if the dealiased type of `self` is `TupleN[T1, T2, ..., Tn]`
    */
  final def isTupleN: Boolean = this.unwrap.isTupleN

  /** The type <this . sym>, reduced if possible */
  final def select(sym: Symbol): TypeRepr = TypeRepr.wrap(this.unwrap.select(sym.unwrapWithin))

  /** The current type applied to given type arguments: `this[targ]` */
  final def appliedTo(targ: TypeRepr): TypeRepr = TypeRepr.wrap(this.unwrap.appliedTo(targ.unwrapWithin))

  /** The current type applied to given type arguments: `this[targ0, ..., targN]` */
  final def appliedTo(targs: List[TypeRepr]): TypeRepr = TypeRepr.wrap(this.unwrap.appliedTo(targs.map(_.unwrapWithin)))

  /**
    * Substitute all types that refer in their symbol attribute to
    *  one of the symbols in `from` by the corresponding types in `to`.
    */
  final def substituteTypes(from: List[Symbol], to: List[TypeRepr]): TypeRepr = TypeRepr.wrap(this.unwrap.substituteTypes(from.map(_.unwrapWithin), to.map(_.unwrapWithin)))

  /** The applied type arguments (empty if there is no such arguments) */
  final def typeArgs: List[TypeRepr] = this.unwrap.typeArgs.map(TypeRepr.wrap(_))

  // =====| Added |=====

  final def andChildren: Set[TypeRepr] = this match
    case and: AndType => and.left.andChildren ++ and.right.andChildren
    case _            => Set(this)

  final def orChildren: Set[TypeRepr] = this match
    case or: OrType => or.left.orChildren ++ or.right.orChildren
    case _          => Set(this)

  final def asTypeOf[A]: Type[A] =
    this.asType.asInstanceOf[Type[A]]

  final def typeOrTermSymbol: Symbol = if (this.isSingleton) this.termSymbol else this.typeSymbol

  final def typeType: Option[TypeType] = this.typeOrTermSymbol.typeType
  final def typeTypeSealed: Option[TypeType.Sealed] = this.typeOrTermSymbol.typeTypeSealed
  final def typeTypeCase: Option[TypeType.Case] = this.typeOrTermSymbol.typeTypeCase
  final def typeTypeCaseClass: Option[TypeType.Case.Class] = this.typeOrTermSymbol.typeTypeCaseClass
  final def typeTypeCaseObject: Option[TypeType.Case.Object] = this.typeOrTermSymbol.typeTypeCaseObject

  final def annotations: Annotations = new Annotations(this.typeOrTermSymbol.annotations.all, show)

  final def typeTree: TypeTree =
    TypeTree.fromType(this.asType)

  override final def maybePos: Option[Position] = this.typeOrTermSymbol.pos

  final def appliedTo(targ0: TypeRepr, targ1: TypeRepr, targN: TypeRepr*): TypeRepr =
    this.appliedTo(targ0 :: targ1 :: targN.toList)

  final def showWith(f: PrinterCompanion => Printer[TypeRepr]): String = this.show(using f(Printer.companion(using quotes)))
  final def showCode: String = showWith(_.TypeReprCode)
  final def showShortCode: String = showWith(_.TypeReprShortCode)
  final def showAnsiCode: String = showWith(_.TypeReprAnsiCode)
  final def showStructure: String = showWith(_.TypeReprStructure)

}
object TypeRepr {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.TypeRepr): TypeRepr = unwrap match
    case unwrap: quotes.reflect.NamedType     => NamedType.wrap(unwrap)
    case unwrap: quotes.reflect.ConstantType  => ConstantType.wrap(unwrap)
    case unwrap: quotes.reflect.SuperType     => SuperType.wrap(unwrap)
    case unwrap: quotes.reflect.Refinement    => Refinement.wrap(unwrap)
    case unwrap: quotes.reflect.AppliedType   => AppliedType.wrap(unwrap)
    case unwrap: quotes.reflect.AnnotatedType => AnnotatedType.wrap(unwrap)
    case unwrap: quotes.reflect.AndOrType     => AndOrType.wrap(unwrap)
    case unwrap: quotes.reflect.AndType       => AndType.wrap(unwrap)
    case unwrap: quotes.reflect.OrType        => OrType.wrap(unwrap)
    case unwrap: quotes.reflect.MatchType     => MatchType.wrap(unwrap)
    case unwrap: quotes.reflect.ByNameType    => ByNameType.wrap(unwrap)
    case unwrap: quotes.reflect.ParamRef      => ParamRef.wrap(unwrap)
    case unwrap: quotes.reflect.ThisType      => ThisType.wrap(unwrap)
    case unwrap: quotes.reflect.RecursiveThis => RecursiveThis.wrap(unwrap)
    case unwrap: quotes.reflect.RecursiveType => RecursiveType.wrap(unwrap)
    case unwrap: quotes.reflect.LambdaType    => LambdaType.wrap(unwrap)
    case unwrap: quotes.reflect.MethodOrPoly  => MethodOrPoly.wrap(unwrap)
    case unwrap: quotes.reflect.MethodType    => MethodType.wrap(unwrap)
    case unwrap: quotes.reflect.PolyType      => PolyType.wrap(unwrap)
    case unwrap: quotes.reflect.TypeLambda    => TypeLambda.wrap(unwrap)
    case unwrap: quotes.reflect.MatchCase     => MatchCase.wrap(unwrap)
    case unwrap: quotes.reflect.TypeBounds    => TypeBounds.wrap(unwrap)
    case unwrap: quotes.reflect.NoPrefix      => NoPrefix.wrap(unwrap)
    case unwrap: quotes.reflect.FlexibleType  => FlexibleType.wrap(unwrap)
    case _                                    => throw UnknownCase("TypeRepr", unwrap)

  def companion(using quotes: Quotes): TypeReprCompanion = TypeReprCompanion(using quotes)
  given Quotes => Conversion[TypeRepr.type, TypeReprCompanion] = _.companion

}

sealed trait NamedType extends TypeRepr {
  override type This <: NamedType
  override val unwrap: quotes.reflect.NamedType
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.NamedType = unwrap.asInstanceOf[newQuotes.reflect.NamedType]

  final def qualifier: TypeRepr = TypeRepr.wrap(this.unwrap.qualifier)

  final def name: String = this.unwrap.name

}
object NamedType {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.NamedType): NamedType = unwrap match
    case unwrap: quotes.reflect.TermRef => TermRef.wrap(unwrap)
    case unwrap: quotes.reflect.TypeRef => TypeRef.wrap(unwrap)
    case _                              => throw UnknownCase("NamedType", unwrap)

  def companion(using quotes: Quotes): NamedTypeCompanion = NamedTypeCompanion(using quotes)
  given Quotes => Conversion[NamedType.type, NamedTypeCompanion] = _.companion

}

final class TermRef(val quotes: Quotes)(val unwrap: quotes.reflect.TermRef) extends NamedType {
  override type This <: TermRef
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.TermRef = unwrap.asInstanceOf[newQuotes.reflect.TermRef]

  // =====| Added |=====

  def toTerm: Term = Ref.term(this)

}
object TermRef {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.TermRef): TermRef =
    new TermRef(quotes)(unwrap)

  def companion(using quotes: Quotes): TermRefCompanion = TermRefCompanion(using quotes)
  given Quotes => Conversion[TermRef.type, TermRefCompanion] = _.companion

  def unapply(x: TermRef): (TypeRepr, String) = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.TermRef.unapply(x.unwrapWithin)
    (TypeRepr.wrap(unwrap._1), unwrap._2)
  }

}

final class TypeRef(val quotes: Quotes)(val unwrap: quotes.reflect.TypeRef) extends NamedType {
  override type This <: TypeRef
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.TypeRef = unwrap.asInstanceOf[newQuotes.reflect.TypeRef]

  def isOpaqueAlias: Boolean = this.unwrap.isOpaqueAlias

  def translucentSuperType: TypeRepr = TypeRepr.wrap(this.unwrap.translucentSuperType)

}
object TypeRef {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.TypeRef): TypeRef =
    new TypeRef(quotes)(unwrap)

  def companion(using quotes: Quotes): TypeRefCompanion = TypeRefCompanion(using quotes)
  given Quotes => Conversion[TypeRef.type, TypeRefCompanion] = _.companion

  def unapply(x: TypeRef): (TypeRepr, String) = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.TypeRef.unapply(x.unwrapWithin)
    (TypeRepr.wrap(unwrap._1), unwrap._2)
  }

}

final class ConstantType(val quotes: Quotes)(val unwrap: quotes.reflect.ConstantType) extends TypeRepr {
  override type This <: ConstantType
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.ConstantType = unwrap.asInstanceOf[newQuotes.reflect.ConstantType]

  def constant: Constant = Constant.wrap(this.unwrap.constant)

}
object ConstantType {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.ConstantType): ConstantType =
    new ConstantType(quotes)(unwrap)

  def companion(using quotes: Quotes): ConstantTypeCompanion = ConstantTypeCompanion(using quotes)
  given Quotes => Conversion[ConstantType.type, ConstantTypeCompanion] = _.companion

  def unapply(x: ConstantType): Some[Constant] = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.ConstantType.unapply(x.unwrapWithin)
    Some(Constant.wrap(unwrap.value))
  }

}

final class SuperType(val quotes: Quotes)(val unwrap: quotes.reflect.SuperType) extends TypeRepr {
  override type This <: SuperType
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.SuperType = unwrap.asInstanceOf[newQuotes.reflect.SuperType]

  def thistpe: TypeRepr = TypeRepr.wrap(this.unwrap.thistpe)

  def supertpe: TypeRepr = TypeRepr.wrap(this.unwrap.supertpe)

}
object SuperType {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.SuperType): SuperType =
    new SuperType(quotes)(unwrap)

  def companion(using quotes: Quotes): SuperTypeCompanion = SuperTypeCompanion(using quotes)
  given Quotes => Conversion[SuperType.type, SuperTypeCompanion] = _.companion

  def unapply(x: SuperType): (TypeRepr, TypeRepr) = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.SuperType.unapply(x.unwrapWithin)
    (TypeRepr.wrap(unwrap._1), TypeRepr.wrap(unwrap._2))
  }

}

final class Refinement(val quotes: Quotes)(val unwrap: quotes.reflect.Refinement) extends TypeRepr {
  override type This <: Refinement
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Refinement = unwrap.asInstanceOf[newQuotes.reflect.Refinement]

  def parent: TypeRepr = TypeRepr.wrap(this.unwrap.parent)

  def name: String = this.unwrap.name

  def info: TypeRepr = TypeRepr.wrap(this.unwrap.info)

}
object Refinement {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Refinement): Refinement =
    new Refinement(quotes)(unwrap)

  def companion(using quotes: Quotes): RefinementCompanion = RefinementCompanion(using quotes)
  given Quotes => Conversion[Refinement.type, RefinementCompanion] = _.companion

  def unapply(x: Refinement): (TypeRepr, String, TypeRepr) = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.Refinement.unapply(x.unwrapWithin)
    (TypeRepr.wrap(unwrap._1), unwrap._2, TypeRepr.wrap(unwrap._3))
  }

}

final class AppliedType(val quotes: Quotes)(val unwrap: quotes.reflect.AppliedType) extends TypeRepr {
  override type This <: AppliedType
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.AppliedType = unwrap.asInstanceOf[newQuotes.reflect.AppliedType]

  def tycon: TypeRepr = TypeRepr.wrap(this.unwrap.tycon)

  def args: List[TypeRepr] = this.unwrap.args.map(TypeRepr.wrap(_))

}
object AppliedType {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.AppliedType): AppliedType =
    new AppliedType(quotes)(unwrap)

  def companion(using quotes: Quotes): AppliedTypeCompanion = AppliedTypeCompanion(using quotes)
  given Quotes => Conversion[AppliedType.type, AppliedTypeCompanion] = _.companion

  def unapply(x: AppliedType): (TypeRepr, List[TypeRepr]) = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.AppliedType.unapply(x.unwrapWithin)
    (TypeRepr.wrap(unwrap._1), unwrap._2.map(TypeRepr.wrap(_)))
  }

}

final class AnnotatedType(val quotes: Quotes)(val unwrap: quotes.reflect.AnnotatedType) extends TypeRepr {
  override type This <: AnnotatedType
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.AnnotatedType = unwrap.asInstanceOf[newQuotes.reflect.AnnotatedType]

  def underlying: TypeRepr = TypeRepr.wrap(this.unwrap.underlying)

  def annotation: Term = Term.wrap(this.unwrap.annotation)

}
object AnnotatedType {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.AnnotatedType): AnnotatedType =
    new AnnotatedType(quotes)(unwrap)

  def companion(using quotes: Quotes): AnnotatedTypeCompanion = AnnotatedTypeCompanion(using quotes)
  given Quotes => Conversion[AnnotatedType.type, AnnotatedTypeCompanion] = _.companion

  def unapply(x: AnnotatedType): (TypeRepr, Term) = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.AnnotatedType.unapply(x.unwrapWithin)
    (TypeRepr.wrap(unwrap._1), Term.wrap(unwrap._2))
  }

}

sealed trait AndOrType extends TypeRepr {
  override type This <: AndOrType
  override val unwrap: quotes.reflect.AndOrType
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.AndOrType = unwrap.asInstanceOf[newQuotes.reflect.AndOrType]

  final def left: TypeRepr = TypeRepr.wrap(this.unwrap.left)

  final def right: TypeRepr = TypeRepr.wrap(this.unwrap.right)

  // =====| Added |=====

  def cases: Set[TypeRepr]

}
object AndOrType {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.AndOrType): AndOrType = unwrap match
    case unwrap: quotes.reflect.AndType => AndType.wrap(unwrap)
    case unwrap: quotes.reflect.OrType  => OrType.wrap(unwrap)
    case _                              => throw UnknownCase("AndOrType", unwrap)

  def companion(using quotes: Quotes): AndOrTypeCompanion = AndOrTypeCompanion(using quotes)
  given Quotes => Conversion[AndOrType.type, AndOrTypeCompanion] = _.companion

}

final class AndType(val quotes: Quotes)(val unwrap: quotes.reflect.AndType) extends AndOrType {
  override type This <: AndType
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.AndType = unwrap.asInstanceOf[newQuotes.reflect.AndType]

  // =====| Added |=====

  /**
    * ((A & B) & (C | D).cases = Set(A, B, C | D)
    */
  override def cases: Set[TypeRepr] = this.andChildren

}
object AndType {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.AndType): AndType =
    new AndType(quotes)(unwrap)

  def companion(using quotes: Quotes): AndTypeCompanion = AndTypeCompanion(using quotes)
  given Quotes => Conversion[AndType.type, AndTypeCompanion] = _.companion

  def unapply(x: AndType): (TypeRepr, TypeRepr) = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.AndType.unapply(x.unwrapWithin)
    (TypeRepr.wrap(unwrap._1), TypeRepr.wrap(unwrap._2))
  }

}

final class OrType(val quotes: Quotes)(val unwrap: quotes.reflect.OrType) extends AndOrType {
  override type This <: OrType
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.OrType = unwrap.asInstanceOf[newQuotes.reflect.OrType]

  // =====| Added |=====

  /**
    * ((A | B) | (C & D).cases = Set(A, B, C & D)
    */
  override def cases: Set[TypeRepr] = this.orChildren

}
object OrType {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.OrType): OrType =
    new OrType(quotes)(unwrap)

  def companion(using quotes: Quotes): OrTypeCompanion = OrTypeCompanion(using quotes)
  given Quotes => Conversion[OrType.type, OrTypeCompanion] = _.companion

  def unapply(x: OrType): (TypeRepr, TypeRepr) = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.OrType.unapply(x.unwrapWithin)
    (TypeRepr.wrap(unwrap._1), TypeRepr.wrap(unwrap._2))
  }

}

final class MatchType(val quotes: Quotes)(val unwrap: quotes.reflect.MatchType) extends TypeRepr {
  override type This <: MatchType
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.MatchType = unwrap.asInstanceOf[newQuotes.reflect.MatchType]

  def bound: TypeRepr = TypeRepr.wrap(this.unwrap.bound)

  def scrutinee: TypeRepr = TypeRepr.wrap(this.unwrap.scrutinee)

  def cases: List[TypeRepr] = this.unwrap.cases.map(TypeRepr.wrap(_))

}
object MatchType {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.MatchType): MatchType =
    new MatchType(quotes)(unwrap)

  def companion(using quotes: Quotes): MatchTypeCompanion = MatchTypeCompanion(using quotes)
  given Quotes => Conversion[MatchType.type, MatchTypeCompanion] = _.companion

  def unapply(x: MatchType): (TypeRepr, TypeRepr, List[TypeRepr]) = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.MatchType.unapply(x.unwrapWithin)
    (TypeRepr.wrap(unwrap._1), TypeRepr.wrap(unwrap._2), unwrap._3.map(TypeRepr.wrap(_)))
  }

}

final class ByNameType(val quotes: Quotes)(val unwrap: quotes.reflect.ByNameType) extends TypeRepr {
  override type This <: ByNameType
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.ByNameType = unwrap.asInstanceOf[newQuotes.reflect.ByNameType]

  def underlying: TypeRepr = TypeRepr.wrap(this.unwrap.underlying)

}
object ByNameType {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.ByNameType): ByNameType =
    new ByNameType(quotes)(unwrap)

  def companion(using quotes: Quotes): ByNameTypeCompanion = ByNameTypeCompanion(using quotes)
  given Quotes => Conversion[ByNameType.type, ByNameTypeCompanion] = _.companion

  def unapply(x: ByNameType): Some[TypeRepr] = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.ByNameType.unapply(x.unwrapWithin)
    Some(TypeRepr.wrap(unwrap.value))
  }

}

final class ParamRef(val quotes: Quotes)(val unwrap: quotes.reflect.ParamRef) extends TypeRepr {
  override type This <: ParamRef
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.ParamRef = unwrap.asInstanceOf[newQuotes.reflect.ParamRef]

  def binder: TypeRepr = TypeRepr.wrap(this.unwrap.binder)

  def paramNum: Int = this.unwrap.paramNum

}
object ParamRef {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.ParamRef): ParamRef =
    new ParamRef(quotes)(unwrap)

  def companion(using quotes: Quotes): ParamRefCompanion = ParamRefCompanion(using quotes)
  given Quotes => Conversion[ParamRef.type, ParamRefCompanion] = _.companion

  def unapply(x: ParamRef): (TypeRepr, Int) = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.ParamRef.unapply(x.unwrapWithin)
    (TypeRepr.wrap(unwrap._1), unwrap._2)
  }

}

final class ThisType(val quotes: Quotes)(val unwrap: quotes.reflect.ThisType) extends TypeRepr {
  override type This <: ThisType
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.ThisType = unwrap.asInstanceOf[newQuotes.reflect.ThisType]

  def tref: TypeRepr = TypeRepr.wrap(this.unwrap.tref)

}
object ThisType {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.ThisType): ThisType =
    new ThisType(quotes)(unwrap)

  def companion(using quotes: Quotes): ThisTypeCompanion = ThisTypeCompanion(using quotes)
  given Quotes => Conversion[ThisType.type, ThisTypeCompanion] = _.companion

  def unapply(x: ThisType): Some[TypeRepr] = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.ThisType.unapply(x.unwrapWithin)
    Some(TypeRepr.wrap(unwrap.value))
  }

}

final class RecursiveThis(val quotes: Quotes)(val unwrap: quotes.reflect.RecursiveThis) extends TypeRepr {
  override type This <: RecursiveThis
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.RecursiveThis = unwrap.asInstanceOf[newQuotes.reflect.RecursiveThis]

  def binder: RecursiveType = RecursiveType.wrap(this.unwrap.binder)

}
object RecursiveThis {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.RecursiveThis): RecursiveThis =
    new RecursiveThis(quotes)(unwrap)

  def companion(using quotes: Quotes): RecursiveThisCompanion = RecursiveThisCompanion(using quotes)
  given Quotes => Conversion[RecursiveThis.type, RecursiveThisCompanion] = _.companion

  def unapply(x: RecursiveThis): Some[RecursiveType] = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.RecursiveThis.unapply(x.unwrapWithin)
    Some(RecursiveType.wrap(unwrap.value))
  }

}

final class RecursiveType(val quotes: Quotes)(val unwrap: quotes.reflect.RecursiveType) extends TypeRepr {
  override type This <: RecursiveType
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.RecursiveType = unwrap.asInstanceOf[newQuotes.reflect.RecursiveType]

  def underlying: TypeRepr = TypeRepr.wrap(this.unwrap.underlying)

  def recThis: RecursiveThis = RecursiveThis.wrap(this.unwrap.recThis)

}
object RecursiveType {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.RecursiveType): RecursiveType =
    new RecursiveType(quotes)(unwrap)

  def companion(using quotes: Quotes): RecursiveTypeCompanion = RecursiveTypeCompanion(using quotes)
  given Quotes => Conversion[RecursiveType.type, RecursiveTypeCompanion] = _.companion

  def unapply(x: RecursiveType): Some[TypeRepr] = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.RecursiveType.unapply(x.unwrapWithin)
    Some(TypeRepr.wrap(unwrap.value))
  }

}

sealed trait LambdaType extends TypeRepr {
  override type This <: LambdaType
  override val unwrap: quotes.reflect.LambdaType
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.LambdaType = unwrap.asInstanceOf[newQuotes.reflect.LambdaType]

  final def paramNames: List[String] = this.unwrap.paramNames

  final def paramTypes: List[TypeRepr] = this.unwrap.paramTypes.map(TypeRepr.wrap(_))

  final def resType: TypeRepr = TypeRepr.wrap(this.unwrap.resType)

}
object LambdaType {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.LambdaType): LambdaType = unwrap match
    case unwrap: quotes.reflect.MethodOrPoly => MethodOrPoly.wrap(unwrap)
    case unwrap: quotes.reflect.TypeLambda   => TypeLambda.wrap(unwrap)
    case _                                   => throw UnknownCase("LambdaType", unwrap)

  def companion(using quotes: Quotes): LambdaTypeCompanion = LambdaTypeCompanion(using quotes)
  given Quotes => Conversion[LambdaType.type, LambdaTypeCompanion] = _.companion

}

sealed trait MethodOrPoly extends LambdaType {
  override type This <: MethodOrPoly
  override val unwrap: quotes.reflect.MethodOrPoly
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.MethodOrPoly = unwrap.asInstanceOf[newQuotes.reflect.MethodOrPoly]
}
object MethodOrPoly {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.MethodOrPoly): MethodOrPoly = unwrap match
    case unwrap: quotes.reflect.MethodType => MethodType.wrap(unwrap)
    case unwrap: quotes.reflect.PolyType   => PolyType.wrap(unwrap)
    case _                                 => throw UnknownCase("MethodOrPoly", unwrap)

  def companion(using quotes: Quotes): MethodOrPolyCompanion = MethodOrPolyCompanion(using quotes)
  given Quotes => Conversion[MethodOrPoly.type, MethodOrPolyCompanion] = _.companion

}

final class MethodType(val quotes: Quotes)(val unwrap: quotes.reflect.MethodType) extends MethodOrPoly {
  override type This <: MethodType
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.MethodType = unwrap.asInstanceOf[newQuotes.reflect.MethodType]

  /** Is this the type of parameter clause like `(implicit X1, ..., Xn)`, `(using X1, ..., Xn)` or `(using x1: X1, ..., xn: Xn)` */
  def isImplicit: Boolean = this.unwrap.isImplicit

  /** Is this the type of parameter clause like `(using X1, ..., Xn)` or `(using x1: X1, x2: X2, ... )` */
  def isContextual: Boolean = this.unwrap.isContextual

  /** Returns a MethodTypeKind object representing the implicitness of the MethodType parameter clause. */
  def methodTypeKind: MethodTypeKind = MethodTypeKind.wrap(this.unwrap.methodTypeKind)

  /** List of `erased` flags for each parameters of the clause */
  @experimental
  def erasedParams: List[Boolean] = this.unwrap.erasedParams

  /** Whether the clause has any erased parameters */
  @experimental
  def hasErasedParams: Boolean = this.unwrap.hasErasedParams

  def param(idx: Int): TypeRepr = TypeRepr.wrap(this.unwrap.param(idx))

}
object MethodType {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.MethodType): MethodType =
    new MethodType(quotes)(unwrap)

  def companion(using quotes: Quotes): MethodTypeCompanion = MethodTypeCompanion(using quotes)
  given Quotes => Conversion[MethodType.type, MethodTypeCompanion] = _.companion

  def unapply(x: MethodType): (List[String], List[TypeRepr], TypeRepr) = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.MethodType.unapply(x.unwrapWithin)
    (unwrap._1, unwrap._2.map(TypeRepr.wrap(_)), TypeRepr.wrap(unwrap._3))
  }

}

final class PolyType(val quotes: Quotes)(val unwrap: quotes.reflect.PolyType) extends MethodOrPoly {
  override type This <: PolyType
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.PolyType = unwrap.asInstanceOf[newQuotes.reflect.PolyType]

  def param(idx: Int): TypeRepr = TypeRepr.wrap(this.unwrap.param(idx))

  def paramBounds: List[TypeBounds] = this.unwrap.paramBounds.map(TypeBounds.wrap(_))

}
object PolyType {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.PolyType): PolyType =
    new PolyType(quotes)(unwrap)

  def companion(using quotes: Quotes): PolyTypeCompanion = PolyTypeCompanion(using quotes)
  given Quotes => Conversion[PolyType.type, PolyTypeCompanion] = _.companion

  def unapply(x: PolyType): (List[String], List[TypeBounds], TypeRepr) = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.PolyType.unapply(x.unwrapWithin)
    (unwrap._1, unwrap._2.map(TypeBounds.wrap(_)), TypeRepr.wrap(unwrap._3))
  }

}

final class TypeLambda(val quotes: Quotes)(val unwrap: quotes.reflect.TypeLambda) extends LambdaType {
  override type This <: TypeLambda
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.TypeLambda = unwrap.asInstanceOf[newQuotes.reflect.TypeLambda]

  /** Reference to the i-th parameter */
  def param(idx: Int): TypeRepr = TypeRepr.wrap(this.unwrap.param(idx))

  /** Type bounds of the i-th parameter */
  def paramBounds: List[TypeBounds] = this.unwrap.paramBounds.map(TypeBounds.wrap(_))

  /**
    * Variance flags for the i-th parameter
    *
    *  Variance flags can be one of `Flags.{Covariant, Contravariant, EmptyFlags}`.
    */
  def paramVariances: List[Flags] = this.unwrap.paramVariances.map(Flags.wrap(_))

}
object TypeLambda {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.TypeLambda): TypeLambda =
    new TypeLambda(quotes)(unwrap)

  def companion(using quotes: Quotes): TypeLambdaCompanion = TypeLambdaCompanion(using quotes)
  given Quotes => Conversion[TypeLambda.type, TypeLambdaCompanion] = _.companion

  def unapply(x: TypeLambda): (List[String], List[TypeBounds], TypeRepr) = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.TypeLambda.unapply(x.unwrapWithin)
    (unwrap._1, unwrap._2.map(TypeBounds.wrap(_)), TypeRepr.wrap(unwrap._3))
  }

}

final class MatchCase(val quotes: Quotes)(val unwrap: quotes.reflect.MatchCase) extends TypeRepr {
  override type This <: MatchCase
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.MatchCase = unwrap.asInstanceOf[newQuotes.reflect.MatchCase]

  /** Pattern `P` of `case P => R` in a `MatchType` */
  def pattern: TypeRepr = TypeRepr.wrap(this.unwrap.pattern)

  /** RHS `R` of `case P => R` in a `MatchType` */
  def rhs: TypeRepr = TypeRepr.wrap(this.unwrap.rhs)

}
object MatchCase {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.MatchCase): MatchCase =
    new MatchCase(quotes)(unwrap)

  def companion(using quotes: Quotes): MatchCaseCompanion = MatchCaseCompanion(using quotes)
  given Quotes => Conversion[MatchCase.type, MatchCaseCompanion] = _.companion

  def unapply(x: MatchCase): (TypeRepr, TypeRepr) = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.MatchCase.unapply(x.unwrapWithin)
    (TypeRepr.wrap(unwrap._1), TypeRepr.wrap(unwrap._2))
  }

}

final class TypeBounds(val quotes: Quotes)(val unwrap: quotes.reflect.TypeBounds) extends TypeRepr {
  override type This <: TypeBounds
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.TypeBounds = unwrap.asInstanceOf[newQuotes.reflect.TypeBounds]

  def low: TypeRepr = TypeRepr.wrap(this.unwrap.low)

  def hi: TypeRepr = TypeRepr.wrap(this.unwrap.hi)

}
object TypeBounds {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.TypeBounds): TypeBounds =
    new TypeBounds(quotes)(unwrap)

  def companion(using quotes: Quotes): TypeBoundsCompanion = TypeBoundsCompanion(using quotes)
  given Quotes => Conversion[TypeBounds.type, TypeBoundsCompanion] = _.companion

  def unapply(x: TypeBounds): (TypeRepr, TypeRepr) = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.TypeBounds.unapply(x.unwrapWithin)
    (TypeRepr.wrap(unwrap._1), TypeRepr.wrap(unwrap._2))
  }

}

final class NoPrefix(val quotes: Quotes)(val unwrap: quotes.reflect.NoPrefix) extends TypeRepr {
  override type This <: NoPrefix
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.NoPrefix = unwrap.asInstanceOf[newQuotes.reflect.NoPrefix]
}
object NoPrefix {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.NoPrefix): NoPrefix =
    new NoPrefix(quotes)(unwrap)

  def companion(using quotes: Quotes): NoPrefixCompanion = NoPrefixCompanion(using quotes)
  given Quotes => Conversion[NoPrefix.type, NoPrefixCompanion] = _.companion

  def unapply(x: NoPrefix): Boolean = {
    given q: Quotes = x.quotes
    q.reflect.NoPrefix.unapply(x.unwrapWithin)
  }

}

final class FlexibleType(val quotes: Quotes)(val unwrap: quotes.reflect.FlexibleType) extends TypeRepr {
  override type This <: FlexibleType
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.FlexibleType = unwrap.asInstanceOf[newQuotes.reflect.FlexibleType]

  def underlying: TypeRepr = TypeRepr.wrap(this.unwrap.underlying)

  def lo: TypeRepr = TypeRepr.wrap(this.unwrap.lo)

  def hi: TypeRepr = TypeRepr.wrap(this.unwrap.hi)

}
object FlexibleType {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.FlexibleType): FlexibleType =
    new FlexibleType(quotes)(unwrap)

  def companion(using quotes: Quotes): FlexibleTypeCompanion = FlexibleTypeCompanion(using quotes)
  given Quotes => Conversion[FlexibleType.type, FlexibleTypeCompanion] = _.companion

  def unapply(x: FlexibleType): Option[TypeRepr] = {
    given q: Quotes = x.quotes
    q.reflect.FlexibleType.unapply(x.unwrapWithin).map(TypeRepr.wrap(_))
  }

}
