package oxygen.quoted

import oxygen.quoted.companion.*
import scala.annotation.experimental
import scala.quoted.*

final class Symbol private (using val quotes: Quotes)(val unwrap: quotes.reflect.Symbol) extends Model, HasTypeType {
  def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Symbol = this.unwrap.asInstanceOf[newQuotes.reflect.Symbol]

  /** Owner of this symbol. The owner is the symbol in which this symbol is defined. Throws if this symbol does not have an owner. */
  def owner: Symbol = Symbol.wrap(this.unwrap.owner)

  /** Owner of this symbol. The owner is the symbol in which this symbol is defined. Returns `NoSymbol` if this symbol does not have an owner. */
  def maybeOwner: Symbol = Symbol.wrap(this.unwrap.maybeOwner)

  /** Flags of this symbol */
  def flags: Flags = Flags.wrap(this.unwrap.flags)

  /** This symbol is private within the resulting type */
  def privateWithin: Option[TypeRepr] = this.unwrap.privateWithin.map(TypeRepr.wrap(_))

  /** This symbol is protected within the resulting type */
  def protectedWithin: Option[TypeRepr] = this.unwrap.protectedWithin.map(TypeRepr.wrap(_))

  /** The name of this symbol */
  def name: String = this.unwrap.name

  /** The full name of this symbol up to the root package */
  def fullName: String = this.unwrap.fullName

  /** Type of the definition */
  @experimental
  def info: TypeRepr = TypeRepr.wrap(this.unwrap.info)

  /** The position of this symbol */
  def pos: Option[Position] = this.unwrap.pos.map(Position.wrap(_))

  /** The documentation for this symbol, if any */
  def docstring: Option[String] = this.unwrap.docstring

  /**
    * Tree of this definition
    *
    *  If this symbol `isClassDef` it will return `a `ClassDef`,
    *  if this symbol `isTypeDef` it will return `a `TypeDef`,
    *  if this symbol `isValDef` it will return `a `ValDef`,
    *  if this symbol `isDefDef` it will return `a `DefDef`
    *  if this symbol `isBind` it will return `a `Bind`,
    *  else will throw
    *
    *  **Warning**: avoid using this method in macros.
    *
    *  **Caveat**: The tree is not guaranteed to exist unless the compiler
    *  option `-Yretain-trees` is enabled.
    *
    *  **Anti-pattern**: The following code is an anti-pattern:
    *
    *      symbol.tree.tpe
    *
    *  It should be replaced by one of the following:
    *
    *      tp.memberType(symbol)
    *      symbol.typeRef
    *      symbol.termRef
    */
  def tree: Tree = Tree.wrap(this.unwrap.tree)

  /** Is the annotation defined with `annotSym` attached to this symbol? */
  def hasAnnotation(annotSym: Symbol): Boolean = this.unwrap.hasAnnotation(annotSym.unwrapWithin)

  /** Get the annotation defined with `annotSym` attached to this symbol */
  def getAnnotation(annotSym: Symbol): Option[Term] = this.unwrap.getAnnotation(annotSym.unwrapWithin).map(Term.wrap(_))

  /** Does this symbol come from a currently compiled source file? */
  def isDefinedInCurrentRun: Boolean = this.unwrap.isDefinedInCurrentRun

  /**
    * Dummy val symbol that owns all statements within the initialization of the class.
    *  This may also contain local definitions such as classes defined in a `locally` block in the class.
    */
  def isLocalDummy: Boolean = this.unwrap.isLocalDummy

  /** Is this symbol a class representing a refinement? */
  def isRefinementClass: Boolean = this.unwrap.isRefinementClass

  /** Is this symbol an alias type? */
  def isAliasType: Boolean = this.unwrap.isAliasType

  /** Is this symbol an anonymous class? */
  def isAnonymousClass: Boolean = this.unwrap.isAnonymousClass

  /** Is this symbol an anonymous function? */
  def isAnonymousFunction: Boolean = this.unwrap.isAnonymousFunction

  /** Is this symbol an abstract type or a type parameter? */
  def isAbstractType: Boolean = this.unwrap.isAbstractType

  /** Is this the constructor of a class? */
  def isClassConstructor: Boolean = this.unwrap.isClassConstructor

  /** Is this the super accessor? */
  def isSuperAccessor: Boolean = this.unwrap.isSuperAccessor

  /** Is this the definition of a type? */
  def isType: Boolean = this.unwrap.isType

  /** Is this the definition of a term? */
  def isTerm: Boolean = this.unwrap.isTerm

  /** Is this the definition of a PackageDef tree? */
  def isPackageDef: Boolean = this.unwrap.isPackageDef

  /** Is this the definition of a ClassDef tree? */
  def isClassDef: Boolean = this.unwrap.isClassDef

  /** Is this the definition of a TypeDef tree */
  def isTypeDef: Boolean = this.unwrap.isTypeDef

  /** Is this the definition of a ValDef tree? */
  def isValDef: Boolean = this.unwrap.isValDef

  /** Is this the definition of a DefDef tree? */
  def isDefDef: Boolean = this.unwrap.isDefDef

  /** Is this the definition of a Bind pattern? */
  def isBind: Boolean = this.unwrap.isBind

  /** Does this symbol represent a no definition? */
  def isNoSymbol: Boolean = this.unwrap.isNoSymbol

  /** Does this symbol represent a definition? */
  def exists: Boolean = this.unwrap.exists

  /** Field with the given name directly declared in the class */
  def declaredField(name: String): Symbol = Symbol.wrap(this.unwrap.declaredField(name))

  /** Fields directly declared in the class */
  def declaredFields: List[Symbol] = this.unwrap.declaredFields.map(Symbol.wrap(_))

  /** Get named non-private fields declared or inherited */
  def fieldMember(name: String): Symbol = Symbol.wrap(this.unwrap.fieldMember(name))

  /** Get all non-private fields declared or inherited */
  def fieldMembers: List[Symbol] = this.unwrap.fieldMembers.map(Symbol.wrap(_))

  /** Get non-private named methods defined directly inside the class */
  def declaredMethod(name: String): List[Symbol] = this.unwrap.declaredMethod(name).map(Symbol.wrap(_))

  /** Get all non-private methods defined directly inside the class, excluding constructors */
  def declaredMethods: List[Symbol] = this.unwrap.declaredMethods.map(Symbol.wrap(_))

  /** Get named non-private methods declared or inherited */
  def methodMember(name: String): List[Symbol] = this.unwrap.methodMember(name).map(Symbol.wrap(_))

  /** Get all non-private methods declared or inherited */
  def methodMembers: List[Symbol] = this.unwrap.methodMembers.map(Symbol.wrap(_))

  /** Get non-private named type defined directly inside the class */
  def declaredType(name: String): List[Symbol] = this.unwrap.declaredType(name).map(Symbol.wrap(_))

  /** Get all non-private types defined directly inside the class */
  def declaredTypes: List[Symbol] = this.unwrap.declaredTypes.map(Symbol.wrap(_))

  /** Type member with the given name declared or inherited in the class */
  def typeMember(name: String): Symbol = Symbol.wrap(this.unwrap.typeMember(name))

  /** Type member directly declared or inherited in the class */
  def typeMembers: List[Symbol] = this.unwrap.typeMembers.map(Symbol.wrap(_))

  /** All members directly declared in the class */
  def declarations: List[Symbol] = this.unwrap.declarations.map(Symbol.wrap(_))

  /**
    * The symbols of each type parameter list and value parameter list of this
    *  method, or Nil if this isn't a method.
    */
  def paramSymss: List[List[Symbol]] = this.unwrap.paramSymss.map(_.map(Symbol.wrap(_)))

  /** Returns all symbols overridden by this symbol. */
  def allOverriddenSymbols: Iterator[Symbol] = this.unwrap.allOverriddenSymbols.iterator.map(Symbol.wrap(_))

  /**
    * The symbol overriding this symbol in given subclass `ofclazz`.
    *
    *  @param ofclazz is a subclass of this symbol's owner
    */
  def overridingSymbol(ofclazz: Symbol): Symbol = Symbol.wrap(this.unwrap.overridingSymbol(ofclazz.unwrapWithin))

  /** The primary constructor of a class or trait, `noSymbol` if not applicable. */
  def primaryConstructor: Symbol = Symbol.wrap(this.unwrap.primaryConstructor)

  /** Fields of a case class type -- only the ones declared in primary constructor */
  def caseFields: List[Symbol] = this.unwrap.caseFields.map(Symbol.wrap(_))

  /** Is this the symbol of a type parameter */
  def isTypeParam: Boolean = this.unwrap.isTypeParam

  /**
    * Variance flags for of this type parameter.
    *
    *  Variance flags can be one of `Flags.{Covariant, Contravariant, EmptyFlags}`.
    *  If this is not the symbol of a type parameter the result is `Flags.EmptyFlags`.
    */
  def paramVariance: Flags = Flags.wrap(this.unwrap.paramVariance)

  /** Signature of this definition */
  def signature: Signature = Signature.wrap(this.unwrap.signature)

  /** The class symbol of the companion module class */
  def moduleClass: Symbol = Symbol.wrap(this.unwrap.moduleClass)

  /** The symbol of the companion class */
  def companionClass: Symbol = Symbol.wrap(this.unwrap.companionClass)

  /** The symbol of the companion module */
  def companionModule: Symbol = Symbol.wrap(this.unwrap.companionModule)

  /** Case class or case object children of a sealed trait or cases of an `enum`. */
  def children: List[Symbol] = this.unwrap.children.map(Symbol.wrap(_))

  /**
    * Returns a nested quote with this symbol as splice owner (`Symbol.spliceOwner`).
    *
    *  Changes the owner under which the definition in a quote are created.
    *
    *  Usages:
    *  ```scala
    *  def rhsExpr(using q: Quotes): Expr[Unit] =
    *    import q.reflect.*
    *    '{ val y = ???; (y, y) }
    *  def aValDef(using q: Quotes)(owner: q.reflect.Symbol) =
    *    import q.reflect.*
    *    val sym = Symbol.newVal(owner, "x", TypeRepr.of[Unit], Flags.EmptyFlags, Symbol.noSymbol)
    *    val rhs = rhsExpr(using sym.asQuotes).asTerm
    *    ValDef(sym, Some(rhs))
    *  ```
    *
    *  ```scala
    *  //{
    *  def inQuotes(using q: Quotes) = {
    *    import q.reflect.*
    *  //}
    *    new TreeMap:
    *      override def transformTerm(tree: Term)(owner: Symbol): Term =
    *        tree match
    *          case tree: Ident =>
    *            given Quotes = owner.asQuotes
    *            // Definitions contained in the quote will be owned by `owner`.
    *            // No need to use `changeOwner` in this case.
    *            '{ val x = ???; x }.asTerm
    *  //{
    *  }
    *  //}
    *  ```
    */
  def asQuotes: Quotes = this.unwrap.asQuotes

  /**
    * Type reference to the symbol usable in the scope of its owner.
    *
    *  To get a reference to a symbol from a specific prefix `tp`, use `tp.select(symbol)` instead.
    *  @see TypeReprMethods.select
    *
    *  @pre symbol.isType returns true
    */
  def typeRef: TypeRef = TypeRef.wrap(this.unwrap.typeRef)

  /**
    * Term reference to the symbol usable in the scope of its owner.
    *
    *  @pre symbol.isType returns false
    */
  def termRef: TermRef = TermRef.wrap(this.unwrap.termRef)

  // =====| Added |=====

  def asQuotesUsing[A](f: Quotes ?=> A): A = f(using this.asQuotes)

  override protected def showSelf: String = fullName
  override protected def typeTypeInternal: Option[TypeType] = TypeType.fromSym(this)

  def annotations: Annotations = new Annotations(this.unwrap.annotations.map(Term.wrap(_)), this.unwrap.toString)

  def toTerm: Term = this.termRef.toTerm

  def isTupleClass: Boolean = quotes.reflect.defn.isTupleClass(this.unwrap)

  // =====| Added |=====

  override def maybePos: Option[Position] = pos

}
object Symbol {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Symbol): Symbol = Symbol(unwrap)

  def companion(using quotes: Quotes): SymbolCompanion = SymbolCompanion(using quotes)
  given Quotes => Conversion[Symbol.type, SymbolCompanion] = _.companion

}
