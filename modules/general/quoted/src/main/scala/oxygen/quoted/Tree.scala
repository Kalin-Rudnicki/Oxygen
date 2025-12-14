package oxygen.quoted

import oxygen.quoted.companion.*
import oxygen.quoted.error.UnknownCase
import scala.quoted.*
import scala.reflect.TypeTest

sealed trait Tree extends Model {
  type This <: Tree
  val quotes: Quotes
  val unwrap: quotes.reflect.Tree
  def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Tree = unwrap.asInstanceOf[newQuotes.reflect.Tree]
  given givenQuotes: quotes.type = quotes

  /** Position in the source code */
  final def pos: Position = Position.wrap(this.unwrap.pos)

  /** Symbol of defined or referred by this tree */
  final def symbol: Symbol = Symbol.wrap(this.unwrap.symbol)

  /** Shows the tree as String */
  final def show(using printer: Printer[Tree]): String = printer.show(this)

  /** Does this tree represent a valid expression? */
  final def isExpr: Boolean = this.unwrap.isExpr

  /** Convert this tree to an `quoted.Expr[Any]` if the tree is a valid expression or throws */
  final def asExpr: Expr[Any] = this.unwrap.asExpr

  /** Convert this tree to an `quoted.Expr[T]` if the tree is a valid expression or throws */
  final def asExprOf[T](using Type[T]): Expr[T] = this.unwrap.asExprOf[T]

  /** Changes the owner of the symbols in the tree */
  final def changeOwner(newOwner: Symbol): This = Tree.wrap(this.unwrap.changeOwner(newOwner.unwrapWithin)).asInstanceOf[This]

  // =====| Added |=====

  override final def maybePos: Option[Position] = Some(pos)

  final def showWith(f: PrinterCompanion => Printer[Tree]): String = this.show(using f(Printer.companion(using quotes)))
  final def showCode: String = showWith(_.TreeCode)
  final def showShortCode: String = showWith(_.TreeShortCode)
  final def showAnsiCode: String = showWith(_.TreeAnsiCode)
  final def showStructure: String = showWith(_.TreeStructure)

}
object Tree {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Tree): Tree = unwrap match
    case unwrap: quotes.reflect.PackageClause    => PackageClause.wrap(unwrap)
    case unwrap: quotes.reflect.Statement        => Statement.wrap(unwrap)
    case unwrap: quotes.reflect.TypedOrTest      => TypedOrTest.wrap(unwrap)
    case unwrap: quotes.reflect.Bind             => Bind.wrap(unwrap)
    case unwrap: quotes.reflect.Unapply          => Unapply.wrap(unwrap)
    case unwrap: quotes.reflect.Alternatives     => Alternatives.wrap(unwrap)
    case unwrap: quotes.reflect.CaseDef          => CaseDef.wrap(unwrap)
    case unwrap: quotes.reflect.TypeCaseDef      => TypeCaseDef.wrap(unwrap)
    case unwrap: quotes.reflect.TypeTree         => TypeTree.wrap(unwrap)
    case unwrap: quotes.reflect.TypeBoundsTree   => TypeBoundsTree.wrap(unwrap)
    case unwrap: quotes.reflect.WildcardTypeTree => WildcardTypeTree.wrap(unwrap)
    case _                                       => throw UnknownCase("Tree", unwrap)

  def companion(using quotes: Quotes): TreeCompanion = TreeCompanion(using quotes)
  given Quotes => Conversion[Tree.type, TreeCompanion] = _.companion

}

final class PackageClause(val quotes: Quotes)(val unwrap: quotes.reflect.PackageClause) extends Tree {
  override type This <: PackageClause
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.PackageClause = unwrap.asInstanceOf[newQuotes.reflect.PackageClause]

  /** Tree containing the package name */
  def pid: Ref = Ref.wrap(this.unwrap.pid)

  /** Definitions, imports or exports within the package */
  def stats: List[Tree] = this.unwrap.stats.map(Tree.wrap(_))

}
object PackageClause {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.PackageClause): PackageClause =
    new PackageClause(quotes)(unwrap)

  def companion(using quotes: Quotes): PackageClauseCompanion = PackageClauseCompanion(using quotes)
  given Quotes => Conversion[PackageClause.type, PackageClauseCompanion] = _.companion

  /** Matches a package clause `package pid { stats }` and extracts the `pid` and `stats` */
  def unapply(tree: PackageClause): (Ref, List[Tree]) = {
    given q: Quotes = tree.quotes
    val unwrap = q.reflect.PackageClause.unapply(tree.unwrapWithin)
    (Ref.wrap(unwrap._1), unwrap._2.map(Tree.wrap(_)))
  }
}

sealed trait Statement extends Tree {
  override type This <: Statement
  override val unwrap: quotes.reflect.Statement
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Statement = unwrap.asInstanceOf[newQuotes.reflect.Statement]
}
object Statement {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Statement): Statement =
    unwrap match
      case unwrap: quotes.reflect.Import     => Import.wrap(unwrap)
      case unwrap: quotes.reflect.Export     => Export.wrap(unwrap)
      case unwrap: quotes.reflect.Definition => Definition.wrap(unwrap)
      case unwrap: quotes.reflect.Term       => Term.wrap(unwrap)
      case _                                 => throw UnknownCase("Statement", unwrap)

  def companion(using quotes: Quotes): StatementCompanion = StatementCompanion(using quotes)
  given Quotes => Conversion[Statement.type, StatementCompanion] = _.companion

}

final class Import(val quotes: Quotes)(val unwrap: quotes.reflect.Import) extends Statement {
  override type This <: Import
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Import = unwrap.asInstanceOf[newQuotes.reflect.Import]

  /** Qualifier of the import */
  def expr: Term = Term.wrap(this.unwrap.expr)

  /**
    * List selectors of the import
    *
    *  See documentation on `Selector`
    */
  def selectors: List[Selector] = this.unwrap.selectors.map(Selector.wrap(_))

}
object Import {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Import): Import =
    new Import(quotes)(unwrap)

  def companion(using quotes: Quotes): ImportCompanion = ImportCompanion(using quotes)
  given Quotes => Conversion[Import.type, ImportCompanion] = _.companion

  /** Matches an `Import` and extracts the qualifier and selectors */
  def unapply(tree: Import): (Term, List[Selector]) = {
    given q: Quotes = tree.quotes
    val unwrap = q.reflect.Import.unapply(tree.unwrapWithin)
    (Term.wrap(unwrap._1), unwrap._2.map(Selector.wrap(_)))
  }

}

final class Export(val quotes: Quotes)(val unwrap: quotes.reflect.Export) extends Statement {
  override type This <: Export
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Export = unwrap.asInstanceOf[newQuotes.reflect.Export]

  /** Qualifier of the export */
  def expr: Term = Term.wrap(this.unwrap.expr)

  /**
    * List selectors of the export
    *
    *  See documentation on `Selector`
    */
  def selectors: List[Selector] = this.unwrap.selectors.map(Selector.wrap(_))

}
object Export {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Export): Export =
    new Export(quotes)(unwrap)

  def companion(using quotes: Quotes): ExportCompanion = ExportCompanion(using quotes)
  given Quotes => Conversion[Export.type, ExportCompanion] = _.companion

  def unapply(tree: Export): (Term, List[Selector]) = {
    given q: Quotes = tree.quotes
    val unwrap = q.reflect.Export.unapply(tree.unwrapWithin)
    (Term.wrap(unwrap._1), unwrap._2.map(Selector.wrap(_)))
  }

}

sealed trait Definition extends Statement {
  override type This <: Definition
  override val unwrap: quotes.reflect.Definition
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Definition = unwrap.asInstanceOf[newQuotes.reflect.Definition]

  /** Name of the definition */
  final def name: String = this.unwrap.name

}
object Definition {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Definition): Definition =
    unwrap match
      case unwrap: quotes.reflect.ClassDef    => ClassDef.wrap(unwrap)
      case unwrap: quotes.reflect.TypeDef     => TypeDef.wrap(unwrap)
      case unwrap: quotes.reflect.ValOrDefDef => ValOrDefDef.wrap(unwrap)
      case _                                  => throw UnknownCase("Definition", unwrap)

  def companion(using quotes: Quotes): DefinitionCompanion = DefinitionCompanion(using quotes)
  given Quotes => Conversion[Definition.type, DefinitionCompanion] = _.companion

}

final class ClassDef(val quotes: Quotes)(val unwrap: quotes.reflect.ClassDef) extends Definition {
  override type This <: ClassDef
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.ClassDef = unwrap.asInstanceOf[newQuotes.reflect.ClassDef]

  /** The primary constructor of this class */
  def constructor: DefDef = DefDef.wrap(this.unwrap.constructor)

  /**
    * List of extended parent classes or traits.
    *  The first parent is always a class.
    */
  def parents: List[Term | TypeTree] = this.unwrap.parents.map(TermOrTypeTree.wrap(_))

  /**
    * Self-type of the class
    *
    *  ```scala
    *  //{
    *  type T
    *  //}
    *  class C { self: T =>
    *    ???
    *  }
    *  ```
    */
  def self: Option[ValDef] = this.unwrap.self.map(ValDef.wrap(_))

  /**
    * Statements within the class
    *
    *  ```scala
    *  class C {
    *    ??? // statements
    *  }
    *  ```
    */
  def body: List[Statement] = this.unwrap.body.map(Statement.wrap(_))

}
object ClassDef {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.ClassDef): ClassDef =
    new ClassDef(quotes)(unwrap)

  def companion(using quotes: Quotes): ClassDefCompanion = ClassDefCompanion(using quotes)
  given Quotes => Conversion[ClassDef.type, ClassDefCompanion] = _.companion

  /** Matches a class definition and extracts its name, constructor, parents, self, and body */
  def unapply(cdef: ClassDef): (String, DefDef, List[Term | TypeTree], Option[ValDef], List[Statement]) = {
    given q: Quotes = cdef.quotes
    val (name, constr, parents, self, body) = q.reflect.ClassDef.unapply(cdef.unwrapWithin)
    (
      name,
      DefDef.wrap(constr),
      parents.map(TermOrTypeTree.wrap(_)),
      self.map(ValDef.wrap(_)),
      body.map(Statement.wrap(_)),
    )
  }

}

final class TypeDef(val quotes: Quotes)(val unwrap: quotes.reflect.TypeDef) extends Definition {
  override type This <: TypeDef
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.TypeDef = unwrap.asInstanceOf[newQuotes.reflect.TypeDef]

  /** The type bounds on the right-hand side of this `type` definition */
  def rhs: Tree = Tree.wrap(this.unwrap.rhs)

}
object TypeDef {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.TypeDef): TypeDef =
    new TypeDef(quotes)(unwrap)

  def companion(using quotes: Quotes): TypeDefCompanion = TypeDefCompanion(using quotes)
  given Quotes => Conversion[TypeDef.type, TypeDefCompanion] = _.companion

  def unapply(tdef: TypeDef): (String, Tree) = {
    given q: Quotes = tdef.quotes
    val (name, rhs) = q.reflect.TypeDef.unapply(tdef.unwrapWithin)
    (name, Tree.wrap(rhs))
  }

}

sealed trait ValOrDefDef extends Definition {
  override type This <: ValOrDefDef
  override val unwrap: quotes.reflect.ValOrDefDef
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.ValOrDefDef = unwrap.asInstanceOf[newQuotes.reflect.ValOrDefDef]

  /** The type tree of this `val` or `def` definition */
  final def tpt: TypeTree = TypeTree.wrap(this.unwrap.tpt)

  /** The right-hand side of this `val` or `def` definition */
  final def rhs: Option[Term] = this.unwrap.rhs.map(Term.wrap(_))

  // =====| Added |=====

  final def requiredRHS: Term = this.rhs.getOrElse(report.errorAndAbort("missing required RHS", pos))

}
object ValOrDefDef {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.ValOrDefDef): ValOrDefDef =
    unwrap match
      case unwrap: quotes.reflect.ValDef => ValDef.wrap(unwrap)
      case unwrap: quotes.reflect.DefDef => DefDef.wrap(unwrap)
      case _                             => throw UnknownCase("ValOrDefDef", unwrap)

  def companion(using quotes: Quotes): ValOrDefDefCompanion = ValOrDefDefCompanion(using quotes)
  given Quotes => Conversion[ValOrDefDef.type, ValOrDefDefCompanion] = _.companion

}

final class DefDef(val quotes: Quotes)(val unwrap: quotes.reflect.DefDef) extends ValOrDefDef {
  override type This <: DefDef
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.DefDef = unwrap.asInstanceOf[newQuotes.reflect.DefDef]

  /** List of type and term parameter clauses */
  def paramss: List[ParamClause] = this.unwrap.paramss.map(ParamClause.wrap(_))

  /**
    * List of leading type parameters or Nil if the method does not have leading type parameters.
    *
    *  Note: Non leading type parameters can be found in extension methods such as
    *  ```scala
    *  //{
    *  type A
    *  type T
    *  //}
    *  extension (a: A) def f[T]() = ???
    *  ```
    */
  def leadingTypeParams: List[TypeDef] = this.unwrap.leadingTypeParams.map(TypeDef.wrap(_))

  /**
    * List of parameter clauses following the leading type parameters or all clauses.
    *  Return all parameter clauses if there are no leading type parameters.
    *
    *  Non leading type parameters can be found in extension methods such as
    *  ```scala
    *  //{
    *  type T
    *  type A
    *  //}
    *  extension (a: A) def f[T]() = ???
    *  ```
    */
  def trailingParamss: List[ParamClause] = this.unwrap.trailingParamss.map(ParamClause.wrap(_))

  /** List of term parameter clauses */
  def termParamss: List[TermParamClause] = this.unwrap.termParamss.map(TermParamClause.wrap(_))

  /** The tree of the return type of this `def` definition */
  def returnTpt: TypeTree = TypeTree.wrap(this.unwrap.returnTpt)

  // =====| Added |=====

  def methodType: TypeRepr =
    paramss.foldRight(returnTpt.tpe) {
      case (params: TermParamClause, accTpe) => MethodType.companion.apply(params.params.map(_.name))(_ => params.params.map(_.tpt.tpe.widen), _ => accTpe)
      case (_: TypeParamClause, _)           => report.errorAndAbort("not yet supported: DefDef.methodType with type params")
    }

}
object DefDef {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.DefDef): DefDef =
    new DefDef(quotes)(unwrap)

  def companion(using quotes: Quotes): DefDefCompanion = DefDefCompanion(using quotes)
  given Quotes => Conversion[DefDef.type, DefDefCompanion] = _.companion

  def unapply(ddef: DefDef): (String, List[ParamClause], TypeTree, Option[Term]) = {
    given q: Quotes = ddef.quotes
    val (name, paramss, tpt, rhs) = q.reflect.DefDef.unapply(ddef.unwrapWithin)
    (
      name,
      paramss.map(ParamClause.wrap(_)),
      TypeTree.wrap(tpt),
      rhs.map(Term.wrap(_)),
    )
  }

}

final class ValDef(val quotes: Quotes)(val unwrap: quotes.reflect.ValDef) extends ValOrDefDef {
  override type This <: ValDef
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.ValDef = unwrap.asInstanceOf[newQuotes.reflect.ValDef]

  // =====| Added |=====

  /**
    * val a = ...
    * returns a ref to `a`
    */
  def valRef: Ref = Ref(this.symbol)

}
object ValDef {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.ValDef): ValDef =
    new ValDef(quotes)(unwrap)

  def companion(using quotes: Quotes): ValDefCompanion = ValDefCompanion(using quotes)
  given Quotes => Conversion[ValDef.type, ValDefCompanion] = _.companion

  def unapply(vdef: ValDef): (String, TypeTree, Option[Term]) = {
    given q: Quotes = vdef.quotes
    val (name, tpt, rhs) = q.reflect.ValDef.unapply(vdef.unwrapWithin)
    (name, TypeTree.wrap(tpt), rhs.map(Term.wrap(_)))
  }

  // =====| Added |=====

  enum ValType {
    case Val, LazyVal, Var

    final def toFlags(using Quotes): Flags = this match
      case ValType.Val     => Flags.EmptyFlags
      case ValType.LazyVal => Flags.Lazy
      case ValType.Var     => Flags.Mutable

  }

}

sealed trait Term extends Statement {
  override type This <: Term
  override val unwrap: quotes.reflect.Term
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Term = unwrap.asInstanceOf[newQuotes.reflect.Term]

  /** TypeRepr of this term */
  final def tpe: TypeRepr = TypeRepr.wrap(this.unwrap.tpe)

  /**
    * Replace Inlined nodes and InlineProxy references to underlying arguments.
    *  The resulting tree is useful for inspection of the value or content of a non-inline argument.
    *
    *  Warning: This tree may contain references that are out of scope and should not be used in the generated code.
    *           This method should only used to port Scala 2 that used to access their outer scope unsoundly.
    */
  final def underlyingArgument: Term = Term.wrap(this.unwrap.underlyingArgument)

  /**
    * Replace Ident nodes references to the underlying tree that defined them.
    *  The resulting tree is useful for inspection of the definition of some bindings.
    *
    *  Warning: This tree may contain references that are out of scope and should not be used in the generated code.
    *           This method should only used to port Scala 2 that used to access their outer scope unsoundly.
    */
  final def underlying: Term = Term.wrap(this.unwrap.underlying)

  /** Converts a partially applied term into a lambda expression */
  final def etaExpand(owner: Symbol): Term = Term.wrap(this.unwrap.etaExpand(owner.unwrapWithin))

  /** A unary apply node with given argument: `tree(arg)` */
  final def appliedTo(arg: Term): Term = Term.wrap(this.unwrap.appliedTo(arg.unwrapWithin))

  /** An apply node with given arguments: `tree(arg, args0, ..., argsN)` */
  final def appliedTo(arg: Term, args: Term*): Term = Term.wrap(this.unwrap.appliedTo(arg.unwrapWithin, args.map(_.unwrapWithin)*))

  /** An apply node with given argument list `tree(args(0), ..., args(args.length - 1))` */
  final def appliedToArgs(args: List[Term]): Apply = Apply.wrap(this.unwrap.appliedToArgs(args.map(_.unwrapWithin)))

  /**
    * The current tree applied to given argument lists:
    *  `tree (argss(0)) ... (argss(argss.length -1))`
    */
  final def appliedToArgss(argss: List[List[Term]]): Term = Term.wrap(this.unwrap.appliedToArgss(argss.map(_.map(_.unwrapWithin))))

  /** The current tree applied to (): `tree()` */
  final def appliedToNone: Apply = Apply.wrap(this.unwrap.appliedToNone)

  /** The current tree applied to given type argument: `tree[targ]` */
  final def appliedToType(targ: TypeRepr): Term = Term.wrap(this.unwrap.appliedToType(targ.unwrapWithin))

  /** The current tree applied to given type arguments: `tree[targ0, ..., targN]` */
  final def appliedToTypes(targs: List[TypeRepr]): Term = Term.wrap(this.unwrap.appliedToTypes(targs.map(_.unwrapWithin)))

  /** The current tree applied to given type argument list: `tree[targs(0), ..., targs(targs.length - 1)]` */
  final def appliedToTypeTrees(targs: List[TypeTree]): Term = Term.wrap(this.unwrap.appliedToTypeTrees(targs.map(_.unwrapWithin)))

  /** A select node that selects the given symbol. */
  final def select(sym: Symbol): Select = Select.wrap(this.unwrap.select(sym.unwrapWithin))

  // =====| Added |=====

  final def select(name: String): Select =
    (this.symbol.methodMembers ++ this.symbol.fieldMembers).filter(_.name == name) match {
      case sym :: Nil => select(sym)
      case Nil        => report.errorAndAbort(s"No symbols with name $name for $show")
      case syms       =>
        val shown = syms.map { sym =>
          sym.tree match {
            case defDef: DefDef =>
              s"def ${defDef.name}${defDef.paramss.map(_.render).mkString}: ${defDef.returnTpt.tpe.show}"
            case valDef: ValDef =>
              s"val ${valDef.name}: ${valDef.tpt.tpe.show}"
            case tree =>
              s"<unknown> ${sym.name}: ${tree.show}"
          }
        }
        report.errorAndAbort(s"Many symbols with name $name for $show:${shown.map(s => s"\n    - $s").mkString}")
    }

  final def removeInline: Term = this match
    case Inlined(_, _, term) => term.removeInline
    case Block(Nil, term)    => term.removeInline
    case _                   => this

  final def removeInlineAndTyped: Term = this match
    case Inlined(_, _, term) => term.removeInlineAndTyped
    case Block(Nil, term)    => term.removeInlineAndTyped
    case Typed(term, _)      => term.removeInlineAndTyped
    case _                   => this

}
object Term {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Term): Term =
    unwrap match
      case unwrap: quotes.reflect.Ref         => Ref.wrap(unwrap)
      case unwrap: quotes.reflect.Literal     => Literal.wrap(unwrap)
      case unwrap: quotes.reflect.This        => This.wrap(unwrap)
      case unwrap: quotes.reflect.New         => New.wrap(unwrap)
      case unwrap: quotes.reflect.NamedArg    => NamedArg.wrap(unwrap)
      case unwrap: quotes.reflect.Apply       => Apply.wrap(unwrap)
      case unwrap: quotes.reflect.TypeApply   => TypeApply.wrap(unwrap)
      case unwrap: quotes.reflect.Super       => Super.wrap(unwrap)
      case unwrap: quotes.reflect.Assign      => Assign.wrap(unwrap)
      case unwrap: quotes.reflect.Block       => Block.wrap(unwrap)
      case unwrap: quotes.reflect.Closure     => Closure.wrap(unwrap)
      case unwrap: quotes.reflect.If          => If.wrap(unwrap)
      case unwrap: quotes.reflect.Match       => Match.wrap(unwrap)
      case unwrap: quotes.reflect.SummonFrom  => SummonFrom.wrap(unwrap)
      case unwrap: quotes.reflect.Try         => Try.wrap(unwrap)
      case unwrap: quotes.reflect.Return      => Return.wrap(unwrap)
      case unwrap: quotes.reflect.Repeated    => Repeated.wrap(unwrap)
      case unwrap: quotes.reflect.Inlined     => Inlined.wrap(unwrap)
      case unwrap: quotes.reflect.SelectOuter => SelectOuter.wrap(unwrap)
      case unwrap: quotes.reflect.While       => While.wrap(unwrap)
      case unwrap: quotes.reflect.Typed       => Typed.wrap(unwrap)
      case _                                  => throw UnknownCase("Term", unwrap)

  def companion(using quotes: Quotes): TermCompanion = TermCompanion(using quotes)
  given Quotes => Conversion[Term.type, TermCompanion] = _.companion

}

sealed trait Ref extends Term {
  override type This <: Ref
  override val unwrap: quotes.reflect.Ref
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Ref = unwrap.asInstanceOf[newQuotes.reflect.Ref]

  // =====| Added |=====

  def name: String

}
object Ref {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Ref): Ref =
    unwrap match
      case unwrap: quotes.reflect.Ident  => Ident.wrap(unwrap)
      case unwrap: quotes.reflect.Select => Select.wrap(unwrap)
      case _                             => throw UnknownCase("Ref", unwrap)

  def companion(using quotes: Quotes): RefCompanion = RefCompanion(using quotes)
  given Quotes => Conversion[Ref.type, RefCompanion] = _.companion

}

final class Ident(val quotes: Quotes)(val unwrap: quotes.reflect.Ident) extends Ref {
  override type This <: Ident
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Ident = unwrap.asInstanceOf[newQuotes.reflect.Ident]

  /** Name of this `Ident` */
  def name: String = this.unwrap.name

}
object Ident {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Ident): Ident =
    new Ident(quotes)(unwrap)

  def companion(using quotes: Quotes): IdentCompanion = IdentCompanion(using quotes)
  given Quotes => Conversion[Ident.type, IdentCompanion] = _.companion

  def unapply(tree: Ident): Some[String] = {
    given q: Quotes = tree.quotes
    q.reflect.Ident.unapply(tree.unwrapWithin)
  }

}

opaque type Wildcard <: Ident = Ident
object Wildcard {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Wildcard): Wildcard =
    Ident.wrap(unwrap)

  def companion(using quotes: Quotes): WildcardCompanion = WildcardCompanion(using quotes)
  given Quotes => Conversion[Wildcard.type, WildcardCompanion] = _.companion

  given WildcardTypeTest: TypeTest[Tree, Wildcard] =
    new TypeTest[Tree, Wildcard] {
      override def unapply(x: Tree): Option[x.type & Wildcard] =
        Option.when(x.quotes.reflect.WildcardTypeTest.unapply(x.unwrap).nonEmpty)(x.asInstanceOf[x.type & Wildcard])
    }

  def unapply(x: Wildcard): true =
    true

}

final class Select(val quotes: Quotes)(val unwrap: quotes.reflect.Select) extends Ref {
  override type This <: Select
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Select = unwrap.asInstanceOf[newQuotes.reflect.Select]

  /** Qualifier of the `qualifier.name` */
  def qualifier: Term = Term.wrap(this.unwrap.qualifier)

  /** Name of this `Select` */
  def name: String = this.unwrap.name

  /** Signature of this method */
  def signature: Option[Signature] = this.unwrap.signature.map(Signature.wrap(_))

}
object Select {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Select): Select =
    new Select(quotes)(unwrap)

  def companion(using quotes: Quotes): SelectCompanion = SelectCompanion(using quotes)
  given Quotes => Conversion[Select.type, SelectCompanion] = _.companion

  /** Matches `<qualifier: Term>.<name: String>` */
  def unapply(x: Select): (Term, String) = {
    given q: Quotes = x.quotes
    val (qual, name) = q.reflect.Select.unapply(x.unwrapWithin)
    (Term.wrap(qual), name)
  }

}

final class Literal(val quotes: Quotes)(val unwrap: quotes.reflect.Literal) extends Term {
  override type This <: Literal
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Literal = unwrap.asInstanceOf[newQuotes.reflect.Literal]

  /** Value of this literal */
  def constant: Constant = Constant.wrap(this.unwrap.constant)

}
object Literal {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Literal): Literal =
    new Literal(quotes)(unwrap)

  def companion(using quotes: Quotes): LiteralCompanion = LiteralCompanion(using quotes)
  given Quotes => Conversion[Literal.type, LiteralCompanion] = _.companion

  /** Matches a literal constant */
  def unapply(x: Literal): Some[Constant] = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.Literal.unapply(x.unwrapWithin)
    Some(Constant.wrap(unwrap.value))
  }

}

final class This(val quotes: Quotes)(val unwrap: quotes.reflect.This) extends Term {
  override type This <: oxygen.quoted.This
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.This = unwrap.asInstanceOf[newQuotes.reflect.This]

  /**
    * Returns `C` if the underlying tree is of the form `C.this`
    *
    *  Otherwise, return `None`.
    */
  def id: Option[String] = this.unwrap.id

}
object This {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.This): This =
    new This(quotes)(unwrap)

  def companion(using quotes: Quotes): ThisCompanion = ThisCompanion(using quotes)
  given Quotes => Conversion[This.type, ThisCompanion] = _.companion

  /** Matches `this` or `qual.this` and returns the name of `qual` */
  def unapply(x: This): Some[Option[String]] = {
    given q: Quotes = x.quotes
    q.reflect.This.unapply(x.unwrapWithin)
  }

}

final class New(val quotes: Quotes)(val unwrap: quotes.reflect.New) extends Term {
  override type This <: New
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.New = unwrap.asInstanceOf[newQuotes.reflect.New]

  /** The type tree of this `new` */
  def tpt: TypeTree = TypeTree.wrap(this.unwrap.tpt)

}
object New {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.New): New =
    new New(quotes)(unwrap)

  def companion(using quotes: Quotes): NewCompanion = NewCompanion(using quotes)
  given Quotes => Conversion[New.type, NewCompanion] = _.companion

  /** Matches `new <tpt: TypeTree>` */
  def unapply(x: New): Some[TypeTree] = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.New.unapply(x.unwrapWithin)
    Some(TypeTree.wrap(unwrap.value))
  }

}

final class NamedArg(val quotes: Quotes)(val unwrap: quotes.reflect.NamedArg) extends Term {
  override type This <: NamedArg
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.NamedArg = unwrap.asInstanceOf[newQuotes.reflect.NamedArg]

  /** The name part of `name = arg` */
  def name: String = this.unwrap.name

  /** The argument part of `name = arg` */
  def value: Term = Term.wrap(this.unwrap.value)

}
object NamedArg {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.NamedArg): NamedArg =
    new NamedArg(quotes)(unwrap)

  def companion(using quotes: Quotes): NamedArgCompanion = NamedArgCompanion(using quotes)
  given Quotes => Conversion[NamedArg.type, NamedArgCompanion] = _.companion

  /** Matches a named argument `<name: String> = <value: Term>` */
  def unapply(x: NamedArg): (String, Term) = {
    given q: Quotes = x.quotes
    val (name, arg) = q.reflect.NamedArg.unapply(x.unwrapWithin)
    (name, Term.wrap(arg))
  }

}

final class Apply(val quotes: Quotes)(val unwrap: quotes.reflect.Apply) extends Term {
  override type This <: Apply
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Apply = unwrap.asInstanceOf[newQuotes.reflect.Apply]

  /**
    * The `fun` part of an (using) application like `fun(args)`
    *
    *  It may be a partially applied method:
    *  ```scala
    *  def f(x1: Int)(x2: Int) = ???
    *  f(1)(2)
    *  ```
    *  - `fun` is `f(1)` in the `Apply` of `f(1)(2)`
    *  - `fun` is `f` in the `Apply` of `f(1)`
    */
  def fun: Term = Term.wrap(this.unwrap.fun)

  /**
    * The arguments (implicitly) passed to the method
    *
    *  The `Apply` may be a partially applied method:
    *  ```scala
    *  def f(x1: Int)(x2: Int) = ???
    *  f(1)(2)
    *  ```
    *  - `args` is `(2)` in the `Apply` of `f(1)(2)`
    *  - `args` is `(1)` in the `Apply` of `f(1)`
    */
  def args: List[Term] = this.unwrap.args.map(Term.wrap(_))

}
object Apply {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Apply): Apply =
    new Apply(quotes)(unwrap)

  def companion(using quotes: Quotes): ApplyCompanion = ApplyCompanion(using quotes)
  given Quotes => Conversion[Apply.type, ApplyCompanion] = _.companion

  /** Matches a function application `<fun: Term>(<args: List[Term]>)` */
  def unapply(x: Apply): (Term, List[Term]) = {
    given q: Quotes = x.quotes
    val (fun, args) = q.reflect.Apply.unapply(x.unwrapWithin)
    (Term.wrap(fun), args.map(Term.wrap(_)))
  }

}

final class TypeApply(val quotes: Quotes)(val unwrap: quotes.reflect.TypeApply) extends Term {
  override type This <: TypeApply
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.TypeApply = unwrap.asInstanceOf[newQuotes.reflect.TypeApply]

  /**
    * The `fun` part of an (inferred) type application like `fun[Args]`
    *
    *  It may be a partially applied method:
    *  ```scala
    *  //{
    *  type T
    *  //}
    *  extension (x: Int) def f[T](y: T) = ???
    *  // represented as
    *  // def f(x: Int)[T](y: T) = ???
    *
    *  1.f[Int](2)
    *  // represented as
    *  // f(1)[Int](2)
    *  ```
    *  - `fun` is `f(1)` in the `TypeApply` of `f(1)[Int]`
    */
  def fun: Term = Term.wrap(this.unwrap.fun)

  /**
    * The (inferred) type arguments passed to the method
    *
    *  The `TypeApply` may be a partially applied method:
    *  ```scala
    *  //{
    *  type T
    *  //}
    *  extension (x: Int) def f[T](y: T) = ???
    *  // represented as
    *  // def f(x: Int)[T](y: T) = ???
    *
    *  1.f[Int](2)
    *  // represented as
    *  // f(1)[Int](2)
    *  ```
    *  - `fun` is `[Int]` in the `TypeApply` of `f(1)[Int]`
    */
  def args: List[TypeTree] = this.unwrap.args.map(TypeTree.wrap(_))

}
object TypeApply {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.TypeApply): TypeApply =
    new TypeApply(quotes)(unwrap)

  def companion(using quotes: Quotes): TypeApplyCompanion = TypeApplyCompanion(using quotes)
  given Quotes => Conversion[TypeApply.type, TypeApplyCompanion] = _.companion

  /** Matches a function type application `<fun: Term>[<args: List[TypeTree]>]` */
  def unapply(x: TypeApply): (Term, List[TypeTree]) = {
    given q: Quotes = x.quotes
    val (fun, args) = q.reflect.TypeApply.unapply(x.unwrapWithin)
    (Term.wrap(fun), args.map(TypeTree.wrap(_)))
  }

}

final class Super(val quotes: Quotes)(val unwrap: quotes.reflect.Super) extends Term {
  override type This <: Super
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Super = unwrap.asInstanceOf[newQuotes.reflect.Super]

  def qualifier: Term = Term.wrap(this.unwrap.qualifier)

  def id: Option[String] = this.unwrap.id

  def idPos: Position = Position.wrap(this.unwrap.idPos)

}
object Super {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Super): Super =
    new Super(quotes)(unwrap)

  def companion(using quotes: Quotes): SuperCompanion = SuperCompanion(using quotes)
  given Quotes => Conversion[Super.type, SuperCompanion] = _.companion

  /** Matches a `<qualifier: Term>.super[<id: Option[Id]>` */
  def unapply(x: Super): (Term, Option[String]) = {
    given q: Quotes = x.quotes
    val (qual, mix) = q.reflect.Super.unapply(x.unwrapWithin)
    (Term.wrap(qual), mix)
  }

}

final class Assign(val quotes: Quotes)(val unwrap: quotes.reflect.Assign) extends Term {
  override type This <: Assign
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Assign = unwrap.asInstanceOf[newQuotes.reflect.Assign]

  def lhs: Term = Term.wrap(this.unwrap.lhs)

  def rhs: Term = Term.wrap(this.unwrap.rhs)

}
object Assign {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Assign): Assign =
    new Assign(quotes)(unwrap)

  def companion(using quotes: Quotes): AssignCompanion = AssignCompanion(using quotes)
  given Quotes => Conversion[Assign.type, AssignCompanion] = _.companion

  /** Matches an assignment `<lhs: Term> = <rhs: Term>` */
  def unapply(x: Assign): (Term, Term) = {
    given q: Quotes = x.quotes
    val (lhs, rhs) = q.reflect.Assign.unapply(x.unwrapWithin)
    (Term.wrap(lhs), Term.wrap(rhs))
  }

}

final class Block(val quotes: Quotes)(val unwrap: quotes.reflect.Block) extends Term {
  override type This <: Block
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Block = unwrap.asInstanceOf[newQuotes.reflect.Block]

  def statements: List[Statement] = this.unwrap.statements.map(Statement.wrap(_))

  def expr: Term = Term.wrap(this.unwrap.expr)

}
object Block {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Block): Block =
    new Block(quotes)(unwrap)

  def companion(using quotes: Quotes): BlockCompanion = BlockCompanion(using quotes)
  given Quotes => Conversion[Block.type, BlockCompanion] = _.companion

  /** Matches a block `{ <statements: List[Statement]>; <expr: Term> }` */
  def unapply(x: Block): (List[Statement], Term) = {
    given q: Quotes = x.quotes
    val (stats, expr) = q.reflect.Block.unapply(x.unwrapWithin)
    (stats.map(Statement.wrap(_)), Term.wrap(expr))
  }

}

final class Closure(val quotes: Quotes)(val unwrap: quotes.reflect.Closure) extends Term {
  override type This <: Closure
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Closure = unwrap.asInstanceOf[newQuotes.reflect.Closure]

  def meth: Term = Term.wrap(this.unwrap.meth)

  def tpeOpt: Option[TypeRepr] = this.unwrap.tpeOpt.map(TypeRepr.wrap(_))

}
object Closure {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Closure): Closure =
    new Closure(quotes)(unwrap)

  def companion(using quotes: Quotes): ClosureCompanion = ClosureCompanion(using quotes)
  given Quotes => Conversion[Closure.type, ClosureCompanion] = _.companion

  def unapply(x: Closure): (Term, Option[TypeRepr]) = {
    given q: Quotes = x.quotes
    val (meth, tpeOpt) = q.reflect.Closure.unapply(x.unwrapWithin)
    (Term.wrap(meth), tpeOpt.map(TypeRepr.wrap(_)))
  }

}

final class If(val quotes: Quotes)(val unwrap: quotes.reflect.If) extends Term {
  override type This <: If
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.If = unwrap.asInstanceOf[newQuotes.reflect.If]

  def cond: Term = Term.wrap(this.unwrap.cond)

  def thenp: Term = Term.wrap(this.unwrap.thenp)

  def elsep: Term = Term.wrap(this.unwrap.elsep)

  def isInline: Boolean = this.unwrap.isInline

}
object If {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.If): If =
    new If(quotes)(unwrap)

  def companion(using quotes: Quotes): IfCompanion = IfCompanion(using quotes)
  given Quotes => Conversion[If.type, IfCompanion] = _.companion

  /** Matches an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
  def unapply(tree: If): (Term, Term, Term) = {
    given q: Quotes = tree.quotes
    val (cond, thenp, elsep) = q.reflect.If.unapply(tree.unwrapWithin)
    (Term.wrap(cond), Term.wrap(thenp), Term.wrap(elsep))
  }

}

final class Match(val quotes: Quotes)(val unwrap: quotes.reflect.Match) extends Term {
  override type This <: Match
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Match = unwrap.asInstanceOf[newQuotes.reflect.Match]

  def scrutinee: Term = Term.wrap(this.unwrap.scrutinee)

  def cases: List[CaseDef] = this.unwrap.cases.map(CaseDef.wrap(_))

  def isInline: Boolean = this.unwrap.isInline

}
object Match {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Match): Match =
    new Match(quotes)(unwrap)

  def companion(using quotes: Quotes): MatchCompanion = MatchCompanion(using quotes)
  given Quotes => Conversion[Match.type, MatchCompanion] = _.companion

  /** Matches a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
  def unapply(x: Match): (Term, List[CaseDef]) = {
    given q: Quotes = x.quotes
    val (scrutinee, cases) = q.reflect.Match.unapply(x.unwrapWithin)
    (Term.wrap(scrutinee), cases.map(CaseDef.wrap(_)))
  }

}

final class SummonFrom(val quotes: Quotes)(val unwrap: quotes.reflect.SummonFrom) extends Term {
  override type This <: SummonFrom
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.SummonFrom = unwrap.asInstanceOf[newQuotes.reflect.SummonFrom]

  def cases: List[CaseDef] = this.unwrap.cases.map(CaseDef.wrap(_))

}
object SummonFrom {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.SummonFrom): SummonFrom =
    new SummonFrom(quotes)(unwrap)

  def companion(using quotes: Quotes): SummonFromCompanion = SummonFromCompanion(using quotes)
  given Quotes => Conversion[SummonFrom.type, SummonFromCompanion] = _.companion

  /** Matches a pattern match `given match { <cases: List[CaseDef]> }` */
  def unapply(x: SummonFrom): Some[List[CaseDef]] = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.SummonFrom.unapply(x.unwrapWithin)
    Some(unwrap.value.map(CaseDef.wrap(_)))
  }

}

final class Try(val quotes: Quotes)(val unwrap: quotes.reflect.Try) extends Term {
  override type This <: Try
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Try = unwrap.asInstanceOf[newQuotes.reflect.Try]

  def body: Term = Term.wrap(this.unwrap.body)

  def cases: List[CaseDef] = this.unwrap.cases.map(CaseDef.wrap(_))

  def finalizer: Option[Term] = this.unwrap.finalizer.map(Term.wrap(_))

}
object Try {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Try): Try =
    new Try(quotes)(unwrap)

  def companion(using quotes: Quotes): TryCompanion = TryCompanion(using quotes)
  given Quotes => Conversion[Try.type, TryCompanion] = _.companion

  /** Matches a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
  def unapply(x: Try): (Term, List[CaseDef], Option[Term]) = {
    given q: Quotes = x.quotes
    val (body, cases, finalizer) = q.reflect.Try.unapply(x.unwrapWithin)
    (Term.wrap(body), cases.map(CaseDef.wrap(_)), finalizer.map(Term.wrap(_)))
  }

}

final class Return(val quotes: Quotes)(val unwrap: quotes.reflect.Return) extends Term {
  override type This <: Return
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Return = unwrap.asInstanceOf[newQuotes.reflect.Return]

  def expr: Term = Term.wrap(this.unwrap.expr)

  def from: Symbol = Symbol.wrap(this.unwrap.from)

}
object Return {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Return): Return =
    new Return(quotes)(unwrap)

  def companion(using quotes: Quotes): ReturnCompanion = ReturnCompanion(using quotes)
  given Quotes => Conversion[Return.type, ReturnCompanion] = _.companion

  /** Matches `return <expr: Term>` and extracts the expression and symbol of the method */
  def unapply(x: Return): (Term, Symbol) = {
    given q: Quotes = x.quotes
    val (expr, from) = q.reflect.Return.unapply(x.unwrapWithin)
    (Term.wrap(expr), Symbol.wrap(from))
  }

}

final class Repeated(val quotes: Quotes)(val unwrap: quotes.reflect.Repeated) extends Term {
  override type This <: Repeated
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Repeated = unwrap.asInstanceOf[newQuotes.reflect.Repeated]

  def elems: List[Term] = this.unwrap.elems.map(Term.wrap(_))

  def elemtpt: TypeTree = TypeTree.wrap(this.unwrap.elemtpt)

}
object Repeated {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Repeated): Repeated =
    new Repeated(quotes)(unwrap)

  def companion(using quotes: Quotes): RepeatedCompanion = RepeatedCompanion(using quotes)
  given Quotes => Conversion[Repeated.type, RepeatedCompanion] = _.companion

  /** Matches a literal sequence of elements */
  def unapply(x: Repeated): (List[Term], TypeTree) = {
    given q: Quotes = x.quotes
    val (elems, tpt) = q.reflect.Repeated.unapply(x.unwrapWithin)
    (elems.map(Term.wrap(_)), TypeTree.wrap(tpt))
  }

  object ignoreTyped {

    def unapply(term: Term): Option[(List[Term], TypeTree)] = term match
      case Typed(term: Repeated, _) => Some(Repeated.unapply(term))
      case term: Repeated           => Some(Repeated.unapply(term))
      case _                        => None

  }

}

final class Inlined(val quotes: Quotes)(val unwrap: quotes.reflect.Inlined) extends Term {
  override type This <: Inlined
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Inlined = unwrap.asInstanceOf[newQuotes.reflect.Inlined]

  def call: Option[Term | TypeTree] = this.unwrap.call.map(TermOrTypeTree.wrap(_))

  def bindings: List[Definition] = this.unwrap.bindings.map(Definition.wrap(_))

  def body: Term = Term.wrap(this.unwrap.body)

}
object Inlined {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Inlined): Inlined =
    new Inlined(quotes)(unwrap)

  def companion(using quotes: Quotes): InlinedCompanion = InlinedCompanion(using quotes)
  given Quotes => Conversion[Inlined.type, InlinedCompanion] = _.companion

  def unapply(x: Inlined): (Option[Term | TypeTree], List[Definition], Term) = {
    given q: Quotes = x.quotes
    val (call, bindings, expansion) = q.reflect.Inlined.unapply(x.unwrapWithin)
    (
      call.map(TermOrTypeTree.wrap(_)),
      bindings.map(Definition.wrap(_)),
      Term.wrap(expansion),
    )
  }

}

final class SelectOuter(val quotes: Quotes)(val unwrap: quotes.reflect.SelectOuter) extends Term {
  override type This <: SelectOuter
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.SelectOuter = unwrap.asInstanceOf[newQuotes.reflect.SelectOuter]

  def qualifier: Term = Term.wrap(this.unwrap.qualifier)

  def name: String = this.unwrap.name

  def level: Int = this.unwrap.level

}
object SelectOuter {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.SelectOuter): SelectOuter =
    new SelectOuter(quotes)(unwrap)

  def companion(using quotes: Quotes): SelectOuterCompanion = SelectOuterCompanion(using quotes)
  given Quotes => Conversion[SelectOuter.type, SelectOuterCompanion] = _.companion

  def unapply(x: SelectOuter): (Term, String, Int) = {
    given q: Quotes = x.quotes
    val (qualifier, name, level) = q.reflect.SelectOuter.unapply(x.unwrapWithin)
    (Term.wrap(qualifier), name, level)
  }

}

final class While(val quotes: Quotes)(val unwrap: quotes.reflect.While) extends Term {
  override type This <: While
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.While = unwrap.asInstanceOf[newQuotes.reflect.While]

  def cond: Term = Term.wrap(this.unwrap.cond)

  def body: Term = Term.wrap(this.unwrap.body)

}
object While {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.While): While =
    new While(quotes)(unwrap)

  def companion(using quotes: Quotes): WhileCompanion = WhileCompanion(using quotes)
  given Quotes => Conversion[While.type, WhileCompanion] = _.companion

  /** Extractor for while loops. Matches `while (<cond>) <body>` and returns (<cond>, <body>) */
  def unapply(x: While): (Term, Term) = {
    given q: Quotes = x.quotes
    val (cond, body) = q.reflect.While.unapply(x.unwrapWithin)
    (Term.wrap(cond), Term.wrap(body))
  }

}

final class Typed(val quotes: Quotes)(val unwrap: quotes.reflect.Typed) extends Term, TypedOrTest {
  override type This <: Typed
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Typed = unwrap.asInstanceOf[newQuotes.reflect.Typed]

  def expr: Term = Term.wrap(this.unwrap.expr)

}
object Typed {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Typed): Typed =
    new Typed(quotes)(unwrap)

  def companion(using quotes: Quotes): TypedCompanion = TypedCompanion(using quotes)
  given Quotes => Conversion[Typed.type, TypedCompanion] = _.companion

  /** Matches `<expr: Term>: <tpt: TypeTree>` */
  def unapply(x: Typed): (Term, TypeTree) = {
    given q: Quotes = x.quotes
    val (expr, tpt) = q.reflect.Typed.unapply(x.unwrapWithin)
    (Term.wrap(expr), TypeTree.wrap(tpt))
  }

}

sealed trait TypedOrTest extends Tree {
  override type This <: TypedOrTest
  override val unwrap: quotes.reflect.TypedOrTest
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.TypedOrTest = unwrap.asInstanceOf[newQuotes.reflect.TypedOrTest]

  def tree: Tree = Tree.wrap(this.unwrap.tree)

  def tpt: TypeTree = TypeTree.wrap(this.unwrap.tpt)

}
object TypedOrTest {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.TypedOrTest): TypedOrTest =
    unwrap match
      case unwrap: quotes.reflect.Typed => Typed.wrap(unwrap)
      case _                            => throw UnknownCase("TypedOrTest", unwrap)

  def companion(using quotes: Quotes): TypedOrTestCompanion = TypedOrTestCompanion(using quotes)
  given Quotes => Conversion[TypedOrTest.type, TypedOrTestCompanion] = _.companion

  /** Matches `<expr: Tree>: <tpt: TypeTree>` */
  def unapply(x: TypedOrTest): (Tree, TypeTree) = {
    given q: Quotes = x.quotes
    val (tree, tpt) = q.reflect.TypedOrTest.unapply(x.unwrapWithin)
    (Tree.wrap(tree), TypeTree.wrap(tpt))
  }

}

final class Bind(val quotes: Quotes)(val unwrap: quotes.reflect.Bind) extends Tree {
  override type This <: Bind
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Bind = unwrap.asInstanceOf[newQuotes.reflect.Bind]

  def name: String = this.unwrap.name

  def pattern: Tree = Tree.wrap(this.unwrap.pattern)

}
object Bind {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Bind): Bind =
    new Bind(quotes)(unwrap)

  def companion(using quotes: Quotes): BindCompanion = BindCompanion(using quotes)
  given Quotes => Conversion[Bind.type, BindCompanion] = _.companion

  def unapply(pattern: Bind): (String, Tree) = {
    given q: Quotes = pattern.quotes
    val (name, pat) = q.reflect.Bind.unapply(pattern.unwrapWithin)
    (name, Tree.wrap(pat))
  }

}

final class Unapply(val quotes: Quotes)(val unwrap: quotes.reflect.Unapply) extends Tree {
  override type This <: Unapply
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Unapply = unwrap.asInstanceOf[newQuotes.reflect.Unapply]

  /**
    * The extractor function of the pattern.
    *
    *  It may be a reference to the `unapply` method of the pattern or may be a
    *  partially applied tree containing type parameters and leading given parameters.
    */
  def fun: Term = Term.wrap(this.unwrap.fun)

  /** Training implicit parameters of the `unapply` method */
  def implicits: List[Term] = this.unwrap.implicits.map(Term.wrap(_))

  /** List of nested patterns */
  def patterns: List[Tree] = this.unwrap.patterns.map(Tree.wrap(_))

}
object Unapply {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Unapply): Unapply =
    new Unapply(quotes)(unwrap)

  def companion(using quotes: Quotes): UnapplyCompanion = UnapplyCompanion(using quotes)
  given Quotes => Conversion[Unapply.type, UnapplyCompanion] = _.companion

  /** Matches an `Unapply(fun, implicits, patterns)` tree representing a pattern `<fun>(<patterns*>)(using <implicits*>)` */
  def unapply(x: Unapply): (Term, List[Term], List[Tree]) = {
    given q: Quotes = x.quotes
    val (fun, implicits, patterns) = q.reflect.Unapply.unapply(x.unwrapWithin)
    (Term.wrap(fun), implicits.map(Term.wrap(_)), patterns.map(Tree.wrap(_)))
  }

}

final class Alternatives(val quotes: Quotes)(val unwrap: quotes.reflect.Alternatives) extends Tree {
  override type This <: Alternatives
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Alternatives = unwrap.asInstanceOf[newQuotes.reflect.Alternatives]

  def patterns: List[Tree] = this.unwrap.patterns.map(Tree.wrap(_))

}
object Alternatives {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Alternatives): Alternatives =
    new Alternatives(quotes)(unwrap)

  def companion(using quotes: Quotes): AlternativesCompanion = AlternativesCompanion(using quotes)
  given Quotes => Conversion[Alternatives.type, AlternativesCompanion] = _.companion

  def unapply(x: Alternatives): Some[List[Tree]] = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.Alternatives.unapply(x.unwrapWithin)
    Some(unwrap.value.map(Tree.wrap(_)))
  }

}

final class CaseDef(val quotes: Quotes)(val unwrap: quotes.reflect.CaseDef) extends Tree {
  override type This <: CaseDef
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.CaseDef = unwrap.asInstanceOf[newQuotes.reflect.CaseDef]

  def pattern: Tree = Tree.wrap(this.unwrap.pattern)

  def guard: Option[Term] = this.unwrap.guard.map(Term.wrap(_))

  def rhs: Term = Term.wrap(this.unwrap.rhs)

}
object CaseDef {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.CaseDef): CaseDef =
    new CaseDef(quotes)(unwrap)

  def companion(using quotes: Quotes): CaseDefCompanion = CaseDefCompanion(using quotes)
  given Quotes => Conversion[CaseDef.type, CaseDefCompanion] = _.companion

  def unapply(x: CaseDef): (Tree, Option[Term], Term) = {
    given q: Quotes = x.quotes
    val (pattern, guard, rhs) = q.reflect.CaseDef.unapply(x.unwrapWithin)
    (Tree.wrap(pattern), guard.map(Term.wrap(_)), Term.wrap(rhs))
  }

}

final class TypeCaseDef(val quotes: Quotes)(val unwrap: quotes.reflect.TypeCaseDef) extends Tree {
  override type This <: TypeCaseDef
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.TypeCaseDef = unwrap.asInstanceOf[newQuotes.reflect.TypeCaseDef]

  def pattern: TypeTree = TypeTree.wrap(this.unwrap.pattern)

  def rhs: TypeTree = TypeTree.wrap(this.unwrap.rhs)

}
object TypeCaseDef {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.TypeCaseDef): TypeCaseDef =
    new TypeCaseDef(quotes)(unwrap)

  def companion(using quotes: Quotes): TypeCaseDefCompanion = TypeCaseDefCompanion(using quotes)
  given Quotes => Conversion[TypeCaseDef.type, TypeCaseDefCompanion] = _.companion

  def unapply(tree: TypeCaseDef): (TypeTree, TypeTree) = {
    given q: Quotes = tree.quotes
    val (pattern, rhs) = q.reflect.TypeCaseDef.unapply(tree.unwrapWithin)
    (TypeTree.wrap(pattern), TypeTree.wrap(rhs))
  }

}

sealed trait TypeTree extends Tree {
  override type This <: TypeTree
  override val unwrap: quotes.reflect.TypeTree
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.TypeTree = unwrap.asInstanceOf[newQuotes.reflect.TypeTree]

  /** TypeRepr of this type tree */
  def tpe: TypeRepr = TypeRepr.wrap(this.unwrap.tpe)

}
object TypeTree {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.TypeTree): TypeTree =
    unwrap match
      case unwrap: quotes.reflect.Inferred       => Inferred.wrap(unwrap)
      case unwrap: quotes.reflect.TypeIdent      => TypeIdent.wrap(unwrap)
      case unwrap: quotes.reflect.TypeSelect     => TypeSelect.wrap(unwrap)
      case unwrap: quotes.reflect.TypeProjection => TypeProjection.wrap(unwrap)
      case unwrap: quotes.reflect.Singleton      => Singleton.wrap(unwrap)
      case unwrap: quotes.reflect.Refined        => Refined.wrap(unwrap)
      case unwrap: quotes.reflect.Applied        => Applied.wrap(unwrap)
      case unwrap: quotes.reflect.Annotated      => Annotated.wrap(unwrap)
      case unwrap: quotes.reflect.MatchTypeTree  => MatchTypeTree.wrap(unwrap)
      case unwrap: quotes.reflect.ByName         => ByName.wrap(unwrap)
      case unwrap: quotes.reflect.LambdaTypeTree => LambdaTypeTree.wrap(unwrap)
      case unwrap: quotes.reflect.TypeBind       => TypeBind.wrap(unwrap)
      case unwrap: quotes.reflect.TypeBlock      => TypeBlock.wrap(unwrap)
      case _                                     => throw UnknownCase("TypeTree", unwrap)

  def companion(using quotes: Quotes): TypeTreeCompanion = TypeTreeCompanion(using quotes)
  given Quotes => Conversion[TypeTree.type, TypeTreeCompanion] = _.companion

}

final class Inferred(val quotes: Quotes)(val unwrap: quotes.reflect.Inferred) extends TypeTree {
  override type This <: Inferred
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Inferred = unwrap.asInstanceOf[newQuotes.reflect.Inferred]

}
object Inferred {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Inferred): Inferred =
    new Inferred(quotes)(unwrap)

  def companion(using quotes: Quotes): InferredCompanion = InferredCompanion(using quotes)
  given Quotes => Conversion[Inferred.type, InferredCompanion] = _.companion

  def unapply(x: Inferred): true = true

}

final class TypeIdent(val quotes: Quotes)(val unwrap: quotes.reflect.TypeIdent) extends TypeTree {
  override type This <: TypeIdent
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.TypeIdent = unwrap.asInstanceOf[newQuotes.reflect.TypeIdent]

  def name: String = this.unwrap.name

}
object TypeIdent {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.TypeIdent): TypeIdent =
    new TypeIdent(quotes)(unwrap)

  def companion(using quotes: Quotes): TypeIdentCompanion = TypeIdentCompanion(using quotes)
  given Quotes => Conversion[TypeIdent.type, TypeIdentCompanion] = _.companion

  def unapply(x: TypeIdent): Some[String] = {
    given q: Quotes = x.quotes
    q.reflect.TypeIdent.unapply(x.unwrapWithin)
  }

}

final class TypeSelect(val quotes: Quotes)(val unwrap: quotes.reflect.TypeSelect) extends TypeTree {
  override type This <: TypeSelect
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.TypeSelect = unwrap.asInstanceOf[newQuotes.reflect.TypeSelect]

  def qualifier: Term = Term.wrap(this.unwrap.qualifier)

  def name: String = this.unwrap.name

}
object TypeSelect {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.TypeSelect): TypeSelect =
    new TypeSelect(quotes)(unwrap)

  def companion(using quotes: Quotes): TypeSelectCompanion = TypeSelectCompanion(using quotes)
  given Quotes => Conversion[TypeSelect.type, TypeSelectCompanion] = _.companion

  def unapply(x: TypeSelect): (Term, String) = {
    given q: Quotes = x.quotes
    val (qual, name) = q.reflect.TypeSelect.unapply(x.unwrapWithin)
    (Term.wrap(qual), name)
  }

}

final class TypeProjection(val quotes: Quotes)(val unwrap: quotes.reflect.TypeProjection) extends TypeTree {
  override type This <: TypeProjection
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.TypeProjection = unwrap.asInstanceOf[newQuotes.reflect.TypeProjection]

  def qualifier: TypeTree = TypeTree.wrap(this.unwrap.qualifier)

  def name: String = this.unwrap.name

}
object TypeProjection {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.TypeProjection): TypeProjection =
    new TypeProjection(quotes)(unwrap)

  def companion(using quotes: Quotes): TypeProjectionCompanion = TypeProjectionCompanion(using quotes)
  given Quotes => Conversion[TypeProjection.type, TypeProjectionCompanion] = _.companion

  def unapply(x: TypeProjection): (TypeTree, String) = {
    given q: Quotes = x.quotes
    val (qual, name) = q.reflect.TypeProjection.unapply(x.unwrapWithin)
    (TypeTree.wrap(qual), name)
  }

}

final class Singleton(val quotes: Quotes)(val unwrap: quotes.reflect.Singleton) extends TypeTree {
  override type This <: Singleton
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Singleton = unwrap.asInstanceOf[newQuotes.reflect.Singleton]

  def ref: Term = Term.wrap(this.unwrap.ref)

}
object Singleton {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Singleton): Singleton =
    new Singleton(quotes)(unwrap)

  def companion(using quotes: Quotes): SingletonCompanion = SingletonCompanion(using quotes)
  given Quotes => Conversion[Singleton.type, SingletonCompanion] = _.companion

  def unapply(x: Singleton): Some[Term] = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.Singleton.unapply(x.unwrapWithin)
    Some(Term.wrap(unwrap.value))
  }

}

final class Refined(val quotes: Quotes)(val unwrap: quotes.reflect.Refined) extends TypeTree {
  override type This <: Refined
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Refined = unwrap.asInstanceOf[newQuotes.reflect.Refined]

  def tpt: TypeTree = TypeTree.wrap(this.unwrap.tpt)

  def refinements: List[Definition] = this.unwrap.refinements.map(Definition.wrap(_))

}
object Refined {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Refined): Refined =
    new Refined(quotes)(unwrap)

  def companion(using quotes: Quotes): RefinedCompanion = RefinedCompanion(using quotes)
  given Quotes => Conversion[Refined.type, RefinedCompanion] = _.companion

  def unapply(x: Refined): (TypeTree, List[Definition]) = {
    given q: Quotes = x.quotes
    val (tpt, refinements) = q.reflect.Refined.unapply(x.unwrapWithin)
    (TypeTree.wrap(tpt), refinements.map(Definition.wrap(_)))
  }

}

final class Applied(val quotes: Quotes)(val unwrap: quotes.reflect.Applied) extends TypeTree {
  override type This <: Applied
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Applied = unwrap.asInstanceOf[newQuotes.reflect.Applied]

  def tpt: TypeTree = TypeTree.wrap(this.unwrap.tpt)

  def args: List[TypeTree | TypeBoundsTree] = this.unwrap.args.map(TypeTreeOrTypeBoundsTree.wrap(_))

}
object Applied {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Applied): Applied =
    new Applied(quotes)(unwrap)

  def companion(using quotes: Quotes): AppliedCompanion = AppliedCompanion(using quotes)
  given Quotes => Conversion[Applied.type, AppliedCompanion] = _.companion

  def unapply(x: Applied): (TypeTree, List[TypeTree | TypeBoundsTree]) = {
    given q: Quotes = x.quotes
    val (tpt, args) = q.reflect.Applied.unapply(x.unwrapWithin)
    (TypeTree.wrap(tpt), args.map(TypeTreeOrTypeBoundsTree.wrap(_)))
  }

}

final class Annotated(val quotes: Quotes)(val unwrap: quotes.reflect.Annotated) extends TypeTree {
  override type This <: Annotated
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Annotated = unwrap.asInstanceOf[newQuotes.reflect.Annotated]

  def arg: TypeTree = TypeTree.wrap(this.unwrap.arg)

  def annotation: Term = Term.wrap(this.unwrap.annotation)

}
object Annotated {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Annotated): Annotated =
    new Annotated(quotes)(unwrap)

  def companion(using quotes: Quotes): AnnotatedCompanion = AnnotatedCompanion(using quotes)
  given Quotes => Conversion[Annotated.type, AnnotatedCompanion] = _.companion

  def unapply(x: Annotated): (TypeTree, Term) = {
    given q: Quotes = x.quotes
    val (arg, annotation) = q.reflect.Annotated.unapply(x.unwrapWithin)
    (TypeTree.wrap(arg), Term.wrap(annotation))
  }

}

final class MatchTypeTree(val quotes: Quotes)(val unwrap: quotes.reflect.MatchTypeTree) extends TypeTree {
  override type This <: MatchTypeTree
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.MatchTypeTree = unwrap.asInstanceOf[newQuotes.reflect.MatchTypeTree]

  def bound: Option[TypeTree] = this.unwrap.bound.map(TypeTree.wrap(_))

  def selector: TypeTree = TypeTree.wrap(this.unwrap.selector)

  def cases: List[TypeCaseDef] = this.unwrap.cases.map(TypeCaseDef.wrap(_))

}
object MatchTypeTree {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.MatchTypeTree): MatchTypeTree =
    new MatchTypeTree(quotes)(unwrap)

  def companion(using quotes: Quotes): MatchTypeTreeCompanion = MatchTypeTreeCompanion(using quotes)
  given Quotes => Conversion[MatchTypeTree.type, MatchTypeTreeCompanion] = _.companion

  def unapply(x: MatchTypeTree): (Option[TypeTree], TypeTree, List[TypeCaseDef]) = {
    given q: Quotes = x.quotes
    val (bound, selector, cases) = q.reflect.MatchTypeTree.unapply(x.unwrapWithin)
    (bound.map(TypeTree.wrap(_)), TypeTree.wrap(selector), cases.map(TypeCaseDef.wrap(_)))
  }

}

final class ByName(val quotes: Quotes)(val unwrap: quotes.reflect.ByName) extends TypeTree {
  override type This <: ByName
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.ByName = unwrap.asInstanceOf[newQuotes.reflect.ByName]

  def result: TypeTree = TypeTree.wrap(this.unwrap.result)

}
object ByName {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.ByName): ByName =
    new ByName(quotes)(unwrap)

  def companion(using quotes: Quotes): ByNameCompanion = ByNameCompanion(using quotes)
  given Quotes => Conversion[ByName.type, ByNameCompanion] = _.companion

  def unapply(x: ByName): Some[TypeTree] = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.ByName.unapply(x.unwrapWithin)
    Some(TypeTree.wrap(unwrap.value))
  }

}

final class LambdaTypeTree(val quotes: Quotes)(val unwrap: quotes.reflect.LambdaTypeTree) extends TypeTree {
  override type This <: LambdaTypeTree
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.LambdaTypeTree = unwrap.asInstanceOf[newQuotes.reflect.LambdaTypeTree]

  def tparams: List[TypeDef] = this.unwrap.tparams.map(TypeDef.wrap(_))

  def body: TypeTree | TypeBoundsTree = TypeTreeOrTypeBoundsTree.wrap(this.unwrap.body)

}
object LambdaTypeTree {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.LambdaTypeTree): LambdaTypeTree =
    new LambdaTypeTree(quotes)(unwrap)

  def companion(using quotes: Quotes): LambdaTypeTreeCompanion = LambdaTypeTreeCompanion(using quotes)
  given Quotes => Conversion[LambdaTypeTree.type, LambdaTypeTreeCompanion] = _.companion

  def unapply(tree: LambdaTypeTree): (List[TypeDef], TypeTree | TypeBoundsTree) = {
    given q: Quotes = tree.quotes
    val (tparams, body) = q.reflect.LambdaTypeTree.unapply(tree.unwrapWithin)
    (tparams.map(TypeDef.wrap(_)), TypeTreeOrTypeBoundsTree.wrap(body))
  }

}

final class TypeBind(val quotes: Quotes)(val unwrap: quotes.reflect.TypeBind) extends TypeTree {
  override type This <: TypeBind
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.TypeBind = unwrap.asInstanceOf[newQuotes.reflect.TypeBind]

  def name: String = this.unwrap.name

  def body: TypeTree | TypeBoundsTree = TypeTreeOrTypeBoundsTree.wrap(this.unwrap.body)

}
object TypeBind {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.TypeBind): TypeBind =
    new TypeBind(quotes)(unwrap)

  def companion(using quotes: Quotes): TypeBindCompanion = TypeBindCompanion(using quotes)
  given Quotes => Conversion[TypeBind.type, TypeBindCompanion] = _.companion

  def unapply(x: TypeBind): (String, TypeTree | TypeBoundsTree) = {
    given q: Quotes = x.quotes
    val (name, body) = q.reflect.TypeBind.unapply(x.unwrapWithin)
    (name, TypeTreeOrTypeBoundsTree.wrap(body))
  }

}

final class TypeBlock(val quotes: Quotes)(val unwrap: quotes.reflect.TypeBlock) extends TypeTree {
  override type This <: TypeBlock
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.TypeBlock = unwrap.asInstanceOf[newQuotes.reflect.TypeBlock]

  def aliases: List[TypeDef] = this.unwrap.aliases.map(TypeDef.wrap(_))

  def tpt: TypeTree = TypeTree.wrap(this.unwrap.tpt)

}
object TypeBlock {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.TypeBlock): TypeBlock =
    new TypeBlock(quotes)(unwrap)

  def companion(using quotes: Quotes): TypeBlockCompanion = TypeBlockCompanion(using quotes)
  given Quotes => Conversion[TypeBlock.type, TypeBlockCompanion] = _.companion

  def unapply(x: TypeBlock): (List[TypeDef], TypeTree) = {
    given q: Quotes = x.quotes
    val (aliases, tpt) = q.reflect.TypeBlock.unapply(x.unwrapWithin)
    (aliases.map(TypeDef.wrap(_)), TypeTree.wrap(tpt))
  }

}

final class TypeBoundsTree(val quotes: Quotes)(val unwrap: quotes.reflect.TypeBoundsTree) extends Tree {
  override type This <: TypeBoundsTree
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.TypeBoundsTree = unwrap.asInstanceOf[newQuotes.reflect.TypeBoundsTree]

  def tpe: TypeBounds = TypeBounds.wrap(this.unwrap.tpe)

  def low: TypeTree = TypeTree.wrap(this.unwrap.low)

  def hi: TypeTree = TypeTree.wrap(this.unwrap.hi)

}
object TypeBoundsTree {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.TypeBoundsTree): TypeBoundsTree =
    new TypeBoundsTree(quotes)(unwrap)

  def companion(using quotes: Quotes): TypeBoundsTreeCompanion = TypeBoundsTreeCompanion(using quotes)
  given Quotes => Conversion[TypeBoundsTree.type, TypeBoundsTreeCompanion] = _.companion

  def unapply(x: TypeBoundsTree): (TypeTree, TypeTree) = {
    given q: Quotes = x.quotes
    val (low, hi) = q.reflect.TypeBoundsTree.unapply(x.unwrapWithin)
    (TypeTree.wrap(low), TypeTree.wrap(hi))
  }

}

final class WildcardTypeTree(val quotes: Quotes)(val unwrap: quotes.reflect.WildcardTypeTree) extends Tree {
  override type This <: WildcardTypeTree
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.WildcardTypeTree = unwrap.asInstanceOf[newQuotes.reflect.WildcardTypeTree]

  def tpe: TypeRepr = TypeRepr.wrap(this.unwrap.tpe)

}
object WildcardTypeTree {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.WildcardTypeTree): WildcardTypeTree =
    new WildcardTypeTree(quotes)(unwrap)

  def companion(using quotes: Quotes): WildcardTypeTreeCompanion = WildcardTypeTreeCompanion(using quotes)
  given Quotes => Conversion[WildcardTypeTree.type, WildcardTypeTreeCompanion] = _.companion

  def unapply(x: WildcardTypeTree): true = true

}

// =====| Added |=====

object TermOrTypeTree {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Tree): Term | TypeTree =
    unwrap match {
      case unwrap: quotes.reflect.Term     => Term.wrap(unwrap)
      case unwrap: quotes.reflect.TypeTree => TypeTree.wrap(unwrap)
      case _                               => throw UnknownCase("TermOrTypeTree", unwrap)
    }

}

object TypeTreeOrTypeBoundsTree {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Tree): TypeTree | TypeBoundsTree =
    unwrap match {
      case unwrap: quotes.reflect.TypeTree       => TypeTree.wrap(unwrap)
      case unwrap: quotes.reflect.TypeBoundsTree => TypeBoundsTree.wrap(unwrap)
      case _                                     => throw UnknownCase("TypeTreeOrTypeBoundsTree", unwrap)
    }

}
