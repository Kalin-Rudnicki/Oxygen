package oxygen.quoted.companion

import oxygen.quoted.*
import scala.annotation.{experimental, unused}
import scala.quoted.*

final class TreeCompanion(using @unused quotes: Quotes) {}

final class PackageClauseCompanion(using quotes: Quotes) {

  /** Create a package clause `package pid { stats }` */
  def apply(pid: Ref, stats: List[Tree]): PackageClause =
    PackageClause.wrap(quotes.reflect.PackageClause.apply(pid.unwrapWithin, stats.map(_.unwrapWithin)))

  /** Copy a package clause `package pid { stats }` */
  def copy(original: Tree)(pid: Ref, stats: List[Tree]): PackageClause =
    PackageClause.wrap(quotes.reflect.PackageClause.copy(original.unwrapWithin)(pid.unwrapWithin, stats.map(_.unwrapWithin)))

}

final class StatementCompanion(using @unused quotes: Quotes) {}

final class ImportCompanion(using quotes: Quotes) {

  /** Create an `Import` with the given qualifier and selectors */
  def apply(expr: Term, selectors: List[Selector]): Import =
    Import.wrap(quotes.reflect.Import.apply(expr.unwrapWithin, selectors.map(_.unwrapWithin)))

  /** Copy an `Import` with the given qualifier and selectors */
  def copy(original: Tree)(expr: Term, selectors: List[Selector]): Import =
    Import.wrap(quotes.reflect.Import.copy(original.unwrapWithin)(expr.unwrapWithin, selectors.map(_.unwrapWithin)))

}

final class ExportCompanion(using @unused quotes: Quotes) {}

final class DefinitionCompanion(using @unused quotes: Quotes) {}

final class ClassDefCompanion(using quotes: Quotes) {

  /**
    * Create a class definition tree
    *
    *  @param cls The class symbol. A new class symbol can be created using `Symbol.newClass`.
    *  @param parents The parents trees class. The trees must align with the parent types of `cls`.
    *                 Parents can be `TypeTree`s if they don't have term parameter,
    *                 otherwise the can be `Term` containing the `New` applied to the parameters of the extended class.
    *  @param body List of members of the class. The members must align with the members of `cls`.
    */
  @experimental
  def apply(cls: Symbol, parents: List[Term | TypeTree], body: List[Statement]): ClassDef =
    ClassDef.wrap(quotes.reflect.ClassDef.apply(cls.unwrapWithin, parents.map(_.unwrapWithin), body.map(_.unwrapWithin)))

  def copy(original: Tree)(name: String, constr: DefDef, parents: List[Term | TypeTree], selfOpt: Option[ValDef], body: List[Statement]): ClassDef =
    ClassDef.wrap(
      quotes.reflect.ClassDef.copy(original.unwrapWithin)(
        name,
        constr.unwrapWithin,
        parents.map(_.unwrapWithin),
        selfOpt.map(_.unwrapWithin),
        body.map(_.unwrapWithin),
      ),
    )

  /**
    * Create the ValDef and ClassDef of a module (equivalent to an `object` declaration in source code).
    *
    *  Equivalent to
    *  ```
    *  def module(module: Symbol, parents: List[Tree], body: List[Statement]): (ValDef, ClassDef) =
    *    val modCls = module.moduleClass
    *    val modClassDef = ClassDef(modCls, parents, body)
    *    val modValDef = ValDef(module, Some(Apply(Select(New(TypeIdent(modCls)), cls.primaryConstructor), Nil)))
    *    List(modValDef, modClassDef)
    *  ```
    *
    *  @param module the module symbol (created using `Symbol.newModule`)
    *  @param parents parents of the module class
    *  @param body body of the module class
    *  @return The module lazy val definition and module class definition.
    *          These should be added one after the other (in that order) in the body of a class or statements of a block.
    *
    *  @syntax markdown
    */
  @experimental
  def module(module: Symbol, parents: List[Term | TypeTree], body: List[Statement]): (ValDef, ClassDef) = {
    val (v, c) = quotes.reflect.ClassDef.module(
      module.unwrapWithin,
      parents.map(_.unwrapWithin),
      body.map(_.unwrapWithin),
    )
    (ValDef.wrap(v), ClassDef.wrap(c))
  }

}

final class TypeDefCompanion(using quotes: Quotes) {

  def apply(symbol: Symbol): TypeDef =
    TypeDef.wrap(quotes.reflect.TypeDef.apply(symbol.unwrapWithin))

  def copy(original: Tree)(name: String, rhs: Tree): TypeDef =
    TypeDef.wrap(quotes.reflect.TypeDef.copy(original.unwrapWithin)(name, rhs.unwrapWithin))

}

final class ValOrDefDefCompanion(using @unused quotes: Quotes) {}

final class DefDefCompanion(using quotes: Quotes) {

  /**
    * Create a method definition `def f[..](...)` with the signature defined in the symbol.
    *
    *  The `rhsFn` is a function that receives references to its parameters, and should return
    *  `Some` containing the implementation of the method, or `None` if the method has no implementation.
    *  Any definition directly inside the implementation should have `symbol` as owner.
    *
    *  Use `Symbol.asQuotes` to create the rhs using quoted code.
    *
    *  See also: `Tree.changeOwner`
    */
  def apply(symbol: Symbol, rhsFn: List[List[Tree]] => Option[Term]): DefDef =
    DefDef.wrap(quotes.reflect.DefDef.apply(symbol.unwrapWithin, xss => rhsFn(xss.map(_.map(Tree.wrap))).map(_.unwrapWithin)))

  def copy(original: Tree)(name: String, paramss: List[ParamClause], tpt: TypeTree, rhs: Option[Term]): DefDef =
    DefDef.wrap(quotes.reflect.DefDef.copy(original.unwrapWithin)(name, paramss.map(_.unwrapWithin), tpt.unwrapWithin, rhs.map(_.unwrapWithin)))

}

final class ValDefCompanion(using quotes: Quotes) {

  /**
    * Create a value definition `val x`, `var x` or `lazy val x` with the signature defined in the symbol.
    *
    *  The `rhs` should return `Some` containing the implementation of the method,
    *  or `None` if the method has no implementation.
    *  Any definition directly inside the implementation should have `symbol` as owner.
    *
    *  Use `Symbol.asQuotes` to create the rhs using quoted code.
    *
    *  See also: `Tree.changeOwner`
    */
  def apply(symbol: Symbol, rhs: Option[Term]): ValDef =
    ValDef.wrap(quotes.reflect.ValDef.apply(symbol.unwrapWithin, rhs.map(_.unwrapWithin)))

  def copy(original: Tree)(name: String, tpt: TypeTree, rhs: Option[Term]): ValDef =
    ValDef.wrap(quotes.reflect.ValDef.copy(original.unwrapWithin)(name, tpt.unwrapWithin, rhs.map(_.unwrapWithin)))

  /**
    * Creates a block `{ val <name> = <rhs: Term>; <body(x): Term> }`
    *
    *  Usage:
    *  ```
    *  ValDef.let(owner, "x", rhs1) { x =>
    *    ValDef.let(x.symbol.owner, "y", rhs2) { y =>
    *      // use `x` and `y`
    *    }
    *  }
    *  ```
    */
  def let(owner: Symbol, name: String, rhs: Term)(body: Ref => Term): Term =
    Term.wrap(quotes.reflect.ValDef.let(owner.unwrapWithin, name, rhs.unwrapWithin)(x => body(Ref.wrap(x)).unwrapWithin))

  /**
    * Creates a block `{ val x = <rhs: Term>; <body(x): Term> }`
    *
    *  Usage:
    *  ```
    *  ValDef.let(owner, rhs1) { x =>
    *    ValDef.let(owner, rhs2) { y =>
    *      // use `x` and `y`
    *    }
    *  }
    *  ```
    */
  def let(owner: Symbol, rhs: Term)(body: Ref => Term): Term =
    Term.wrap(quotes.reflect.ValDef.let(owner.unwrapWithin, rhs.unwrapWithin)(x => body(Ref.wrap(x)).unwrapWithin))

  /**
    * Creates a block `{ val x1 = <terms(0): Term>; ...; val xn = <terms(n-1): Term>; <body(List(x1, ..., xn)): Term> }`
    *
    *  Usage:
    *  ```
    *  ValDef.let(owner, rhsList) { xs =>
    *     ...
    *  }
    *  ```
    */
  def let(owner: Symbol, terms: List[Term])(body: List[Ref] => Term): Term =
    Term.wrap(quotes.reflect.ValDef.let(owner.unwrapWithin, terms.map(_.unwrapWithin))(x => body(x.map(Ref.wrap(_))).unwrapWithin))

  // =====| Added |=====

  /**
    * Creates a block `{ val <name> = <rhs: Term>; <body(x): Term> }`
    *
    *  Usage:
    *  ```
    *  ValDef.let(owner, "x", rhs1) { x =>
    *    ValDef.let(x.symbol.owner, "y", rhs2) { y =>
    *      // use `x` and `y`
    *    }
    *  }
    *  ```
    */
  def let(owner: Symbol, name: String, rhs: Term, flags: Flags)(body: Ref => Term): Term = {
    val sym: Symbol = Symbol.newVal(owner, name, rhs.tpe.widen, flags, Symbol.noSymbol)
    val valDef: ValDef = apply(sym, Some(rhs.changeOwner(sym)))
    val ref: Ref = Ref.companion.apply(sym)
    Block.companion.apply(List(valDef), body(ref))
  }

}

final class TermCompanion(using quotes: Quotes) {

  /**
    * Returns a term that is functionally equivalent to `t`,
    *  however if `t` is of the form `((y1, ..., yn) => e2)(e1, ..., en)`
    *  then it optimizes the top most call by returning `Some`
    *  with the result of beta-reducing the function application.
    *  Similarly, all outermost curried function applications will be beta-reduced, if possible.
    *  Otherwise returns `None`.
    *
    *  To retain semantics the argument `ei` is bound as `val yi = ei` and by-name arguments to `def yi = ei`.
    *  Some bindings may be elided as an early optimization.
    *
    *  Example:
    *  ```scala sc:nocompile
    *  ((a: Int, b: Int) => a + b).apply(x, y)
    *  ```
    *  will be reduced to
    *  ```scala sc:nocompile
    *  val a = x
    *  val b = y
    *  a + b
    *  ```
    *
    *  Generally:
    *  ```scala sc:nocompile
    *  ([X1, Y1, ...] => (x1, y1, ...) => ... => [Xn, Yn, ...] => (xn, yn, ...) => f[X1, Y1, ..., Xn, Yn, ...](x1, y1, ..., xn, yn, ...))).apply[Tx1, Ty1, ...](myX1, myY1, ...)....apply[Txn, Tyn, ...](myXn, myYn, ...)
    *  ```
    *  will be reduced to
    *  ```scala sc:nocompile
    *  type X1 = Tx1
    *  type Y1 = Ty1
    *  ...
    *  val x1 = myX1
    *  val y1 = myY1
    *  ...
    *  type Xn = Txn
    *  type Yn = Tyn
    *  ...
    *  val xn = myXn
    *  val yn = myYn
    *  ...
    *  f[X1, Y1, ..., Xn, Yn, ...](x1, y1, ..., xn, yn, ...)
    *  ```
    */
  def betaReduce(term: Term): Option[Term] =
    quotes.reflect.Term.betaReduce(term.unwrapWithin).map(Term.wrap)

}

final class RefCompanion(using quotes: Quotes) {

  /** A tree representing the same reference as the given type */
  def term(tp: TermRef): Ref =
    Ref.wrap(quotes.reflect.Ref.term(tp.unwrapWithin))

  /**
    * Create a reference tree from a symbol
    *
    *  If `sym` refers to a class member `foo` in class `C`,
    *  returns a tree representing `C.this.foo`.
    *
    *  If `sym` refers to an object member `foo` in object C, itself in prefix
    *  `pre` (which might include `.this`, if it contains a class),
    *  returns `pre.C.foo`.
    *
    *  If `sym` refers to a local definition `foo`, returns
    *  a tree representing `foo`.
    *
    *  @note In all cases, the constructed tree should only
    *  be spliced into the places where such accesses make sense.
    *  For example, it is incorrect to have `C.this.foo` outside
    *  the class body of `C`, or have `foo` outside the lexical
    *  scope for the definition of `foo`.
    */
  def apply(sym: Symbol): Ref =
    Ref.wrap(quotes.reflect.Ref.apply(sym.unwrapWithin))

}

final class IdentCompanion(using quotes: Quotes) {

  def apply(tmref: TermRef): Term =
    Term.wrap(quotes.reflect.Ident.apply(tmref.unwrapWithin))

  def copy(original: Tree)(name: String): Ident =
    Ident.wrap(quotes.reflect.Ident.copy(original.unwrapWithin)(name))

}

final class WildcardCompanion(using quotes: Quotes) {

  /** Create a tree representing a `_` wildcard. */
  def apply(): Wildcard = Wildcard.wrap(quotes.reflect.Wildcard())

}

final class SelectCompanion(using quotes: Quotes) {

  /** Select a term member by symbol */
  def apply(qualifier: Term, symbol: Symbol): Select =
    Select.wrap(quotes.reflect.Select.apply(qualifier.unwrapWithin, symbol.unwrapWithin))

  /**
    * Select a field or a non-overloaded method by name
    *
    *  @note The method will produce an assertion error if the selected
    *        method is overloaded. The method `overloaded` should be used
    *        in that case.
    */
  def unique(qualifier: Term, name: String): Select =
    Select.wrap(quotes.reflect.Select.unique(qualifier.unwrapWithin, name))

  /** Call an overloaded method with the given type and term parameters */
  def overloaded(qualifier: Term, name: String, targs: List[TypeRepr], args: List[Term]): Term =
    Term.wrap(quotes.reflect.Select.overloaded(qualifier.unwrapWithin, name, targs.map(_.unwrapWithin), args.map(_.unwrapWithin)))

  /** Call an overloaded method with the given type and term parameters */
  def overloaded(qualifier: Term, name: String, targs: List[TypeRepr], args: List[Term], returnType: TypeRepr): Term =
    Term.wrap(quotes.reflect.Select.overloaded(qualifier.unwrapWithin, name, targs.map(_.unwrapWithin), args.map(_.unwrapWithin), returnType.unwrapWithin))

  def copy(original: Tree)(qualifier: Term, name: String): Select =
    Select.wrap(quotes.reflect.Select.copy(original.unwrapWithin)(qualifier.unwrapWithin, name))

}

final class LiteralCompanion(using quotes: Quotes) {

  /** Create a literal constant */
  def apply(constant: Constant): Literal =
    Literal.wrap(quotes.reflect.Literal.apply(constant.unwrapWithin))

  def copy(original: Tree)(constant: Constant): Literal =
    Literal.wrap(quotes.reflect.Literal.copy(original.unwrapWithin)(constant.unwrapWithin))

}

final class ThisCompanion(using quotes: Quotes) {

  /** Create a `C.this` for `C` pointing to `cls` */
  def apply(cls: Symbol): This =
    This.wrap(quotes.reflect.This.apply(cls.unwrapWithin))

  def copy(original: Tree)(qual: Option[String]): This =
    This.wrap(quotes.reflect.This.copy(original.unwrapWithin)(qual))

}

final class NewCompanion(using quotes: Quotes) {

  /** Create a `new <tpt: TypeTree>` */
  def apply(tpt: TypeTree): New =
    New.wrap(quotes.reflect.New.apply(tpt.unwrapWithin))

  def copy(original: Tree)(tpt: TypeTree): New =
    New.wrap(quotes.reflect.New.copy(original.unwrapWithin)(tpt.unwrapWithin))

}

final class NamedArgCompanion(using quotes: Quotes) {

  /** Create a named argument `<name: String> = <value: Term>` */
  def apply(name: String, arg: Term): NamedArg =
    NamedArg.wrap(quotes.reflect.NamedArg.apply(name, arg.unwrapWithin))

  def copy(original: Tree)(name: String, arg: Term): NamedArg =
    NamedArg.wrap(quotes.reflect.NamedArg.copy(original.unwrapWithin)(name, arg.unwrapWithin))

}

final class ApplyCompanion(using quotes: Quotes) {

  /** Create a function application `<fun: Term>(<args: List[Term]>)` */
  def apply(fun: Term, args: List[Term]): Apply =
    Apply.wrap(quotes.reflect.Apply.apply(fun.unwrapWithin, args.map(_.unwrapWithin)))

  def copy(original: Tree)(fun: Term, args: List[Term]): Apply =
    Apply.wrap(quotes.reflect.Apply.copy(original.unwrapWithin)(fun.unwrapWithin, args.map(_.unwrapWithin)))

}

final class TypeApplyCompanion(using quotes: Quotes) {

  /** Create a function type application `<fun: Term>[<args: List[TypeTree]>]` */
  def apply(fun: Term, args: List[TypeTree]): TypeApply =
    TypeApply.wrap(quotes.reflect.TypeApply.apply(fun.unwrapWithin, args.map(_.unwrapWithin)))

  def copy(original: Tree)(fun: Term, args: List[TypeTree]): TypeApply =
    TypeApply.wrap(quotes.reflect.TypeApply.copy(original.unwrapWithin)(fun.unwrapWithin, args.map(_.unwrapWithin)))

}

final class SuperCompanion(using quotes: Quotes) {

  /** Creates a `<qualifier: Term>.super[<id: Option[Id]>` */
  def apply(qual: Term, mix: Option[String]): Super =
    Super.wrap(quotes.reflect.Super.apply(qual.unwrapWithin, mix))

  def copy(original: Tree)(qual: Term, mix: Option[String]): Super =
    Super.wrap(quotes.reflect.Super.copy(original.unwrapWithin)(qual.unwrapWithin, mix))

}

final class AssignCompanion(using quotes: Quotes) {

  /** Create an assignment `<lhs: Term> = <rhs: Term>` */
  def apply(lhs: Term, rhs: Term): Assign =
    Assign.wrap(quotes.reflect.Assign.apply(lhs.unwrapWithin, rhs.unwrapWithin))

  def copy(original: Tree)(lhs: Term, rhs: Term): Assign =
    Assign.wrap(quotes.reflect.Assign.copy(original.unwrapWithin)(lhs.unwrapWithin, rhs.unwrapWithin))

}

final class BlockCompanion(using quotes: Quotes) {

  /** Creates a block `{ <statements: List[Statement]>; <expr: Term> }` */
  def apply(stats: List[Statement], expr: Term): Block =
    Block.wrap(quotes.reflect.Block.apply(stats.map(_.unwrapWithin), expr.unwrapWithin))

  def copy(original: Tree)(stats: List[Statement], expr: Term): Block =
    Block.wrap(quotes.reflect.Block.copy(original.unwrapWithin)(stats.map(_.unwrapWithin), expr.unwrapWithin))

}

final class ClosureCompanion(using quotes: Quotes) {

  def apply(meth: Term, tpe: Option[TypeRepr]): Closure =
    Closure.wrap(quotes.reflect.Closure.apply(meth.unwrapWithin, tpe.map(_.unwrapWithin)))

  def copy(original: Tree)(meth: Tree, tpe: Option[TypeRepr]): Closure =
    Closure.wrap(quotes.reflect.Closure.copy(original.unwrapWithin)(meth.unwrapWithin, tpe.map(_.unwrapWithin)))

}

final class IfCompanion(using quotes: Quotes) {

  /** Create an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
  def apply(cond: Term, thenp: Term, elsep: Term): If =
    If.wrap(quotes.reflect.If.apply(cond.unwrapWithin, thenp.unwrapWithin, elsep.unwrapWithin))

  def copy(original: Tree)(cond: Term, thenp: Term, elsep: Term): If =
    If.wrap(quotes.reflect.If.copy(original.unwrapWithin)(cond.unwrapWithin, thenp.unwrapWithin, elsep.unwrapWithin))

}

final class MatchCompanion(using quotes: Quotes) {

  /** Creates a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
  def apply(selector: Term, cases: List[CaseDef]): Match =
    Match.wrap(quotes.reflect.Match.apply(selector.unwrapWithin, cases.map(_.unwrapWithin)))

  def copy(original: Tree)(selector: Term, cases: List[CaseDef]): Match =
    Match.wrap(quotes.reflect.Match.copy(original.unwrapWithin)(selector.unwrapWithin, cases.map(_.unwrapWithin)))

}

final class SummonFromCompanion(using quotes: Quotes) {

  /** Creates a pattern match `given match { <cases: List[CaseDef]> }` */
  def apply(cases: List[CaseDef]): SummonFrom =
    SummonFrom.wrap(quotes.reflect.SummonFrom.apply(cases.map(_.unwrapWithin)))

  def copy(original: Tree)(cases: List[CaseDef]): SummonFrom =
    SummonFrom.wrap(quotes.reflect.SummonFrom.copy(original.unwrapWithin)(cases.map(_.unwrapWithin)))

}

final class TryCompanion(using quotes: Quotes) {

  /** Create a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
  def apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try =
    Try.wrap(quotes.reflect.Try.apply(expr.unwrapWithin, cases.map(_.unwrapWithin), finalizer.map(_.unwrapWithin)))

  def copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try =
    Try.wrap(quotes.reflect.Try.copy(original.unwrapWithin)(expr.unwrapWithin, cases.map(_.unwrapWithin), finalizer.map(_.unwrapWithin)))

}

final class ReturnCompanion(using quotes: Quotes) {

  /** Creates `return <expr: Term>` */
  def apply(expr: Term, from: Symbol): Return =
    Return.wrap(quotes.reflect.Return.apply(expr.unwrapWithin, from.unwrapWithin))

  def copy(original: Tree)(expr: Term, from: Symbol): Return =
    Return.wrap(quotes.reflect.Return.copy(original.unwrapWithin)(expr.unwrapWithin, from.unwrapWithin))

}

final class RepeatedCompanion(using quotes: Quotes) {

  /** Create a literal sequence of elements */
  def apply(elems: List[Term], tpt: TypeTree): Repeated =
    Repeated.wrap(quotes.reflect.Repeated.apply(elems.map(_.unwrapWithin), tpt.unwrapWithin))

  /** Copy a literal sequence of elements */
  def copy(original: Tree)(elems: List[Term], tpt: TypeTree): Repeated =
    Repeated.wrap(quotes.reflect.Repeated.copy(original.unwrapWithin)(elems.map(_.unwrapWithin), tpt.unwrapWithin))

  // =====| Added |=====

  /**
    * Similar to [[apply]], but [[apply]] is missing the `*` in `fun(values*)`.
    */
  def spread(elems: Seq[Term], tpt: TypeTree): Term = {
    type A
    given Type[A] = tpt.tpe.asTypeOf
    val elems2: Seq[Expr[A]] = elems.map(_.asExprOf[A])
    val expr: Expr[Seq[A]] = '{ Seq(${ Expr.ofSeq(elems2) }*) }
    expr.toTerm.removeInline.narrow[Apply].args.head
  }

}

final class InlinedCompanion(using quotes: Quotes) {

  def apply(call: Option[Tree], bindings: List[Definition], expansion: Term): Inlined =
    Inlined.wrap(quotes.reflect.Inlined.apply(call.map(_.unwrapWithin), bindings.map(_.unwrapWithin), expansion.unwrapWithin))

  def copy(original: Tree)(call: Option[Tree], bindings: List[Definition], expansion: Term): Inlined =
    Inlined.wrap(quotes.reflect.Inlined.copy(original.unwrapWithin)(call.map(_.unwrapWithin), bindings.map(_.unwrapWithin), expansion.unwrapWithin))

}

final class SelectOuterCompanion(using quotes: Quotes) {

  def apply(qualifier: Term, name: String, levels: Int): SelectOuter =
    SelectOuter.wrap(quotes.reflect.SelectOuter.apply(qualifier.unwrapWithin, name, levels))

  def copy(original: Tree)(qualifier: Term, name: String, levels: Int): SelectOuter =
    SelectOuter.wrap(quotes.reflect.SelectOuter.copy(original.unwrapWithin)(qualifier.unwrapWithin, name, levels))

}

final class WhileCompanion(using quotes: Quotes) {

  /** Creates a while loop `while (<cond>) <body>` and returns (<cond>, <body>) */
  def apply(cond: Term, body: Term): While =
    While.wrap(quotes.reflect.While.apply(cond.unwrapWithin, body.unwrapWithin))

  def copy(original: Tree)(cond: Term, body: Term): While =
    While.wrap(quotes.reflect.While.copy(original.unwrapWithin)(cond.unwrapWithin, body.unwrapWithin))

}

final class TypedCompanion(using quotes: Quotes) {

  /** Create a type ascription `<x: Term>: <tpt: TypeTree>` */
  def apply(expr: Term, tpt: TypeTree): Typed =
    Typed.wrap(quotes.reflect.Typed.apply(expr.unwrapWithin, tpt.unwrapWithin))

  def copy(original: Tree)(expr: Term, tpt: TypeTree): Typed =
    Typed.wrap(quotes.reflect.Typed.copy(original.unwrapWithin)(expr.unwrapWithin, tpt.unwrapWithin))

}

final class TypedOrTestCompanion(using quotes: Quotes) {

  /** Create a type ascription `<x: Tree>: <tpt: TypeTree>` */
  def apply(expr: Tree, tpt: TypeTree): TypedOrTest =
    TypedOrTest.wrap(quotes.reflect.TypedOrTest.apply(expr.unwrapWithin, tpt.unwrapWithin))

  def copy(original: Tree)(expr: Tree, tpt: TypeTree): TypedOrTest =
    TypedOrTest.wrap(quotes.reflect.TypedOrTest.copy(original.unwrapWithin)(expr.unwrapWithin, tpt.unwrapWithin))

}

final class BindCompanion(using quotes: Quotes) {

  def apply(sym: Symbol, pattern: Tree): Bind =
    Bind.wrap(quotes.reflect.Bind.apply(sym.unwrapWithin, pattern.unwrapWithin))

  def copy(original: Tree)(name: String, pattern: Tree): Bind =
    Bind.wrap(quotes.reflect.Bind.copy(original.unwrapWithin)(name, pattern.unwrapWithin))

}

final class UnapplyCompanion(using quotes: Quotes) {

  def apply(fun: Term, implicits: List[Term], patterns: List[Tree]): Unapply =
    Unapply.wrap(quotes.reflect.Unapply.apply(fun.unwrapWithin, implicits.map(_.unwrapWithin), patterns.map(_.unwrapWithin)))

  def copy(original: Tree)(fun: Term, implicits: List[Term], patterns: List[Tree]): Unapply =
    Unapply.wrap(quotes.reflect.Unapply.copy(original.unwrapWithin)(fun.unwrapWithin, implicits.map(_.unwrapWithin), patterns.map(_.unwrapWithin)))

}

final class AlternativesCompanion(using quotes: Quotes) {

  def apply(patterns: List[Tree]): Alternatives =
    Alternatives.wrap(quotes.reflect.Alternatives.apply(patterns.map(_.unwrapWithin)))

  def copy(original: Tree)(patterns: List[Tree]): Alternatives =
    Alternatives.wrap(quotes.reflect.Alternatives.copy(original.unwrapWithin)(patterns.map(_.unwrapWithin)))

}

final class CaseDefCompanion(using quotes: Quotes) {

  def apply(pattern: Tree, guard: Option[Term], rhs: Term): CaseDef =
    CaseDef.wrap(quotes.reflect.CaseDef.apply(pattern.unwrapWithin, guard.map(_.unwrapWithin), rhs.unwrapWithin))

  def copy(original: Tree)(pattern: Tree, guard: Option[Term], rhs: Term): CaseDef =
    CaseDef.wrap(quotes.reflect.CaseDef.copy(original.unwrapWithin)(pattern.unwrapWithin, guard.map(_.unwrapWithin), rhs.unwrapWithin))

}

final class TypeCaseDefCompanion(using quotes: Quotes) {

  def apply(pattern: TypeTree, rhs: TypeTree): TypeCaseDef =
    TypeCaseDef.wrap(quotes.reflect.TypeCaseDef.apply(pattern.unwrapWithin, rhs.unwrapWithin))

  def copy(original: Tree)(pattern: TypeTree, rhs: TypeTree): TypeCaseDef =
    TypeCaseDef.wrap(quotes.reflect.TypeCaseDef.copy(original.unwrapWithin)(pattern.unwrapWithin, rhs.unwrapWithin))

}

final class TypeTreeCompanion(using quotes: Quotes) {

  def of[T <: AnyKind](using t: Type[T]): TypeTree =
    TypeTree.wrap(quotes.reflect.TypeTree.of[T])

  def ref(typeSymbol: Symbol): TypeTree =
    TypeTree.wrap(quotes.reflect.TypeTree.ref(typeSymbol.unwrapWithin))

  // =====| Added |=====

  def fromType(tpe: Type[?]): TypeTree =
    TypeTree.of[Any](using tpe.asInstanceOf[Type[Any]])

}

final class InferredCompanion(using quotes: Quotes) {

  def apply(tpe: TypeRepr): Inferred =
    Inferred.wrap(quotes.reflect.Inferred.apply(tpe.unwrapWithin))

}

final class TypeIdentCompanion(using quotes: Quotes) {

  def apply(sym: Symbol): TypeTree =
    TypeTree.wrap(quotes.reflect.TypeIdent.apply(sym.unwrapWithin))

  def copy(original: Tree)(name: String): TypeIdent =
    TypeIdent.wrap(quotes.reflect.TypeIdent.copy(original.unwrapWithin)(name))

}

final class TypeSelectCompanion(using quotes: Quotes) {

  def apply(qualifier: Term, name: String): TypeSelect =
    TypeSelect.wrap(quotes.reflect.TypeSelect.apply(qualifier.unwrapWithin, name))

  def copy(original: Tree)(qualifier: Term, name: String): TypeSelect =
    TypeSelect.wrap(quotes.reflect.TypeSelect.copy(original.unwrapWithin)(qualifier.unwrapWithin, name))

}

final class TypeProjectionCompanion(using quotes: Quotes) {

  def copy(original: Tree)(qualifier: TypeTree, name: String): TypeProjection =
    TypeProjection.wrap(quotes.reflect.TypeProjection.copy(original.unwrapWithin)(qualifier.unwrapWithin, name))

}

final class SingletonCompanion(using quotes: Quotes) {

  def apply(ref: Term): Singleton =
    Singleton.wrap(quotes.reflect.Singleton.apply(ref.unwrapWithin))

  def copy(original: Tree)(ref: Term): Singleton =
    Singleton.wrap(quotes.reflect.Singleton.copy(original.unwrapWithin)(ref.unwrapWithin))

}

final class RefinedCompanion(using quotes: Quotes) {

  def copy(original: Tree)(tpt: TypeTree, refinements: List[Definition]): Refined =
    Refined.wrap(quotes.reflect.Refined.copy(original.unwrapWithin)(tpt.unwrapWithin, refinements.map(_.unwrapWithin)))

}

final class AppliedCompanion(using quotes: Quotes) {

  def apply(tpt: TypeTree, args: List[Tree]): Applied =
    Applied.wrap(quotes.reflect.Applied.apply(tpt.unwrapWithin, args.map(_.unwrapWithin)))

  def copy(original: Tree)(tpt: TypeTree, args: List[Tree]): Applied =
    Applied.wrap(quotes.reflect.Applied.copy(original.unwrapWithin)(tpt.unwrapWithin, args.map(_.unwrapWithin)))

}

final class AnnotatedCompanion(using quotes: Quotes) {

  def apply(arg: TypeTree, annotation: Term): Annotated =
    Annotated.wrap(quotes.reflect.Annotated.apply(arg.unwrapWithin, annotation.unwrapWithin))

  def copy(original: Tree)(arg: TypeTree, annotation: Term): Annotated =
    Annotated.wrap(quotes.reflect.Annotated.copy(original.unwrapWithin)(arg.unwrapWithin, annotation.unwrapWithin))

}

final class MatchTypeTreeCompanion(using quotes: Quotes) {

  def apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree =
    MatchTypeTree.wrap(quotes.reflect.MatchTypeTree.apply(bound.map(_.unwrapWithin), selector.unwrapWithin, cases.map(_.unwrapWithin)))

  def copy(original: Tree)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree =
    MatchTypeTree.wrap(quotes.reflect.MatchTypeTree.copy(original.unwrapWithin)(bound.map(_.unwrapWithin), selector.unwrapWithin, cases.map(_.unwrapWithin)))

}

final class ByNameCompanion(using quotes: Quotes) {

  def apply(result: TypeTree): ByName =
    ByName.wrap(quotes.reflect.ByName.apply(result.unwrapWithin))

  def copy(original: Tree)(result: TypeTree): ByName =
    ByName.wrap(quotes.reflect.ByName.copy(original.unwrapWithin)(result.unwrapWithin))

}

final class LambdaTypeTreeCompanion(using quotes: Quotes) {

  def apply(tparams: List[TypeDef], body: Tree): LambdaTypeTree =
    LambdaTypeTree.wrap(quotes.reflect.LambdaTypeTree.apply(tparams.map(_.unwrapWithin), body.unwrapWithin))

  def copy(original: Tree)(tparams: List[TypeDef], body: Tree): LambdaTypeTree =
    LambdaTypeTree.wrap(quotes.reflect.LambdaTypeTree.copy(original.unwrapWithin)(tparams.map(_.unwrapWithin), body.unwrapWithin))

}

final class TypeBindCompanion(using quotes: Quotes) {

  def copy(original: Tree)(name: String, tpt: Tree): TypeBind =
    TypeBind.wrap(quotes.reflect.TypeBind.copy(original.unwrapWithin)(name, tpt.unwrapWithin))

}

final class TypeBlockCompanion(using quotes: Quotes) {

  def apply(aliases: List[TypeDef], tpt: TypeTree): TypeBlock =
    TypeBlock.wrap(quotes.reflect.TypeBlock.apply(aliases.map(_.unwrapWithin), tpt.unwrapWithin))

  def copy(original: Tree)(aliases: List[TypeDef], tpt: TypeTree): TypeBlock =
    TypeBlock.wrap(quotes.reflect.TypeBlock.copy(original.unwrapWithin)(aliases.map(_.unwrapWithin), tpt.unwrapWithin))

}

final class TypeBoundsTreeCompanion(using quotes: Quotes) {

  def apply(low: TypeTree, hi: TypeTree): TypeBoundsTree =
    TypeBoundsTree.wrap(quotes.reflect.TypeBoundsTree.apply(low.unwrapWithin, hi.unwrapWithin))

  def copy(original: Tree)(low: TypeTree, hi: TypeTree): TypeBoundsTree =
    TypeBoundsTree.wrap(quotes.reflect.TypeBoundsTree.copy(original.unwrapWithin)(low.unwrapWithin, hi.unwrapWithin))

}

final class WildcardTypeTreeCompanion(using quotes: Quotes) {

  def apply(tpe: TypeRepr): WildcardTypeTree =
    WildcardTypeTree.wrap(quotes.reflect.WildcardTypeTree.apply(tpe.unwrapWithin))

}
