package oxygen.meta

import oxygen.core.NonEmptyList
import oxygen.core.syntax.either.*
import oxygen.core.syntax.option.*
import scala.annotation.{compileTimeOnly, experimental}
import scala.quoted.*

@scala.annotation.nowarn
final class Meta[Q <: Quotes](val quotes: Q) {
  given Quotes = quotes
  import quotes.reflect as Raw

  extension (self: Expr[?])
    def toTerm: Tree.Statement.Term =
      Tree.Statement.Term(self)

  extension (self: Type[?])
    def typeRepr: TypeRepr =
      TypeRepr.fromType(self)

  extension (self: Expr.type) {

    def unitBlock(seq: Seq[Expr[Any]]): Expr[Unit] =
      Tree.Statement.Term
        .Block(
          seq.toList.map(_.toTerm),
          '{ () }.toTerm,
        )
        .asExprTyped

  }

  sealed trait Tree {

    val raw: Raw.Tree

    /** Position in the source code */
    def pos: Position = Position(raw.pos)

    /** Symbol of defined or referred by this tree */
    def symbol: Symbol = Symbol(raw.symbol)

    /** Shows the tree as String */
    def show(using Raw.Printer[Raw.Tree]): String = raw.show

    /** Does this tree represent a valid expression? */
    def isExpr: Boolean = raw.isExpr

    /** Convert this tree to an `quoted.Expr[Any]` if the tree is a valid expression or throws */
    def asExpr: Expr[Any] = raw.asExpr
    def asExprOf[A: Type]: Expr[A] = raw.asExprOf[A]
    def asExprTyped[A]: Expr[A] = raw.asExpr.asInstanceOf[Expr[A]]

  }
  object Tree {

    def apply(raw: Raw.Tree): Tree = raw match
      case raw: Raw.Statement        => Tree.Statement(raw)
      case raw: Raw.TypeTree         => Tree.TypeTree(raw)
      case raw: Raw.TypedOrTest      => Tree.TypedOrTest(raw)
      case raw: Raw.PackageClause    => Tree.PackageClause(raw)
      case raw: Raw.Bind             => Tree.Bind(raw)
      case raw: Raw.Unapply          => Tree.Unapply(raw)
      case raw: Raw.Alternatives     => Tree.Alternatives(raw)
      case raw: Raw.CaseDef          => Tree.CaseDef(raw)
      case raw: Raw.TypeCaseDef      => Tree.TypeCaseDef(raw)
      case raw: Raw.TypeBoundsTree   => Tree.TypeBoundsTree(raw)
      case raw: Raw.WildcardTypeTree => Tree.WildcardTypeTree(raw)

    // ---  ---

    sealed trait Statement extends Tree {

      override val raw: Raw.Statement

    }
    object Statement {

      def apply(raw: Raw.Statement): Statement = raw match
        case raw: Raw.Definition => Tree.Statement.Definition(raw)
        case raw: Raw.Term       => Tree.Statement.Term(raw)
        case raw: Raw.Import     => Tree.Statement.Import(raw)
        case raw: Raw.Export     => Tree.Statement.Export(raw)

      // ---  ---

      final case class Import(raw: Raw.Import) extends Statement

      final case class Export(raw: Raw.Export) extends Statement

      sealed trait Definition extends Statement {

        override val raw: Raw.Definition

        def name: String = raw.name

      }
      object Definition {

        def apply(raw: Raw.Definition): Definition = raw match
          case raw: Raw.ClassDef    => Tree.Statement.Definition.ClassDef(raw)
          case raw: Raw.TypeDef     => Tree.Statement.Definition.TypeDef(raw)
          case raw: Raw.ValOrDefDef => Tree.Statement.Definition.ValOrDefDef(raw)

        // ---  ---

        final case class ClassDef(raw: Raw.ClassDef) extends Definition {

          /** The primary constructor of this class */
          def constructor: DefDef = DefDef(raw.constructor)

          /**
            * List of extended parent classes or traits.
            *  The first parent is always a class.
            */
          def parents: List[TermOrTypeTree] = raw.parents.map { tree => TermOrTypeTree.unsafe(Tree(tree)) }

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
          def self: Option[ValDef] = raw.self.map(ValDef(_))

          /**
            * Statements within the class
            *
            *  ```scala
            *  class C {
            *    ??? // statements
            *  }
            *  ```
            */
          def body: List[Statement] = raw.body.map(Statement(_))

        }
        object ClassDef {

          // def unapply(cdef: ClassDef): (String, DefDef, List[TermOrTypeTree], Option[ValDef], List[Statement])

          /**
            * Create a class definition tree
            *
            *  @param cls The class symbol. A new class symbol can be created using `Symbol.newClass`.
            *  @param parents The parents trees class. The trees must align with the parent types of `cls`.
            *                 Parents can be `TypeTree`s if they don't have term parameter,
            *                 otherwise the can be `Term` containing the `New` applied to the parameters of the extended class.
            *  @param body List of members of the class. The members must align with the members of `cls`.
            */
          // TODO add selfOpt: Option[ValDef]?
          @experimental def apply(cls: Symbol, parents: List[TermOrTypeTree], body: List[Statement]): ClassDef =
            ClassDef(Raw.ClassDef(cls.raw, parents.map(_.raw), body.map(_.raw)))

          def copy(original: Tree)(name: String, constr: DefDef, parents: List[TermOrTypeTree], selfOpt: Option[ValDef], body: List[Statement]): ClassDef =
            ClassDef(Raw.ClassDef.copy(original.raw)(name, constr.raw, parents.map(_.raw), selfOpt.map(_.raw), body.map(_.raw)))

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
          // TODO add selfOpt: Option[ValDef]?
          @experimental def module(module: Symbol, parents: List[TermOrTypeTree], body: List[Statement]): (ValDef, ClassDef) = {
            val (valDef, classDef) = Raw.ClassDef.module(module.raw, parents.map(_.raw), body.map(_.raw))
            (ValDef(valDef), ClassDef(classDef))
          }

        }

        final case class TypeDef(raw: Raw.TypeDef) extends Definition {

          def rhs: Tree = Tree(raw.rhs)

        }
        object TypeDef {

          def apply(symbol: Symbol): TypeDef =
            TypeDef(Raw.TypeDef(symbol.raw))

          def copy(original: Tree)(name: String, rhs: Tree): TypeDef =
            TypeDef(Raw.TypeDef.copy(original.raw)(name, rhs.raw))

          // def unapply(tdef: TypeDef): (String, Tree)

        }

        sealed trait ValOrDefDef extends Definition {

          val raw: Raw.ValOrDefDef

          /** The type tree of this `val` or `def` definition */
          def tpt: TypeTree = TypeTree(raw.tpt)

          /** The right-hand side of this `val` or `def` definition */
          def rhs: Option[Term] = raw.rhs.map(Term(_))

        }
        object ValOrDefDef {

          def apply(raw: Raw.ValOrDefDef): ValOrDefDef = raw match
            case raw: Raw.DefDef => Tree.Statement.Definition.DefDef(raw)
            case raw: Raw.ValDef => Tree.Statement.Definition.ValDef(raw)

        }

        final case class DefDef(raw: Raw.DefDef) extends ValOrDefDef {

          /** List of type and term parameter clauses */
          def paramss: List[ParamClause] = raw.paramss.map(ParamClause(_))

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
          def leadingTypeParams: List[TypeDef] = raw.leadingTypeParams.map(TypeDef(_))

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
          def trailingParamss: List[ParamClause] = raw.trailingParamss.map(ParamClause(_))

          /** List of term parameter clauses */
          def termParamss: List[ParamClause.TermParamClause] = raw.termParamss.map(ParamClause.TermParamClause(_))

          /** The tree of the return type of this `def` definition */
          def returnTpt: TypeTree = TypeTree(raw.returnTpt)

        }
        object DefDef {

          def apply(symbol: Symbol, rhsFn: List[List[Tree]] => Option[Term]): DefDef =
            DefDef(Raw.DefDef(symbol.raw, i => rhsFn(i.map(_.map(Tree(_)))).map(_.raw)))

          def copy(original: Tree)(name: String, paramss: List[ParamClause], tpt: TypeTree, rhs: Option[Term]): DefDef =
            DefDef(Raw.DefDef.copy(original.raw)(name, paramss.map(_.raw), tpt.raw, rhs.map(_.raw)))

          // def unapply(ddef: DefDef): (String, List[ParamClause], TypeTree, Option[Term])

        }

        final case class ValDef(raw: Raw.ValDef) extends ValOrDefDef
        object ValDef {

          /**
            * Create a value definition `val x`, `var x` or `lazy val x` with the signature defined in the symbol.
            *
            *  The `rhs` should return be `Some` containing the implementation of the method.
            *  Returns `None` the method has no implementation.
            *  Any definition directly inside the implementation should have `symbol` as owner.
            *
            *  Use `Symbol.asQuotes` to create the rhs using quoted code.
            *
            *  See also: `Tree.changeOwner`
            */
          def apply(symbol: Symbol, rhs: Option[Term]): ValDef =
            ValDef(Raw.ValDef(symbol.raw, rhs.map(_.raw)))

          def copy(original: Tree)(name: String, tpt: TypeTree, rhs: Option[Term]): ValDef =
            ValDef(Raw.ValDef.copy(original.raw)(name, tpt.raw, rhs.map(_.raw)))

          // def unapply(vdef: ValDef): (String, TypeTree, Option[Term])

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
          def let(owner: Symbol, name: String, rhs: Term)(body: Tree.Statement.Term.Ref => Term): Term =
            Term(Raw.ValDef.let(owner.raw, name, rhs.raw)(i => body(Tree.Statement.Term.Ref(i)).raw))

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
          def let(owner: Symbol, rhs: Term)(body: Tree.Statement.Term.Ref => Term): Term =
            let(owner, "x", rhs)(body)

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
          def let(owner: Symbol, terms: List[Term])(body: List[Tree.Statement.Term.Ref] => Term): Term =
            Term(Raw.ValDef.let(owner.raw, terms.map(_.raw))(i => body(i.map(Tree.Statement.Term.Ref(_))).raw))

        }

      }

      sealed trait Term extends Statement {

        override val raw: Raw.Term

        /** TypeRepr of this term */
        def tpe: TypeRepr = TypeRepr(raw.tpe)

        /**
          * Replace Inlined nodes and InlineProxy references to underlying arguments.
          *  The resulting tree is useful for inspection of the value or content of a non-inline argument.
          *
          *  Warning: This tree may contain references that are out of scope and should not be used in the generated code.
          *           This method should only used to port Scala 2 that used to access their outer scope unsoundly.
          */
        def underlyingArgument: Term = Term(raw.underlyingArgument)

        /**
          * Replace Ident nodes references to the underlying tree that defined them.
          *  The resulting tree is useful for inspection of the definition of some bindings.
          *
          *  Warning: This tree may contain references that are out of scope and should not be used in the generated code.
          *           This method should only used to port Scala 2 that used to access their outer scope unsoundly.
          */
        def underlying: Term = Term(raw.underlying)

        /** Converts a partially applied term into a lambda expression */
        def etaExpand(owner: Symbol): Term = Term(raw.etaExpand(owner.raw))

        /** A unary apply node with given argument: `tree(arg)` */
        def appliedTo(arg: Term): Term = Term(raw.appliedTo(arg.raw))

        /** An apply node with given arguments: `tree(arg, args0, ..., argsN)` */
        def appliedTo(arg: Term, args: Term*): Term = Term(raw.appliedTo(arg.raw, args.map(_.raw)*))

        /** An apply node with given argument list `tree(args(0), ..., args(args.length - 1))` */
        def appliedToArgs(args: List[Term]): Term.Apply = Term.Apply(raw.appliedToArgs(args.map(_.raw)))

        /**
          * The current tree applied to given argument lists:
          *  `tree (argss(0)) ... (argss(argss.length -1))`
          */
        def appliedToArgss(argss: List[List[Term]]): Term = Term(raw.appliedToArgss(argss.map(_.map(_.raw))))

        /** The current tree applied to (): `tree()` */
        def appliedToNone: Term.Apply = Term.Apply(raw.appliedToNone)

        /** The current tree applied to given type argument: `tree[targ]` */
        def appliedToType(targ: TypeRepr): Term = Term(raw.appliedToType(targ.raw))

        /** The current tree applied to given type arguments: `tree[targ0, ..., targN]` */
        def appliedToTypes(targs: List[TypeRepr]): Term = Term(raw.appliedToTypes(targs.map(_.raw)))

        /** The current tree applied to given type argument list: `tree[targs(0), ..., targs(targs.length - 1)]` */
        def appliedToTypeTrees(targs: List[TypeTree]): Term = Term(raw.appliedToTypeTrees(targs.map(_.raw)))

        /** A select node that selects the given symbol. */
        def select(sym: Symbol): Term.Ref.Select = Term.Ref.Select(raw.select(sym.raw))

      }
      object Term {

        def apply(raw: Raw.Term): Term = raw match
          case raw: Raw.Ref         => Tree.Statement.Term.Ref(raw)
          case raw: Raw.Literal     => Tree.Statement.Term.Literal(raw)
          case raw: Raw.This        => Tree.Statement.Term.This(raw)
          case raw: Raw.New         => Tree.Statement.Term.New(raw)
          case raw: Raw.NamedArg    => Tree.Statement.Term.NamedArg(raw)
          case raw: Raw.Apply       => Tree.Statement.Term.Apply(raw)
          case raw: Raw.TypeApply   => Tree.Statement.Term.TypeApply(raw)
          case raw: Raw.Super       => Tree.Statement.Term.Super(raw)
          case raw: Raw.Assign      => Tree.Statement.Term.Assign(raw)
          case raw: Raw.Block       => Tree.Statement.Term.Block(raw)
          case raw: Raw.Closure     => Tree.Statement.Term.Closure(raw)
          case raw: Raw.If          => Tree.Statement.Term.If(raw)
          case raw: Raw.Match       => Tree.Statement.Term.Match(raw)
          case raw: Raw.SummonFrom  => Tree.Statement.Term.SummonFrom(raw)
          case raw: Raw.Try         => Tree.Statement.Term.Try(raw)
          case raw: Raw.Return      => Tree.Statement.Term.Return(raw)
          case raw: Raw.Repeated    => Tree.Statement.Term.Repeated(raw)
          case raw: Raw.Inlined     => Tree.Statement.Term.Inlined(raw)
          case raw: Raw.SelectOuter => Tree.Statement.Term.SelectOuter(raw)
          case raw: Raw.While       => Tree.Statement.Term.While(raw)
          case raw: Raw.Typed       => Tree.Statement.Term.Typed(raw)

        def apply(expr: Expr[?]): Term = {
          import Raw.asTerm
          Term(expr.asTerm)
        }

        final case class Literal(raw: Raw.Literal) extends Term {

          /** Value of this literal */
          def constant: Constant = Constant(raw.constant)

        }
        object Literal {

          /** Create a literal constant */
          def apply(constant: Constant): Literal =
            Literal(Raw.Literal(constant.raw))

          def copy(original: Tree)(constant: Constant): Literal =
            Literal(Raw.Literal.copy(original.raw)(constant.raw))

          /** Matches a literal constant */
          // def unapply(x: Literal): Some[Constant]

        }

        final case class This(raw: Raw.This) extends Term {

          /**
            * Returns `C` if the underlying tree is of the form `C.this`
            *
            *  Otherwise, return `None`.
            */
          def id: Option[String] = raw.id

        }
        object This {

          /** Create a `C.this` for `C` pointing to `cls` */
          def apply(cls: Symbol): This =
            This(Raw.This(cls.raw))

          def copy(original: Tree)(qual: Option[String]): This =
            This(Raw.This.copy(original.raw)(qual))

          /** Matches `this` or `qual.this` and returns the name of `qual` */
          // def unapply(x: This): Some[Option[String]]

        }

        final case class New(raw: Raw.New) extends Term {

          /** Returns the type tree of this `new` */
          def tpt: TypeTree = TypeTree(raw.tpt)

        }
        object New {

          /** Create a `new <tpt: TypeTree>` */
          def apply(tpt: TypeTree): New =
            New(Raw.New(tpt.raw))

          def copy(original: Tree)(tpt: TypeTree): New =
            New(Raw.New.copy(original.raw)(tpt.raw))

          /** Matches `new <tpt: TypeTree>` */
          // def unapply(x: New): Some[TypeTree]

        }

        final case class NamedArg(raw: Raw.NamedArg) extends Term {

          /** The name part of `name = arg` */
          def name: String = raw.name

          /** The argument part of `name = arg` */
          def value: Term = Term(raw.value)

        }
        object NamedArg {

          /** Create a named argument `<name: String> = <value: Term>` */
          def apply(name: String, arg: Term): NamedArg =
            NamedArg(Raw.NamedArg(name, arg.raw))

          def copy(original: Tree)(name: String, arg: Term): NamedArg =
            NamedArg(Raw.NamedArg.copy(original.raw)(name, arg.raw))

          /** Matches a named argument `<name: String> = <value: Term>` */
          // def unapply(x: NamedArg): (String, Term)

        }

        final case class Apply(raw: Raw.Apply) extends Term {

          /**
            * The `fun` part of an (implicit) application like `fun(args)`
            *
            *  It may be a partially applied method:
            *  ```scala
            *  def f(x1: Int)(x2: Int) = ???
            *  f(1)(2)
            *  ```
            *  - `fun` is `f(1)` in the `Apply` of `f(1)(2)`
            *  - `fun` is `f` in the `Apply` of `f(1)`
            */
          def fun: Term = Term(raw.fun)

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
          def args: List[Term] = raw.args.map(Term(_))

        }
        object Apply {

          /** Create a function application `<fun: Term>(<args: List[Term]>)` */
          def apply(fun: Term, args: List[Term]): Apply =
            Apply(Raw.Apply(fun.raw, args.map(_.raw)))

          def copy(original: Tree)(fun: Term, args: List[Term]): Apply =
            Apply(Raw.Apply.copy(original.raw)(fun.raw, args.map(_.raw)))

          /** Matches a function application `<fun: Term>(<args: List[Term]>)` */
          // def unapply(x: Apply): (Term, List[Term])

        }

        final case class TypeApply(raw: Raw.TypeApply) extends Term {

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
          def fun: Term = Term(raw.fun)

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
          def args: List[TypeTree] = raw.args.map(TypeTree(_))

        }
        object TypeApply {

          /** Create a function type application `<fun: Term>[<args: List[TypeTree]>]` */
          def apply(fun: Term, args: List[TypeTree]): TypeApply =
            TypeApply(Raw.TypeApply(fun.raw, args.map(_.raw)))

          def copy(original: Tree)(fun: Term, args: List[TypeTree]): TypeApply =
            TypeApply(Raw.TypeApply.copy(original.raw)(fun.raw, args.map(_.raw)))

          /** Matches a function type application `<fun: Term>[<args: List[TypeTree]>]` */
          // def unapply(x: TypeApply): (Term, List[TypeTree])

        }

        final case class Super(raw: Raw.Super) extends Term {

          def qualifier: Term = Term(raw.qualifier)

          def id: Option[String] = raw.id

          def idPos: Position = Position(raw.idPos)

        }
        object Super {

          /** Creates a `<qualifier: Term>.super[<id: Option[Id]>` */
          def apply(qual: Term, mix: Option[String]): Super =
            Super(Raw.Super(qual.raw, mix))

          def copy(original: Tree)(qual: Term, mix: Option[String]): Super =
            Super(Raw.Super.copy(original.raw)(qual.raw, mix))

          /** Matches a `<qualifier: Term>.super[<id: Option[Id]>` */
          // def unapply(x: Super): (Term, Option[String])

        }

        final case class Assign(raw: Raw.Assign) extends Term {

          def lhs: Term = Term(raw.lhs)

          def rhs: Term = Term(raw.rhs)

        }
        object Assign {

          /** Create an assignment `<lhs: Term> = <rhs: Term>` */
          def apply(lhs: Term, rhs: Term): Assign =
            Assign(Raw.Assign(lhs.raw, rhs.raw))

          def copy(original: Tree)(lhs: Term, rhs: Term): Assign =
            Assign(Raw.Assign.copy(original.raw)(lhs.raw, rhs.raw))

          /** Matches an assignment `<lhs: Term> = <rhs: Term>` */
          // def unapply(x: Assign): (Term, Term)

        }

        final case class Block(raw: Raw.Block) extends Term {

          def statements: List[Statement] = raw.statements.map(Statement(_))

          def expr: Term = Term(raw.expr)

        }
        object Block {

          /** Creates a block `{ <statements: List[Statement]>; <expr: Term> }` */
          def apply(stats: List[Statement], expr: Term): Block =
            Block(Raw.Block(stats.map(_.raw), expr.raw))

          def copy(original: Tree)(stats: List[Statement], expr: Term): Block =
            Block(Raw.Block.copy(original.raw)(stats.map(_.raw), expr.raw))

          /** Matches a block `{ <statements: List[Statement]>; <expr: Term> }` */
          // def unapply(x: Block): (List[Statement], Term)

        }

        final case class Closure(raw: Raw.Closure) extends Term {

          def meth: Term = Term(raw.meth)

          def tpeOpt: Option[TypeRepr] = raw.tpeOpt.map(TypeRepr(_))

        }
        object Closure {

          def apply(meth: Term, tpe: Option[TypeRepr]): Closure =
            Closure(Raw.Closure(meth.raw, tpe.map(_.raw)))

          def copy(original: Tree)(meth: Tree, tpe: Option[TypeRepr]): Closure =
            Closure(Raw.Closure.copy(original.raw)(meth.raw, tpe.map(_.raw)))

          // def unapply(x: Closure): (Term, Option[TypeRepr])

        }

        final case class If(raw: Raw.If) extends Term {

          def cond: Term = Term(raw.cond)

          def thenp: Term = Term(raw.thenp)

          def elsep: Term = Term(raw.elsep)

          def isInline: Boolean = raw.isInline

        }
        object If {

          /** Create an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
          def apply(cond: Term, thenp: Term, elsep: Term): If =
            If(Raw.If(cond.raw, thenp.raw, elsep.raw))

          def copy(original: Tree)(cond: Term, thenp: Term, elsep: Term): If =
            If(Raw.If.copy(original.raw)(cond.raw, thenp.raw, elsep.raw))

          /** Matches an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
          // def unapply(tree: If): (Term, Term, Term)

        }

        final case class Match(raw: Raw.Match) extends Term {

          def scrutinee: Term = Term(raw.scrutinee)

          def cases: List[CaseDef] = raw.cases.map(CaseDef(_))

          def isInline: Boolean = raw.isInline

        }
        object Match {

          /** Creates a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
          def apply(selector: Term, cases: List[CaseDef]): Match =
            Match(Raw.Match(selector.raw, cases.map(_.raw)))

          def copy(original: Tree)(selector: Term, cases: List[CaseDef]): Match =
            Match(Raw.Match.copy(original.raw)(selector.raw, cases.map(_.raw)))

          /** Matches a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
          // def unapply(x: Match): (Term, List[CaseDef])

        }

        final case class SummonFrom(raw: Raw.SummonFrom) extends Term {

          def cases: List[CaseDef] = raw.cases.map(CaseDef(_))

        }
        object SummonFrom {

          /** Creates a pattern match `given match { <cases: List[CaseDef]> }` */
          def apply(cases: List[CaseDef]): SummonFrom =
            SummonFrom(Raw.SummonFrom(cases.map(_.raw)))

          def copy(original: Tree)(cases: List[CaseDef]): SummonFrom =
            SummonFrom(Raw.SummonFrom.copy(original.raw)(cases.map(_.raw)))

          /** Matches a pattern match `given match { <cases: List[CaseDef]> }` */
          // def unapply(x: SummonFrom): Some[List[CaseDef]]

        }

        final case class Try(raw: Raw.Try) extends Term {

          def body: Term = Term(raw.body)

          def cases: List[CaseDef] = raw.cases.map(CaseDef(_))

          def finalizer: Option[Term] = raw.finalizer.map(Term(_))

        }
        object Try {

          /** Create a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
          def apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try =
            Try(Raw.Try(expr.raw, cases.map(_.raw), finalizer.map(_.raw)))

          def copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try =
            Try(Raw.Try.copy(original.raw)(expr.raw, cases.map(_.raw), finalizer.map(_.raw)))

          /** Matches a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
          // def unapply(x: Try): (Term, List[CaseDef], Option[Term])

        }

        final case class Return(raw: Raw.Return) extends Term {

          def expr: Term = Term(raw.expr)

          def from: Symbol = Symbol(raw.from)

        }
        object Return {

          /** Creates `return <expr: Term>` */
          def apply(expr: Term, from: Symbol): Return =
            Return(Raw.Return(expr.raw, from.raw))

          def copy(original: Tree)(expr: Term, from: Symbol): Return =
            Return(Raw.Return.copy(original.raw)(expr.raw, from.raw))

          /** Matches `return <expr: Term>` and extracts the expression and symbol of the method */
          // def unapply(x: Return): (Term, Symbol)

        }

        final case class Repeated(raw: Raw.Repeated) extends Term {

          def elems: List[Term] = raw.elems.map(Term(_))

          def elemtpt: TypeTree = TypeTree(raw.elemtpt)

        }
        object Repeated {

          def apply(elems: List[Term], tpt: TypeTree): Repeated =
            Repeated(Raw.Repeated(elems.map(_.raw), tpt.raw))

          def copy(original: Tree)(elems: List[Term], tpt: TypeTree): Repeated =
            Repeated(Raw.Repeated.copy(original.raw)(elems.map(_.raw), tpt.raw))

          // def unapply(x: Repeated): (List[Term], TypeTree)

        }

        final case class Inlined(raw: Raw.Inlined) extends Term {

          def call: Option[TermOrTypeTree] = raw.call.map(t => TermOrTypeTree.unsafe(Tree(t)))
          def bindings: List[Definition] = raw.bindings.map(Definition(_))
          def body: Term = Term(raw.body)

        }

        object Inlined {

          def apply(call: Option[TermOrTypeTree], bindings: List[Definition], expansion: Term): Inlined =
            Inlined(Raw.Inlined(call.map(_.raw), bindings.map(_.raw), expansion.raw))

          def copy(original: Tree)(call: Option[TermOrTypeTree], bindings: List[Definition], expansion: Term): Inlined =
            Inlined(Raw.Inlined.copy(original.raw)(call.map(_.raw), bindings.map(_.raw), expansion.raw))

          // def unapply(x: Inlined): (Option[TermOrTypeTree ], List[Definition], Term)

        }

        final case class SelectOuter(raw: Raw.SelectOuter) extends Term {

          def qualifier: Term = Term(raw.qualifier)

          def name: String = raw.name

          def level: Int = raw.level

        }
        object SelectOuter {

          def apply(qualifier: Term, name: String, levels: Int): SelectOuter =
            SelectOuter(Raw.SelectOuter(qualifier.raw, name, levels))

          def copy(original: Tree)(qualifier: Term, name: String, levels: Int): SelectOuter =
            SelectOuter(Raw.SelectOuter.copy(original.raw)(qualifier.raw, name, levels))

          // def unapply(x: SelectOuter): (Term, String, Int)

        }

        final case class While(raw: Raw.While) extends Term {

          def cond: Term = Term(raw.cond)

          def body: Term = Term(raw.body)

        }
        object While {

          /** Creates a while loop `while (<cond>) <body>` and returns (<cond>, <body>) */
          def apply(cond: Term, body: Term): While =
            While(Raw.While(cond.raw, body.raw))

          def copy(original: Tree)(cond: Term, body: Term): While =
            While(Raw.While.copy(original.raw)(cond.raw, body.raw))

          /** Extractor for while loops. Matches `while (<cond>) <body>` and returns (<cond>, <body>) */
          // def unapply(x: While): (Term, Term)

        }

        export internal.Typed

        sealed trait Ref extends Term {

          override val raw: Raw.Ref

        }
        object Ref {

          def apply(raw: Raw.Ref): Ref = raw match
            case raw: Raw.Ident  => Tree.Statement.Term.Ref.Ident(raw)
            case raw: Raw.Select => Tree.Statement.Term.Ref.Select(raw)
            // cases somehow falling through?
            case _ => Tree.Statement.Term.Ref.Ident(raw.asInstanceOf[Raw.Ident])

          /** A tree representing the same reference as the given type */
          def term(tp: TypeRepr.NamedType.TermRef): Ref =
            Ref(Raw.Ref.term(tp.raw))

          /**
            * Create a reference tree from a symbol
            *
            *  If `sym` refers to a class member `foo` in class `C`,
            *  returns a tree representing `C.this.foo`.
            *
            *  If `sym` refers to a local definition `foo`, returns
            *  a tree representing `foo`.
            *
            *  @note In both cases, the constructed tree should only
            *  be spliced into the places where such accesses make sense.
            *  For example, it is incorrect to have `C.this.foo` outside
            *  the class body of `C`, or have `foo` outside the lexical
            *  scope for the definition of `foo`.
            */
          def apply(sym: Symbol): Ref =
            Ref(Raw.Ref(sym.raw))

          // ---  ---

          sealed trait Ident extends Ref {

            override val raw: Raw.Ident

            /** Name of this `Ident` */
            def name: String = raw.name

          }
          object Ident {

            def apply(raw: Raw.Ident): Ident = raw match
              case raw: Raw.Wildcard => Tree.Statement.Term.Ref.Ident.Wildcard(raw)
              case _                 => Tree.Statement.Term.Ref.Ident.Other(raw)

            def apply(tmref: TypeRepr.NamedType.TermRef): Term =
              Term(Raw.Ident(tmref.raw))

            def copy(original: Tree)(name: String): Ident =
              Ident(Raw.Ident.copy(original.raw)(name))

            /** Matches a term identifier and returns its name */
            // def unapply(tree: Ident): Some[String]

            // ---  ---

            final case class Wildcard(raw: Raw.Wildcard) extends Ident
            object Wildcard {

              /** Create a tree representing a `_` wildcard. */
              def apply(): Wildcard =
                Wildcard(Raw.Wildcard())

              /** Match a tree representing a `_` wildcard. */
              // def unapply(wildcard: Wildcard): true

            }

            final case class Other(raw: Raw.Ident) extends Ident

          }

          final case class Select(raw: Raw.Select) extends Ref {

            /** Qualifier of the `qualifier.name` */
            def qualifier: Term = Term(raw.qualifier)

            /** Name of this `Select` */
            def name: String = raw.name

            /** Signature of this method */
            def signature: Option[Signature] = raw.signature.map(Signature(_))

          }
          object Select {

            /** Select a term member by symbol */
            def apply(qualifier: Term, symbol: Symbol): Select =
              Select(Raw.Select(qualifier.raw, symbol.raw))

            /**
              * Select a field or a non-overloaded method by name
              *
              *  @note The method will produce an assertion error if the selected
              *        method is overloaded. The method `overloaded` should be used
              *        in that case.
              */
            def unique(qualifier: Term, name: String): Select =
              Select(Raw.Select.unique(qualifier.raw, name))

            /** Call an overloaded method with the given type and term parameters */
            def overloaded(qualifier: Term, name: String, targs: List[TypeRepr], args: List[Term]): Term =
              Term(Raw.Select.overloaded(qualifier.raw, name, targs.map(_.raw), args.map(_.raw)))

            /** Call an overloaded method with the given type and term parameters */
            def overloaded(qualifier: Term, name: String, targs: List[TypeRepr], args: List[Term], returnType: TypeRepr): Term =
              Term(Raw.Select.overloaded(qualifier.raw, name, targs.map(_.raw), args.map(_.raw), returnType.raw))

            def copy(original: Tree)(qualifier: Term, name: String): Select =
              Select(Raw.Select.copy(original.raw)(qualifier.raw, name))

            /** Matches `<qualifier: Term>.<name: String>` */
            // def unapply(x: Select): (Term, String)

          }

        }

      }

    }

    sealed trait TypeTree extends Tree {

      override val raw: Raw.TypeTree

      /** TypeRepr of this type tree */
      def tpe: TypeRepr = TypeRepr(raw.tpe)

    }
    object TypeTree {

      def apply(raw: Raw.TypeTree): TypeTree = raw match
        case raw: Raw.Inferred       => Tree.TypeTree.Inferred(raw)
        case raw: Raw.TypeIdent      => Tree.TypeTree.TypeIdent(raw)
        case raw: Raw.TypeSelect     => Tree.TypeTree.TypeSelect(raw)
        case raw: Raw.TypeProjection => Tree.TypeTree.TypeProjection(raw)
        case raw: Raw.Singleton      => Tree.TypeTree.Singleton(raw)
        case raw: Raw.Refined        => Tree.TypeTree.Refined(raw)
        case raw: Raw.Applied        => Tree.TypeTree.Applied(raw)
        case raw: Raw.Annotated      => Tree.TypeTree.Annotated(raw)
        case raw: Raw.MatchTypeTree  => Tree.TypeTree.MatchTypeTree(raw)
        case raw: Raw.ByName         => Tree.TypeTree.ByName(raw)
        case raw: Raw.LambdaTypeTree => Tree.TypeTree.LambdaTypeTree(raw)
        case raw: Raw.TypeBind       => Tree.TypeTree.TypeBind(raw)
        case raw: Raw.TypeBlock      => Tree.TypeTree.TypeBlock(raw)

      /** Returns the tree of type or kind (TypeTree) of T */
      def of[T <: AnyKind](using Type[T]): TypeTree =
        TypeTree(Raw.TypeTree.of[T])

      def fromType(tpe: Type[?]): TypeTree =
        TypeTree.of[Any](using tpe.asInstanceOf[Type[Any]])

      /** Returns the tree of type or kind (TypeTree) of T */
      @compileTimeOnly("Reference to `scala.quoted.Type.of` was not handled by PickleQuotes")
      def ofType[T <: AnyKind]: TypeTree = {
        given Type[T] = Type.of[T](using quotes)
        TypeTree.of[T]
      }

      /**
        * Returns a type tree reference to the symbol
        *
        *  @param typeSymbol The type symbol for which we are creating a type tree reference.
        */
      def ref(typeSymbol: Symbol): TypeTree =
        TypeTree(Raw.TypeTree.ref(typeSymbol.raw))

      // ---  ---

      final case class Inferred(raw: Raw.Inferred) extends TypeTree
      object Inferred {

        def apply(tpe: TypeRepr): Inferred =
          Inferred(Raw.Inferred(tpe.raw))

        /** Matches a TypeTree containing an inferred type */
        // def unapply(x: Inferred): true

      }

      final case class TypeIdent(raw: Raw.TypeIdent) extends TypeTree {

        def name: String = raw.name

      }
      object TypeIdent {

        def apply(sym: Symbol): TypeTree =
          TypeTree(Raw.TypeIdent(sym.raw))

        def copy(original: Tree)(name: String): TypeIdent =
          TypeIdent(Raw.TypeIdent.copy(original.raw)(name))

        // def unapply(x: TypeIdent): Some[String]

      }

      final case class TypeSelect(raw: Raw.TypeSelect) extends TypeTree {

        def qualifier: Tree.Statement.Term = Tree.Statement.Term(raw.qualifier)

        def name: String = raw.name

      }
      object TypeSelect {

        def apply(qualifier: Tree.Statement.Term, name: String): TypeSelect =
          TypeSelect(Raw.TypeSelect(qualifier.raw, name))

        def copy(original: Tree)(qualifier: Tree.Statement.Term, name: String): TypeSelect =
          TypeSelect(Raw.TypeSelect.copy(original.raw)(qualifier.raw, name))

        // def unapply(x: TypeSelect): (Term, String)

      }

      final case class TypeProjection(raw: Raw.TypeProjection) extends TypeTree {

        def qualifier: TypeTree = TypeTree(raw.qualifier)

        def name: String = raw.name

      }
      object TypeProjection {

        def copy(original: Tree)(qualifier: TypeTree, name: String): TypeProjection =
          TypeProjection(Raw.TypeProjection.copy(original.raw)(qualifier.raw, name))

        // def unapply(x: TypeProjection): (TypeTree, String)

      }

      final case class Singleton(raw: Raw.Singleton) extends TypeTree {

        def ref: Tree.Statement.Term = Tree.Statement.Term(raw.ref)

      }
      object Singleton {

        def apply(ref: Tree.Statement.Term): Singleton =
          Singleton(Raw.Singleton(ref.raw))

        def copy(original: Tree)(ref: Tree.Statement.Term): Singleton =
          Singleton(Raw.Singleton.copy(original.raw)(ref.raw))

        // def unapply(x: Singleton): Some[Term]

      }

      final case class Refined(raw: Raw.Refined) extends TypeTree {

        def tpt: TypeTree = TypeTree(raw.tpt)

        def refinements: List[Tree.Statement.Definition] = raw.refinements.map(Tree.Statement.Definition(_))

      }
      object Refined {

        def copy(original: Tree)(tpt: TypeTree, refinements: List[Tree.Statement.Definition]): Refined =
          Refined(Raw.Refined.copy(original.raw)(tpt.raw, refinements.map(_.raw)))

        // def unapply(x: Refined): (TypeTree, List[Definition])

      }

      final case class Applied(raw: Raw.Applied) extends TypeTree {

        def tpt: TypeTree = TypeTree(raw.tpt)

        def args: List[TypeTreeOrTypeBoundsTree] = raw.args.map(tree => TypeTreeOrTypeBoundsTree.unsafe(Tree(tree)))

      }
      object Applied {

        def apply(tpt: TypeTree, args: List[TypeTreeOrTypeBoundsTree]): Applied =
          Applied(Raw.Applied(tpt.raw, args.map(_.raw)))

        def copy(original: Tree)(tpt: TypeTree, args: List[TypeTreeOrTypeBoundsTree]): Applied =
          Applied(Raw.Applied.copy(original.raw)(tpt.raw, args.map(_.raw)))

        // def unapply(x: Applied): (TypeTree, List[Tree /*TypeTree | TypeBoundsTree*/])

      }

      final case class Annotated(raw: Raw.Annotated) extends TypeTree {

        def arg: TypeTree = TypeTree(raw.arg)

        def annotation: Tree.Statement.Term = Tree.Statement.Term(raw.annotation)

      }
      object Annotated {

        def apply(arg: TypeTree, annotation: Tree.Statement.Term): Annotated =
          Annotated(Raw.Annotated(arg.raw, annotation.raw))

        def copy(original: Tree)(arg: TypeTree, annotation: Tree.Statement.Term): Annotated =
          Annotated(Raw.Annotated.copy(original.raw)(arg.raw, annotation.raw))

        // def unapply(x: Annotated): (TypeTree, Term)

      }

      final case class MatchTypeTree(raw: Raw.MatchTypeTree) extends TypeTree {

        def bound: Option[TypeTree] = raw.bound.map(TypeTree(_))

        def selector: TypeTree = TypeTree(raw.selector)

        def cases: List[TypeCaseDef] = raw.cases.map(TypeCaseDef(_))

      }
      object MatchTypeTree {

        def apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree =
          MatchTypeTree(Raw.MatchTypeTree(bound.map(_.raw), selector.raw, cases.map(_.raw)))

        def copy(original: Tree)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree =
          MatchTypeTree(Raw.MatchTypeTree.copy(original.raw)(bound.map(_.raw), selector.raw, cases.map(_.raw)))

        // def unapply(x: MatchTypeTree): (Option[TypeTree], TypeTree, List[TypeCaseDef])

      }

      final case class ByName(raw: Raw.ByName) extends TypeTree {

        def result: TypeTree = TypeTree(raw.result)

      }
      object ByName {

        def apply(result: TypeTree): ByName =
          ByName(Raw.ByName(result.raw))

        def copy(original: Tree)(result: TypeTree): ByName =
          ByName(Raw.ByName.copy(original.raw)(result.raw))

        // def unapply(x: ByName): Some[TypeTree]

      }

      final case class LambdaTypeTree(raw: Raw.LambdaTypeTree) extends TypeTree {

        def tparams: List[Tree.Statement.Definition.TypeDef] = raw.tparams.map(Tree.Statement.Definition.TypeDef(_))

        def body: TypeTreeOrTypeBoundsTree = TypeTreeOrTypeBoundsTree.unsafe(Tree(raw.body))

      }
      object LambdaTypeTree {

        def apply(tparams: List[Tree.Statement.Definition.TypeDef], body: TypeTreeOrTypeBoundsTree): LambdaTypeTree =
          LambdaTypeTree(Raw.LambdaTypeTree(tparams.map(_.raw), body.raw))

        def copy(original: Tree)(tparams: List[Tree.Statement.Definition.TypeDef], body: TypeTreeOrTypeBoundsTree): LambdaTypeTree =
          LambdaTypeTree(Raw.LambdaTypeTree.copy(original.raw)(tparams.map(_.raw), body.raw))

        // def unapply(tree: LambdaTypeTree): (List[TypeDef], TypeTreeOrTypeBoundsTree)

      }

      final case class TypeBind(raw: Raw.TypeBind) extends TypeTree {

        def name: String = raw.name

        def body: TypeTreeOrTypeBoundsTree = TypeTreeOrTypeBoundsTree.unsafe(Tree(raw.body))

      }
      object TypeBind {

        def copy(original: Tree)(name: String, tpt: TypeTreeOrTypeBoundsTree): TypeBind =
          TypeBind(Raw.TypeBind.copy(original.raw)(name, tpt.raw))

        // def unapply(x: TypeBind): (String, Tree TypeTreeOrTypeBoundsTree)

      }

      final case class TypeBlock(raw: Raw.TypeBlock) extends TypeTree {

        def aliases: List[Tree.Statement.Definition.TypeDef] = raw.aliases.map(Tree.Statement.Definition.TypeDef(_))

        def tpt: TypeTree = TypeTree(raw.tpt)

      }
      object TypeBlock {

        def apply(aliases: List[Tree.Statement.Definition.TypeDef], tpt: TypeTree): TypeBlock =
          TypeBlock(Raw.TypeBlock(aliases.map(_.raw), tpt.raw))

        def copy(original: Tree)(aliases: List[Tree.Statement.Definition.TypeDef], tpt: TypeTree): TypeBlock =
          TypeBlock(Raw.TypeBlock.copy(original.raw)(aliases.map(_.raw), tpt.raw))

        // def unapply(x: TypeBlock): (List[TypeDef], TypeTree)

      }

    }

    sealed trait TypedOrTest extends Tree {

      override val raw: Raw.TypedOrTest

      def tree: Tree = Tree(raw.tree)

      def tpt: TypeTree = TypeTree(raw.tpt)

    }
    object TypedOrTest {

      def apply(raw: Raw.TypedOrTest): TypedOrTest = raw match
        case raw: Raw.Typed => Tree.TypedOrTest.Typed(raw)

      /** Create a type ascription `<x: Tree>: <tpt: TypeTree>` */
      def apply(expr: Tree, tpt: TypeTree): TypedOrTest =
        TypedOrTest(Raw.TypedOrTest(expr.raw, tpt.raw))

      def copy(original: Tree)(expr: Tree, tpt: TypeTree): TypedOrTest =
        TypedOrTest(Raw.TypedOrTest.copy(original.raw)(expr.raw, tpt.raw))

      /** Matches `<expr: Tree>: <tpt: TypeTree>` */
      // def unapply(x: TypedOrTest): (Tree, TypeTree)
      // ---  ---
      export internal.Typed

    }

    final case class PackageClause(raw: Raw.PackageClause) extends Tree {

      /** Tree containing the package name */
      def pid: Tree.Statement.Term.Ref = Tree.Statement.Term.Ref(raw.pid)

      /** Definitions, imports or exports within the package */
      def stats: List[Tree] = raw.stats.map(Tree(_))

    }
    object PackageClause {

      /** Create a package clause `package pid { stats }` */
      def apply(pid: Tree.Statement.Term.Ref, stats: List[Tree]): PackageClause =
        PackageClause(Raw.PackageClause(pid.raw, stats.map(_.raw)))

      /** Copy a package clause `package pid { stats }` */
      def copy(original: Tree)(pid: Tree.Statement.Term.Ref, stats: List[Tree]): PackageClause =
        PackageClause(Raw.PackageClause.copy(original.raw)(pid.raw, stats.map(_.raw)))

      /** Matches a package clause `package pid { stats }` and extracts the `pid` and `stats` */
      // def unapply(tree: PackageClause): (Ref, List[Tree])

    }

    final case class Bind(raw: Raw.Bind) extends Tree {

      def name: String = raw.name

      def pattern: Tree = Tree(raw.pattern)

    }
    object Bind {

      def apply(sym: Symbol, pattern: Tree): Bind =
        Bind(Raw.Bind(sym.raw, pattern.raw))

      def copy(original: Tree)(name: String, pattern: Tree): Bind =
        Bind(Raw.Bind.copy(original.raw)(name, pattern.raw))

      // def unapply(pattern: Bind): (String, Tree)

    }

    final case class Unapply(raw: Raw.Unapply) extends Tree {

      /**
        * The extractor function of the pattern.
        *
        *  It may be a reference to the `unapply` method of the pattern or may be a
        *  partially applied tree containing type parameters and leading given parameters.
        */
      def fun: Tree.Statement.Term = Tree.Statement.Term(raw.fun)

      /** Training implicit parameters of the `unapply` method */
      def implicits: List[Tree.Statement.Term] = raw.implicits.map(Tree.Statement.Term(_))

      /** List of nested patterns */
      def patterns: List[Tree] = raw.patterns.map(Tree(_))

    }
    object Unapply {

      /** Create an `Unapply` tree representing a pattern `<fun>(<patterns*>)(using <implicits*>)` */
      def apply(fun: Tree.Statement.Term, implicits: List[Tree.Statement.Term], patterns: List[Tree]): Unapply =
        Unapply(Raw.Unapply(fun.raw, implicits.map(_.raw), patterns.map(_.raw)))

      /** Copy an `Unapply` tree representing a pattern `<fun>(<patterns*>)(using <implicits*>)` */
      def copy(original: Tree)(fun: Tree.Statement.Term, implicits: List[Tree.Statement.Term], patterns: List[Tree]): Unapply =
        Unapply(Raw.Unapply.copy(original.raw)(fun.raw, implicits.map(_.raw), patterns.map(_.raw)))

      /** Matches an `Unapply(fun, implicits, patterns)` tree representing a pattern `<fun>(<patterns*>)(using <implicits*>)` */
      // def unapply(x: Unapply): (Term, List[Term], List[Tree])

    }

    final case class Alternatives(raw: Raw.Alternatives) extends Tree {

      def patterns: List[Tree] = raw.patterns.map(Tree(_))

    }
    object Alternatives {

      def apply(patterns: List[Tree]): Alternatives =
        Alternatives(Raw.Alternatives(patterns.map(_.raw)))

      def copy(original: Tree)(patterns: List[Tree]): Alternatives =
        Alternatives(Raw.Alternatives.copy(original.raw)(patterns.map(_.raw)))

      // def unapply(x: Alternatives): Some[List[Tree]]

    }

    final case class CaseDef(raw: Raw.CaseDef) extends Tree {

      def pattern: Tree = Tree(raw.pattern)

      def guard: Option[Tree.Statement.Term] = raw.guard.map(Tree.Statement.Term(_))

      def rhs: Tree.Statement.Term = Tree.Statement.Term(raw.rhs)

    }
    object CaseDef {

      def apply(pattern: Tree, guard: Option[Tree.Statement.Term], rhs: Tree.Statement.Term): CaseDef =
        CaseDef(Raw.CaseDef(pattern.raw, guard.map(_.raw), rhs.raw))

      def copy(original: Tree)(pattern: Tree, guard: Option[Tree.Statement.Term], rhs: Tree.Statement.Term): CaseDef =
        CaseDef(Raw.CaseDef.copy(original.raw)(pattern.raw, guard.map(_.raw), rhs.raw))

      // def unapply(x: CaseDef): (Tree, Option[Term], Term)

    }

    final case class TypeCaseDef(raw: Raw.TypeCaseDef) extends Tree {

      def pattern: TypeTree = TypeTree(raw.pattern)

      def rhs: TypeTree = TypeTree(raw.rhs)

    }
    object TypeCaseDef {

      def apply(pattern: TypeTree, rhs: TypeTree): TypeCaseDef =
        TypeCaseDef(Raw.TypeCaseDef(pattern.raw, rhs.raw))

      def copy(original: Tree)(pattern: TypeTree, rhs: TypeTree): TypeCaseDef =
        TypeCaseDef(Raw.TypeCaseDef.copy(original.raw)(pattern.raw, rhs.raw))

      // def unapply(tree: TypeCaseDef): (TypeTree, TypeTree)

    }

    // Type tree representing a type bound written in the source
    /*TypeTree | TypeBoundsTree*/
    final case class TypeBoundsTree(raw: Raw.TypeBoundsTree) extends Tree {

      def tpe: TypeRepr.TypeBounds = TypeRepr.TypeBounds(raw.tpe)

      def low: TypeTree = TypeTree(raw.low)

      def hi: TypeTree = TypeTree(raw.hi)

    }
    object TypeBoundsTree {

      def apply(low: TypeTree, hi: TypeTree): TypeBoundsTree =
        TypeBoundsTree(Raw.TypeBoundsTree(low.raw, hi.raw))

      def copy(original: Tree)(low: TypeTree, hi: TypeTree): TypeBoundsTree =
        TypeBoundsTree(Raw.TypeBoundsTree.copy(original.raw)(low.raw, hi.raw))

      // def unapply(x: TypeBoundsTree): (TypeTree, TypeTree)

    }

    final case class WildcardTypeTree(raw: Raw.WildcardTypeTree) extends Tree {

      def tpe: TypeRepr = TypeRepr(raw.tpe)

    }
    object WildcardTypeTree {

      def apply(tpe: TypeRepr): WildcardTypeTree =
        WildcardTypeTree(Raw.WildcardTypeTree(tpe.raw))

      /** Matches a TypeBoundsTree containing wildcard type bounds */
      // def unapply(x: WildcardTypeTree): true

    }

    type TermOrTypeTree = Tree.Statement.Term | TypeTree
    object TermOrTypeTree {

      def unsafe(tree: Tree): TermOrTypeTree = tree match
        case term: Tree.Statement.Term => term
        case typeTree: TypeTree        => typeTree
        case tree                      => throw new RuntimeException(s"Not a `TermOrTypeTree`: ${tree.show}")

    }

    type TypeTreeOrTypeBoundsTree = TypeTree | TypeBoundsTree
    object TypeTreeOrTypeBoundsTree {

      def unsafe(tree: Tree): TypeTreeOrTypeBoundsTree = tree match
        case typeTree: TypeTree             => typeTree
        case typeBoundsTree: TypeBoundsTree => typeBoundsTree
        case tree                           => throw new RuntimeException(s"Not a `TypeTreeOrTypeBoundsTree`: ${tree.show}")

    }

  }

  sealed trait ParamClause {

    val raw: Raw.ParamClause

    /** List of parameters of the clause */
    def params: Either[List[Tree.Statement.Definition.ValDef], List[Tree.Statement.Definition.TypeDef]] = {
      val tmp1: List[Raw.ValDef | Raw.TypeDef] = raw.params
      val (tmp2, tmp3): (List[Raw.ValDef], List[Raw.TypeDef]) =
        tmp1.partitionMap {
          case x: Raw.ValDef  => x.asLeft
          case x: Raw.TypeDef => x.asRight
        }

      (tmp2, tmp3) match {
        case (Nil, Nil)      => Nil.asLeft
        case (valDefs, Nil)  => valDefs.map(Tree.Statement.Definition.ValDef(_)).asLeft
        case (Nil, typeDefs) => typeDefs.map(Tree.Statement.Definition.TypeDef(_)).asRight
      }
    }

  }
  object ParamClause {

    def apply(raw: Raw.ParamClause): ParamClause = raw match
      case raw: Raw.TypeParamClause => ParamClause.TypeParamClause(raw)
      case raw: Raw.TermParamClause => ParamClause.TermParamClause(raw)

    final case class TypeParamClause(raw: Raw.TypeParamClause) extends ParamClause

    final case class TermParamClause(raw: Raw.TermParamClause) extends ParamClause

  }

  sealed trait TypeRepr {

    val raw: Raw.TypeRepr

    /** Shows the type as a String */
    def show(using Raw.Printer[Raw.TypeRepr]): String = raw.show

    def show2(default: TypeRepr => String): String = this match {
      case TypeRepr.AppliedType(root, args) =>
        s"${root.show2(default)}[${args.map(a => "\n    " + a.show2.replaceAll("\n", "\n    ") + ",").mkString}\n]"
      case _ =>
        default(this)
    }
    def show2: String = show2(_.raw.toString)

    /**
      * Convert this `TypeRepr` to an `Type[?]`
      *
      *  Usage:
      *  ```scala
      *  //{
      *  import scala.quoted._
      *  def f(using Quotes) = {
      *    val q: Quotes = summon[Quotes]
      *    import q.reflect._
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
    def asType: Type[?] = raw.asType
    def asTyped[A]: Type[A] = raw.asType.asInstanceOf[Type[A]]

    /**
      * Is `self` type the same as `that` type?
      *  This is the case iff `self <:< that` and `that <:< self`.
      */
    def =:=(that: TypeRepr): Boolean = this.raw =:= that.raw

    /** Is this type a subtype of that type? */
    def <:<(that: TypeRepr): Boolean = this.raw <:< that.raw

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
    def widen: TypeRepr = TypeRepr(raw.widen)

    /**
      * Widen from TermRef to its underlying non-termref
      *  base type, while also skipping ByName types.
      */
    def widenTermRefByName: TypeRepr = TypeRepr(raw.widenTermRefByName)

    /** Widen from ByName type to its result type. */
    def widenByName: TypeRepr = TypeRepr(raw.widenByName)

    /** Follow aliases, annotated types until type is no longer alias type, annotated type. */
    def dealias: TypeRepr = TypeRepr(raw.dealias)

    /**
      * A simplified version of this type which is equivalent wrt =:= to this type.
      *  Reduces typerefs, applied match types, and and or types.
      */
    def simplified: TypeRepr = TypeRepr(raw.simplified)

    def classSymbol: Option[Symbol] = raw.classSymbol.map(Symbol(_))
    def typeSymbol: Symbol = Symbol(raw.typeSymbol)
    def termSymbol: Symbol = Symbol(raw.termSymbol)
    def isSingleton: Boolean = raw.isSingleton

    /**
      * The type of `member` as seen from prefix `self`.
      *
      *  Also see `typeRef` and `termRef`
      */
    def memberType(member: Symbol): TypeRepr = TypeRepr(raw.memberType(member.raw))

    /** The base classes of this type with the class itself as first element. */
    def baseClasses: List[Symbol] = raw.baseClasses.map(Symbol(_))

    /**
      * The least type instance of given class which is a super-type
      *  of this type.  Example:
      *  {{{
      *    class D[T]
      *    class C extends p.D[Int]
      *    ThisType(C).baseType(D) = p.D[Int]
      * }}}
      */
    def baseType(cls: Symbol): TypeRepr = TypeRepr(raw.baseType(cls.raw))

    /** Is this type an instance of a non-bottom subclass of the given class `cls`? */
    def derivesFrom(cls: Symbol): Boolean = raw.derivesFrom(cls.raw)

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
    def isFunctionType: Boolean = raw.isFunctionType

    /**
      * Is this type an context function type?
      *
      *  @see `isFunctionType`
      */
    def isContextFunctionType: Boolean = raw.isContextFunctionType

    /**
      * Is this type a function type with erased parameters?
      *
      *  @see `isFunctionType`
      */
    def isErasedFunctionType: Boolean = raw.isErasedFunctionType

    /**
      * Is this type a dependent function type?
      *
      *  @see `isFunctionType`
      */
    def isDependentFunctionType: Boolean = raw.isDependentFunctionType

    /**
      * Is this type a `TupleN` type?
      *
      * @return true if the dealiased type of `self` is `TupleN[T1, T2, ..., Tn]`
      */
    def isTupleN: Boolean = raw.isTupleN

    /** The type <this . sym>, reduced if possible */
    def select(sym: Symbol): TypeRepr = TypeRepr(raw.select(sym.raw))

    /** The current type applied to given type arguments: `this[targ]` */
    def appliedTo(targ: TypeRepr): TypeRepr = TypeRepr(raw.appliedTo(targ.raw))

    /** The current type applied to given type arguments: `this[targ0, ..., targN]` */
    def appliedTo(targs: List[TypeRepr]): TypeRepr = TypeRepr(raw.appliedTo(targs.map(_.raw)))

    def appliedTo(targ0: TypeRepr, targ1: TypeRepr, targN: TypeRepr*): TypeRepr = this.appliedTo(targ0 :: targ1 :: targN.toList)

    /**
      * Substitute all types that refer in their symbol attribute to
      *  one of the symbols in `from` by the corresponding types in `to`.
      */
    def substituteTypes(from: List[Symbol], to: List[TypeRepr]): TypeRepr = TypeRepr(raw.substituteTypes(from.map(_.raw), to.map(_.raw)))

    /** The applied type arguments (empty if there is no such arguments) */
    def typeArgs: List[TypeRepr] = raw.typeArgs.map(TypeRepr(_))

    // ---  ---

    def &&(that: TypeRepr): TypeRepr.AndOrType.AndType = TypeRepr.AndOrType.AndType(this, that)
    def ||(that: TypeRepr): TypeRepr.AndOrType.OrType = TypeRepr.AndOrType.OrType(this, that)

    final def typeOrTermSymbol: Symbol = if (this.isSingleton) this.termSymbol else this.typeSymbol

    final def typeType: Option[Symbol.TypeType] = this.typeOrTermSymbol.typeType
    final def typeTypeSealed: Option[Symbol.TypeType.Sealed] = this.typeOrTermSymbol.typeTypeSealed
    final def typeTypeCase: Option[Symbol.TypeType.Case] = this.typeOrTermSymbol.typeTypeCase

    final def optionalAnnotation[Annot: Type]: Option[Expr[Annot]] = {
      val annotTpe = TypeRepr.of[Annot]
      val annotFlags = annotTpe.typeSymbol.flags

      if (annotFlags.is(Flags.Abstract) || annotFlags.is(Flags.Trait))
        report.errorAndAbort(s"Bad annotation type ${annotTpe.show} is abstract")

      this.typeOrTermSymbol.getAnnotation(annotTpe.typeSymbol) match
        case Some(tree) if tree.tpe <:< annotTpe => tree.asExprOf[Annot].some
        case _                                   => None
    }

    final def requiredAnnotation[Annot: Type]: Expr[Annot] =
      optionalAnnotation[Annot].getOrElse(report.errorAndAbort(s"Missing required annotation `${TypeRepr.of[Annot].show}` for `${this.show}`"))

    final def optionalAnnotationValue[Annot: {Type, FromExpr}]: Option[Annot] =
      optionalAnnotation[Annot].map { expr =>
        expr.value.getOrElse(report.errorAndAbort(s"Found annotation `${TypeRepr.of[Annot].show}` for `${this.show}`, but are unable to extract Expr.value\n${expr.show}"))
      }

    final def requiredAnnotationValue[Annot: {Type, FromExpr}]: Annot = {
      val expr = requiredAnnotation[Annot]
      expr.value.getOrElse(report.errorAndAbort(s"Found annotation `${TypeRepr.of[Annot].show}` for `${this.show}`, but are unable to extract Expr.value\n${expr.show}"))
    }

    final def typeTree: Tree.TypeTree =
      Tree.TypeTree.fromType(this.asType)

  }

  object TypeRepr {

    def apply(raw: Raw.TypeRepr): TypeRepr = raw match
      case raw: Raw.NamedType     => TypeRepr.NamedType(raw)
      case raw: Raw.AndOrType     => TypeRepr.AndOrType(raw)
      case raw: Raw.LambdaType    => TypeRepr.LambdaType(raw)
      case raw: Raw.ConstantType  => TypeRepr.ConstantType(raw)
      case raw: Raw.SuperType     => TypeRepr.SuperType(raw)
      case raw: Raw.Refinement    => TypeRepr.Refinement(raw)
      case raw: Raw.AppliedType   => TypeRepr.AppliedType(raw)
      case raw: Raw.AnnotatedType => TypeRepr.AnnotatedType(raw)
      case raw: Raw.MatchType     => TypeRepr.MatchType(raw)
      case raw: Raw.ByNameType    => TypeRepr.ByNameType(raw)
      case raw: Raw.ParamRef      => TypeRepr.ParamRef(raw)
      case raw: Raw.ThisType      => TypeRepr.ThisType(raw)
      case raw: Raw.RecursiveThis => TypeRepr.RecursiveThis(raw)
      case raw: Raw.RecursiveType => TypeRepr.RecursiveType(raw)
      case raw: Raw.MatchCase     => TypeRepr.MatchCase(raw)
      case raw: Raw.TypeBounds    => TypeRepr.TypeBounds(raw)
      case raw: Raw.NoPrefix      => TypeRepr.NoPrefix(raw)

      /** Returns the type or kind (TypeRepr) of T */
    def of[T <: AnyKind](using Type[T]): TypeRepr =
      TypeRepr(Raw.TypeRepr.of[T])

    def fromType(tpe: Type[?]): TypeRepr = {
      type _T
      @scala.annotation.unused
      given Type[_T] = tpe.asInstanceOf[Type[_T]]

      TypeRepr.of[_T]
    }

    /** Returns the type or kind (TypeRepr) of T */
    @compileTimeOnly("Reference to `scala.quoted.Type.of` was not handled by PickleQuotes")
    inline def ofType[T <: AnyKind]: TypeRepr = {
      given Type[T] = Type.of[T](using quotes)
      TypeRepr.of[T]
    }

    /** Returns the type constructor of the runtime (erased) class */
    def typeConstructorOf(clazz: Class[?]): TypeRepr =
      TypeRepr(Raw.TypeRepr.typeConstructorOf(clazz))

    /**
      * Attempts to create a tuple in the form of `(A, B, C)`,
      * otherwise calls [[tupleUsingAppend]].
      */
    def tuplePreferTupleN(typeParams: TypeRepr*): TypeRepr =
      if (typeParams.size > 0 && typeParams.size <= 22) defn.TupleClass(typeParams.size).typeRef.appliedTo(typeParams.toList)
      else tupleUsingAppend(typeParams*)

    /**
      * Creates a TypeRepf of tuple in the form of `A *: B *: C *: EmptyTuple`
      */
    def tupleUsingAppend(typeParams: TypeRepr*): TypeRepr =
      typeParams.foldRight(defn.emptyTuple) { case (t, acc) =>
        defn.tupleAppend.appliedTo(t, acc)
      }

    // ---  ---

    sealed trait NamedType extends TypeRepr {

      override val raw: Raw.NamedType

      def qualifier: TypeRepr = TypeRepr(raw.qualifier)

      def name: String = raw.name

    }
    object NamedType {

      def apply(raw: Raw.NamedType): NamedType = raw match
        case raw: Raw.TermRef => TypeRepr.NamedType.TermRef(raw)
        case raw: Raw.TypeRef => TypeRepr.NamedType.TypeRef(raw)

      // ---  ---

      final case class TermRef(raw: Raw.TermRef) extends NamedType {

        def toTerm: Tree.Statement.Term = Tree.Statement.Term.Ref.term(this)

      }
      object TermRef {

        def apply(qual: TypeRepr, name: String): TermRef =
          TermRef(Raw.TermRef(qual.raw, name))

        // def unapply(x: TermRef): (TypeRepr, String)

      }

      final case class TypeRef(raw: Raw.TypeRef) extends NamedType {

        def isOpaqueAlias: Boolean = raw.isOpaqueAlias

        def translucentSuperType: TypeRepr = TypeRepr(raw.translucentSuperType)

      }
      object TypeRef {

        // def unapply(x: TypeRef): (TypeRepr, String)

      }

    }

    sealed trait AndOrType extends TypeRepr {

      override val raw: Raw.AndOrType

      def left: TypeRepr = TypeRepr(raw.left)

      def right: TypeRepr = TypeRepr(raw.right)

    }
    object AndOrType {

      def apply(raw: Raw.AndOrType): AndOrType = raw match
        case raw: Raw.AndType => TypeRepr.AndOrType.AndType(raw)
        case raw: Raw.OrType  => TypeRepr.AndOrType.OrType(raw)

      final case class AndType(raw: Raw.AndType) extends AndOrType
      object AndType {

        def apply(lhs: TypeRepr, rhs: TypeRepr): AndType =
          AndType(Raw.AndType(lhs.raw, rhs.raw))

        // def unapply(x: AndType): (TypeRepr, TypeRepr)

      }

      final case class OrType(raw: Raw.OrType) extends AndOrType
      object OrType {

        def apply(lhs: TypeRepr, rhs: TypeRepr): OrType =
          OrType(Raw.OrType(lhs.raw, rhs.raw))

        // def unapply(x: OrType): (TypeRepr, TypeRepr)

      }

    }

    sealed trait LambdaType extends TypeRepr {

      override val raw: Raw.LambdaType

      def paramNames: List[String] = raw.paramNames

      def paramTypes: List[TypeRepr] = raw.paramTypes.map(TypeRepr(_))

      def resType: TypeRepr = TypeRepr(raw.resType)

    }
    object LambdaType {

      def apply(raw: Raw.LambdaType): LambdaType = raw match
        case raw: Raw.MethodOrPoly => TypeRepr.LambdaType.MethodOrPoly(raw)
        case raw: Raw.TypeLambda   => TypeRepr.LambdaType.TypeLambda(raw)

      sealed trait MethodOrPoly extends LambdaType {
        override val raw: Raw.MethodOrPoly
      }
      object MethodOrPoly {

        def apply(raw: Raw.MethodOrPoly): MethodOrPoly = raw match
          case raw: Raw.MethodType => TypeRepr.LambdaType.MethodOrPoly.MethodType(raw)
          case raw: Raw.PolyType   => TypeRepr.LambdaType.MethodOrPoly.PolyType(raw)

        final case class MethodType(raw: Raw.MethodType) extends MethodOrPoly {

          /** Is this the type of using parameter clause `(implicit X1, ..., Xn)`, `(using X1, ..., Xn)` or `(using x1: X1, ..., xn: Xn)` */
          def isImplicit: Boolean = raw.isImplicit

          /** Is this the type of erased parameter clause `(erased x1: X1, ..., xn: Xn)` */
          // TODO:deprecate in 3.4 and stabilize `erasedParams` and `hasErasedParams`.
          // @deprecated("Use `hasErasedParams`","3.4")
          def isErased: Boolean = raw.isErased

          /** List of `erased` flags for each parameters of the clause */
          // @experimental def erasedParams: List[Boolean] = raw.erasedParams

          /** Whether the clause has any erased parameters */
          // @experimental def hasErasedParams: Boolean = raw.hasErasedParams
          def param(idx: Int): TypeRepr = TypeRepr(raw.param(idx))

        }
        object MethodType {

          def apply(paramNames: List[String])(paramInfosExp: MethodType => List[TypeRepr], resultTypeExp: MethodType => TypeRepr): MethodType =
            MethodType(Raw.MethodType(paramNames)(i => paramInfosExp(MethodType(i)).map(_.raw), i => resultTypeExp(MethodType(i)).raw))

          // def unapply(x: MethodType): (List[String], List[TypeRepr], TypeRepr)

        }

        final case class PolyType(raw: Raw.PolyType) extends MethodOrPoly {

          def param(idx: Int): TypeRepr = TypeRepr(raw.param(idx))

          def paramBounds: List[TypeBounds] = raw.paramBounds.map(TypeBounds(_))

        }
        object PolyType {

          def apply(paramNames: List[String])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => TypeRepr): PolyType =
            PolyType(Raw.PolyType(paramNames)(i => paramBoundsExp(PolyType(i)).map(_.raw), i => resultTypeExp(PolyType(i)).raw))

          // def unapply(x: PolyType): (List[String], List[TypeBounds], TypeRepr)

        }

      }

      final case class TypeLambda(raw: Raw.TypeLambda) extends LambdaType {

        def param(idx: Int): TypeRepr = TypeRepr(raw.param(idx))

        def paramBounds: List[TypeBounds] = raw.paramBounds.map(TypeBounds(_))

      }
      object TypeLambda {

        def apply(paramNames: List[String], boundsFn: TypeLambda => List[TypeBounds], bodyFn: TypeLambda => TypeRepr): TypeLambda =
          TypeLambda(Raw.TypeLambda(paramNames, i => boundsFn(TypeLambda(i)).map(_.raw), i => bodyFn(TypeLambda(i)).raw))

        // def unapply(x: TypeLambda): (List[String], List[TypeBounds], TypeRepr)

      }

    }

    final case class ConstantType(raw: Raw.ConstantType) extends TypeRepr {

      def constant: Constant = Constant(raw.constant)

    }
    object ConstantType {

      def apply(x: Constant): ConstantType =
        ConstantType(Raw.ConstantType(x.raw))

      // def unapply(x: ConstantType): Some[Constant]

    }

    final case class SuperType(raw: Raw.SuperType) extends TypeRepr {

      def thistpe: TypeRepr = TypeRepr(raw.thistpe)

      def supertpe: TypeRepr = TypeRepr(raw.supertpe)

    }
    object SuperType {

      def apply(thistpe: TypeRepr, supertpe: TypeRepr): SuperType =
        SuperType(Raw.SuperType(thistpe.raw, supertpe.raw))

      // def unapply(x: SuperType): (TypeRepr, TypeRepr)

    }

    final case class Refinement(raw: Raw.Refinement) extends TypeRepr {

      def parent: TypeRepr = TypeRepr(raw.parent)

      def name: String = raw.name

      def info: TypeRepr = TypeRepr(raw.info)

    }
    object Refinement {

      def apply(parent: TypeRepr, name: String, info: TypeRepr): Refinement =
        Refinement(Raw.Refinement(parent.raw, name, info.raw))

      // def unapply(x: Refinement): (TypeRepr, String, TypeRepr)

    }

    final case class AppliedType(raw: Raw.AppliedType) extends TypeRepr {

      def tycon: TypeRepr = TypeRepr(raw.tycon)

      def args: List[TypeRepr] = raw.args.map(TypeRepr(_))

    }
    object AppliedType {

      /** Applied the type constructor `T` to a list of type arguments `T_1,..,T_n` to create `T[T_1,..,T_n]` */
      def apply(tycon: TypeRepr, args: List[TypeRepr]): AppliedType =
        AppliedType(Raw.AppliedType(tycon.raw, args.map(_.raw)))

      def unapply(x: AppliedType): (TypeRepr, List[TypeRepr]) = {
        val (a, b) = Raw.AppliedType.unapply(x.raw)
        (TypeRepr(a), b.map(TypeRepr(_)))
      }

    }

    final case class AnnotatedType(raw: Raw.AnnotatedType) extends TypeRepr {

      def underlying: TypeRepr = TypeRepr(raw.underlying)

      def annotation: Tree.Statement.Term = Tree.Statement.Term(raw.annotation)

    }
    object AnnotatedType {

      def apply(underlying: TypeRepr, annot: Tree.Statement.Term): AnnotatedType =
        AnnotatedType(Raw.AnnotatedType(underlying.raw, annot.raw))

      // def unapply(x: AnnotatedType): (TypeRepr, Term)

    }

    final case class MatchType(raw: Raw.MatchType) extends TypeRepr {

      def bound: TypeRepr = TypeRepr(raw.bound)

      def scrutinee: TypeRepr = TypeRepr(raw.scrutinee)

      def cases: List[TypeRepr] = raw.cases.map(TypeRepr(_))

    }
    object MatchType {

      def apply(bound: TypeRepr, scrutinee: TypeRepr, cases: List[TypeRepr]): MatchType =
        MatchType(Raw.MatchType(bound.raw, scrutinee.raw, cases.map(_.raw)))

      // def unapply(x: MatchType): (TypeRepr, TypeRepr, List[TypeRepr])

    }

    final case class ByNameType(raw: Raw.ByNameType) extends TypeRepr {

      def underlying: TypeRepr = TypeRepr(raw.underlying)

    }
    object ByNameType {

      def apply(underlying: TypeRepr): TypeRepr =
        TypeRepr(Raw.ByNameType(underlying.raw))

      // def unapply(x: ByNameType): Some[TypeRepr]

    }

    final case class ParamRef(raw: Raw.ParamRef) extends TypeRepr {

      def binder: TypeRepr = TypeRepr(raw.binder)

      def paramNum: Int = raw.paramNum

    }
    object ParamRef {

      // def unapply(x: ParamRef): (TypeRepr, Int)

    }

    final case class ThisType(raw: Raw.ThisType) extends TypeRepr {

      def tref: TypeRepr = TypeRepr(raw.tref)

    }
    object ThisType {

      // def unapply(x: ThisType): Some[TypeRepr]

    }

    final case class RecursiveThis(raw: Raw.RecursiveThis) extends TypeRepr {

      def binder: RecursiveType = RecursiveType(raw.binder)

    }
    object RecursiveThis {

      // def unapply(x: RecursiveThis): Some[RecursiveType]

    }

    final case class RecursiveType(raw: Raw.RecursiveType) extends TypeRepr {

      def underlying: TypeRepr = TypeRepr(raw.underlying)

      def recThis: RecursiveThis = RecursiveThis(raw.recThis)

    }
    object RecursiveType {

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
        RecursiveType(Raw.RecursiveType(i => parentExp(RecursiveType(i)).raw))

      // def unapply(x: RecursiveType): Some[TypeRepr]

    }

    final case class MatchCase(raw: Raw.MatchCase) extends TypeRepr {

      /** Pattern `P` of `case P => R` in a `MatchType` */
      def pattern: TypeRepr = TypeRepr(raw.pattern)

      /** RHS `R` of `case P => R` in a `MatchType` */
      def rhs: TypeRepr = TypeRepr(raw.rhs)

    }
    object MatchCase {

      /* Create match type case `case <pattern> => <rhs>` */
      def apply(pattern: TypeRepr, rhs: TypeRepr): MatchCase =
        MatchCase(Raw.MatchCase(pattern.raw, rhs.raw))

      /* Matches a match type case `case <pattern> => <rhs>` */
      // def unapply(x: MatchCase): (TypeRepr, TypeRepr)

    }

    final case class TypeBounds(raw: Raw.TypeBounds) extends TypeRepr
    object TypeBounds {

      def apply(low: TypeRepr, hi: TypeRepr): TypeBounds =
        TypeBounds(Raw.TypeBounds(low.raw, hi.raw))

      def empty: TypeBounds =
        TypeBounds(Raw.TypeBounds.empty)

      def upper(hi: TypeRepr): TypeBounds =
        TypeBounds(Raw.TypeBounds.upper(hi.raw))

      def lower(lo: TypeRepr): TypeBounds =
        TypeBounds(Raw.TypeBounds.lower(lo.raw))

      // def unapply(x: TypeBounds): (TypeRepr, TypeRepr)

    }

    final case class NoPrefix(raw: Raw.NoPrefix) extends TypeRepr
    object NoPrefix {

      // def unapply(x: NoPrefix): true

    }

  }

  sealed trait Selector {

    val raw: Raw.Selector

  }
  object Selector {

    def apply(raw: Raw.Selector): Selector = raw match
      case raw: Raw.SimpleSelector => Selector.SimpleSelector(raw)
      case raw: Raw.RenameSelector => Selector.RenameSelector(raw)
      case raw: Raw.OmitSelector   => Selector.OmitSelector(raw)
      case raw: Raw.GivenSelector  => Selector.GivenSelector(raw)

    final case class SimpleSelector(raw: Raw.SimpleSelector) extends Selector {

      def name: String = raw.name

      def namePos: Position = Position(raw.namePos)

    }
    object SimpleSelector {

      // def unapply(x: SimpleSelector): Some[String]

    }

    final case class RenameSelector(raw: Raw.RenameSelector) extends Selector {

      def fromName: String = raw.fromName

      def fromPos: Position = Position(raw.fromPos)

      def toName: String = raw.toName

      def toPos: Position = Position(raw.toPos)

    }
    object RenameSelector {

      // def unapply(x: RenameSelector): (String, String)

    }

    final case class OmitSelector(raw: Raw.OmitSelector) extends Selector {

      def name: String = raw.name

      def namePos: Position = Position(raw.namePos)

    }
    object OmitSelector {

      // def unapply(x: OmitSelector): Some[String]

    }

    final case class GivenSelector(raw: Raw.GivenSelector) extends Selector {

      def bound: Option[Tree.TypeTree] = raw.bound.map(Tree.TypeTree(_))

    }
    object GivenSelector {

      // def unapply(x: GivenSelector): Some[Option[TypeTree]]

    }

  }

  sealed trait Constant {

    val raw: Raw.Constant

    /** Returns the value of the constant */
    def value: Any = raw.value

    /** Shows the constant as a String */
    def show(using Raw.Printer[Raw.Constant]): String = raw.show

  }
  object Constant {

    def apply(raw: Raw.Constant): Constant = raw match
      case raw: Raw.BooleanConstant => Constant.BooleanConstant(raw)
      case raw: Raw.ByteConstant    => Constant.ByteConstant(raw)
      case raw: Raw.ShortConstant   => Constant.ShortConstant(raw)
      case raw: Raw.IntConstant     => Constant.IntConstant(raw)
      case raw: Raw.LongConstant    => Constant.LongConstant(raw)
      case raw: Raw.FloatConstant   => Constant.FloatConstant(raw)
      case raw: Raw.DoubleConstant  => Constant.DoubleConstant(raw)
      case raw: Raw.CharConstant    => Constant.CharConstant(raw)
      case raw: Raw.StringConstant  => Constant.StringConstant(raw)
      case raw: Raw.UnitConstant    => Constant.UnitConstant(raw)
      case raw: Raw.NullConstant    => Constant.NullConstant(raw)
      case raw: Raw.ClassOfConstant => Constant.ClassOfConstant(raw)

    final case class BooleanConstant(raw: Raw.BooleanConstant) extends Constant
    object BooleanConstant {

      /** Create a constant Boolean value */
      def apply(x: Boolean): BooleanConstant =
        BooleanConstant(Raw.BooleanConstant(x))

      /** Match Boolean value constant and extract its value */
      // def unapply(constant: BooleanConstant): Some[Boolean]

    }

    final case class ByteConstant(raw: Raw.ByteConstant) extends Constant
    object ByteConstant {

      /** Create a constant Byte value */
      def apply(x: Byte): ByteConstant =
        ByteConstant(Raw.ByteConstant(x))

      /** Match Byte value constant and extract its value */
      // def unapply(constant: ByteConstant): Some[Byte]

    }

    final case class ShortConstant(raw: Raw.ShortConstant) extends Constant
    object ShortConstant {

      /** Create a constant Short value */
      def apply(x: Short): ShortConstant =
        ShortConstant(Raw.ShortConstant(x))

      /** Match Short value constant and extract its value */
      // def unapply(constant: ShortConstant): Some[Short]

    }

    final case class IntConstant(raw: Raw.IntConstant) extends Constant
    object IntConstant {

      /** Create a constant Int value */
      def apply(x: Int): IntConstant =
        IntConstant(Raw.IntConstant(x))

      /** Match Int value constant and extract its value */
      // def unapply(constant: IntConstant): Some[Int]

    }

    final case class LongConstant(raw: Raw.LongConstant) extends Constant
    object LongConstant {

      /** Create a constant Long value */
      def apply(x: Long): LongConstant =
        LongConstant(Raw.LongConstant(x))

      /** Match Long value constant and extract its value */
      // def unapply(constant: LongConstant): Some[Long]

    }

    final case class FloatConstant(raw: Raw.FloatConstant) extends Constant
    object FloatConstant {

      /** Create a constant Float value */
      def apply(x: Float): FloatConstant =
        FloatConstant(Raw.FloatConstant(x))

      /** Match Float value constant and extract its value */
      // def unapply(constant: FloatConstant): Some[Float]

    }

    final case class DoubleConstant(raw: Raw.DoubleConstant) extends Constant
    object DoubleConstant {

      /** Create a constant Double value */
      def apply(x: Double): DoubleConstant =
        DoubleConstant(Raw.DoubleConstant(x))

      /** Match Double value constant and extract its value */
      // def unapply(constant: DoubleConstant): Some[Double]

    }

    final case class CharConstant(raw: Raw.CharConstant) extends Constant
    object CharConstant {

      /** Create a constant Char value */
      def apply(x: Char): CharConstant =
        CharConstant(Raw.CharConstant(x))

      /** Match Char value constant and extract its value */
      // def unapply(constant: CharConstant): Some[Char]

    }

    final case class StringConstant(raw: Raw.StringConstant) extends Constant
    object StringConstant {

      /** Create a constant String value */
      def apply(x: String): StringConstant =
        StringConstant(Raw.StringConstant(x))

      /** Match String value constant and extract its value */
      // def unapply(constant: StringConstant): Some[String]

    }

    final case class UnitConstant(raw: Raw.UnitConstant) extends Constant
    object UnitConstant {

      /** Create a constant Unit value */
      def apply(): UnitConstant =
        UnitConstant(Raw.UnitConstant())

      /** Match Unit value constant */
      // def unapply(constant: UnitConstant): true

    }

    final case class NullConstant(raw: Raw.NullConstant) extends Constant
    object NullConstant {

      /** Create a constant Null value */
      def apply(): NullConstant =
        NullConstant(Raw.NullConstant())

      /** Match Null value constant */
      // def unapply(constant: NullConstant): true

    }

    final case class ClassOfConstant(raw: Raw.ClassOfConstant) extends Constant
    object ClassOfConstant {

      /** Create a constant class value representing `classOf[<tpe>]` */
      def apply(tpe: TypeRepr): ClassOfConstant =
        ClassOfConstant(Raw.ClassOfConstant(tpe.raw))

      /** Match a class value constant representing `classOf[<tpe>]` and extract its type */
      // def unapply(constant: ClassOfConstant): Option[TypeRepr]

    }

  }

  final case class Signature(raw: Raw.Signature) {

    /**
      * The signatures of the method parameters.
      *
      *  Each *type parameter section* is represented by a single Int corresponding
      *  to the number of type parameters in the section.
      *  Each *term parameter* is represented by a String corresponding to the fully qualified
      *  name of the parameter type.
      */
    def paramSigs: List[String | Int] = raw.paramSigs

    /** The signature of the result type */
    def resultSig: String = raw.resultSig

  }
  object Signature {

    /** Matches the method signature and returns its parameters and result type. */
    // def unapply(sig: Signature): (List[String | Int], String)

  }

  final case class Position(raw: Raw.Position) {

    /** The start offset in the source file */
    def start: Int = raw.start

    /** The end offset in the source file */
    def end: Int = raw.end

    /** Source file in which this position is located */
    def sourceFile: SourceFile = SourceFile(raw.sourceFile)

    /** The start line in the source file */
    def startLine: Int = raw.startLine

    /** The end line in the source file */
    def endLine: Int = raw.endLine

    /** The start column in the source file */
    def startColumn: Int = raw.startColumn

    /** The end column in the source file */
    def endColumn: Int = raw.endColumn

    /** Source code within the position */
    def sourceCode: Option[String] = raw.sourceCode

  }
  object Position {

    /** Position of the expansion site of the macro */
    def ofMacroExpansion: Position =
      Position(Raw.Position.ofMacroExpansion)

    /** Create a new position in the source with the given range. The range must be contained in the file. */
    def apply(sourceFile: SourceFile, start: Int, end: Int): Position =
      Position(Raw.Position(sourceFile.raw, start, end))

  }

  final case class SourceFile(raw: Raw.SourceFile) {

    /** Path to this source file. May be `None` for virtual files such as in the REPL. */
    def getJPath: Option[java.nio.file.Path] = raw.getJPath

    /** Name of the source file */
    def name: String = raw.name

    /**
      * Path of the source file.
      *
      *  It does not necessarily point to a path in the filesystem, it could be the path of a virtual file.
      *  Use `getJPath` to get paths to the filesystem.
      */
    def path: String = raw.path

    /** Content of this source file */
    def content: Option[String] = raw.content

  }
  object SourceFile {

    /** Returns the source file being compiled. The path is relative to the current working directory. */
    def current: SourceFile =
      SourceFile(Raw.SourceFile.current)

  }

  final case class Symbol(raw: Raw.Symbol) {

    /** Owner of this symbol. The owner is the symbol in which this symbol is defined. Throws if this symbol does not have an owner. */
    def owner: Symbol = Symbol(raw.owner)

    /** Owner of this symbol. The owner is the symbol in which this symbol is defined. Returns `NoSymbol` if this symbol does not have an owner. */
    def maybeOwner: Symbol = Symbol(raw.maybeOwner)

    /** Flags of this symbol */
    def flags: Flags = Flags(raw.flags)

    /** This symbol is private within the resulting type */
    def privateWithin: Option[TypeRepr] = raw.privateWithin.map(TypeRepr(_))

    /** This symbol is protected within the resulting type */
    def protectedWithin: Option[TypeRepr] = raw.protectedWithin.map(TypeRepr(_))

    /** The name of this symbol */
    def name: String = raw.name

    /** The full name of this symbol up to the root package */
    def fullName: String = raw.fullName

    /** Type of the definition */
    @experimental def info: TypeRepr = TypeRepr(raw.info)

    /** The position of this symbol */
    def pos: Option[Position] = raw.pos.map(Position(_))

    /** The documentation for this symbol, if any */
    def docstring: Option[String] = raw.docstring

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
    def tree: Tree = Tree(raw.tree)

    /** Is the annotation defined with `annotSym` attached to this symbol? */
    def hasAnnotation(annotSym: Symbol): Boolean = raw.hasAnnotation(annotSym.raw)

    /** Get the annotation defined with `annotSym` attached to this symbol */
    def getAnnotation(annotSym: Symbol): Option[Tree.Statement.Term] = raw.getAnnotation(annotSym.raw).map(Tree.Statement.Term(_))

    /** Annotations attached to this symbol */
    def annotations: List[Tree.Statement.Term] = raw.annotations.map(Tree.Statement.Term(_))

    /** Does this symbol come from a currently compiled source file? */
    def isDefinedInCurrentRun: Boolean = raw.isDefinedInCurrentRun

    /**
      * Dummy val symbol that owns all statements within the initialization of the class.
      *  This may also contain local definitions such as classes defined in a `locally` block in the class.
      */
    def isLocalDummy: Boolean = raw.isLocalDummy

    /** Is this symbol a class representing a refinement? */
    def isRefinementClass: Boolean = raw.isRefinementClass

    /** Is this symbol an alias type? */
    def isAliasType: Boolean = raw.isAliasType

    /** Is this symbol an anonymous class? */
    def isAnonymousClass: Boolean = raw.isAnonymousClass

    /** Is this symbol an anonymous function? */
    def isAnonymousFunction: Boolean = raw.isAnonymousFunction

    /** Is this symbol an abstract type? */
    def isAbstractType: Boolean = raw.isAbstractType

    /** Is this the constructor of a class? */
    def isClassConstructor: Boolean = raw.isClassConstructor

    /** Is this the definition of a type? */
    def isType: Boolean = raw.isType

    /** Is this the definition of a term? */
    def isTerm: Boolean = raw.isTerm

    /** Is this the definition of a PackageDef tree? */
    def isPackageDef: Boolean = raw.isPackageDef

    /** Is this the definition of a ClassDef tree? */
    def isClassDef: Boolean = raw.isClassDef

    /** Is this the definition of a TypeDef tree */
    def isTypeDef: Boolean = raw.isTypeDef

    /** Is this the definition of a ValDef tree? */
    def isValDef: Boolean = raw.isValDef

    /** Is this the definition of a DefDef tree? */
    def isDefDef: Boolean = raw.isDefDef

    /** Is this the definition of a Bind pattern? */
    def isBind: Boolean = raw.isBind

    /** Does this symbol represent a no definition? */
    def isNoSymbol: Boolean = raw.isNoSymbol

    /** Does this symbol represent a definition? */
    def exists: Boolean = raw.exists

    /** Field with the given name directly declared in the class */
    def declaredField(name: String): Symbol = Symbol(raw.declaredField(name))

    /** Fields directly declared in the class */
    def declaredFields: List[Symbol] = raw.declaredFields.map(Symbol(_))

    /** Get named non-private fields declared or inherited */
    def fieldMember(name: String): Symbol = Symbol(raw.fieldMember(name))

    /** Get all non-private fields declared or inherited */
    def fieldMembers: List[Symbol] = raw.fieldMembers.map(Symbol(_))

    /** Get non-private named methods defined directly inside the class */
    def declaredMethod(name: String): List[Symbol] = raw.declaredMethod(name).map(Symbol(_))

    /** Get all non-private methods defined directly inside the class, excluding constructors */
    def declaredMethods: List[Symbol] = raw.declaredMethods.map(Symbol(_))

    /** Get named non-private methods declared or inherited */
    def methodMember(name: String): List[Symbol] = raw.methodMember(name).map(Symbol(_))

    /** Get all non-private methods declared or inherited */
    def methodMembers: List[Symbol] = raw.methodMembers.map(Symbol(_))

    /** Get non-private named type defined directly inside the class */
    def declaredType(name: String): List[Symbol] = raw.declaredType(name).map(Symbol(_))

    /** Get all non-private types defined directly inside the class */
    def declaredTypes: List[Symbol] = raw.declaredTypes.map(Symbol(_))

    /** Type member with the given name declared or inherited in the class */
    def typeMember(name: String): Symbol = Symbol(raw.typeMember(name))

    /** Type member directly declared or inherited in the class */
    def typeMembers: List[Symbol] = raw.typeMembers.map(Symbol(_))

    /** All members directly declared in the class */
    def declarations: List[Symbol] = raw.declarations.map(Symbol(_))

    /**
      * The symbols of each type parameter list and value parameter list of this
      *  method, or Nil if this isn't a method.
      */
    def paramSymss: List[List[Symbol]] = raw.paramSymss.map(_.map(Symbol(_)))

    /** Returns all symbols overridden by this symbol. */
    def allOverriddenSymbols: Iterator[Symbol] = raw.allOverriddenSymbols.map(Symbol(_))

    /**
      * The symbol overriding this symbol in given subclass `ofclazz`.
      *
      *  @param ofclazz is a subclass of this symbol's owner
      */
    def overridingSymbol(ofclazz: Symbol): Symbol = Symbol(raw.overridingSymbol(ofclazz.raw))

    /** The primary constructor of a class or trait, `noSymbol` if not applicable. */
    def primaryConstructor: Symbol = Symbol(raw.primaryConstructor)

    /** Fields of a case class type -- only the ones declared in primary constructor */
    def caseFields: List[Symbol] = raw.caseFields.map(Symbol(_))

    def isTypeParam: Boolean = raw.isTypeParam

    /** Signature of this definition */
    def signature: Signature = Signature(raw.signature)

    /** The class symbol of the companion module class */
    def moduleClass: Symbol = Symbol(raw.moduleClass)

    /** The symbol of the companion class */
    def companionClass: Symbol = Symbol(raw.companionClass)

    /** The symbol of the companion module */
    def companionModule: Symbol = Symbol(raw.companionModule)

    /** Case class or case object children of a sealed trait or cases of an `enum`. */
    def children: List[Symbol] = raw.children.map(Symbol(_))

    /**
      * Returns a nested quote with this symbol as splice owner (`Symbol.spliceOwner`).
      *
      *  Changes the owner under which the definition in a quote are created.
      *
      *  Usages:
      *  ```scala
      *  def rhsExpr(using q: Quotes): Expr[Unit] =
      *    import q.reflect._
      *    '{ val y = ???; (y, y) }
      *  def aValDef(using q: Quotes)(owner: q.reflect.Symbol) =
      *    import q.reflect._
      *    val sym = Symbol.newVal(owner, "x", TypeRepr.of[Unit], Flags.EmptyFlags, Symbol.noSymbol)
      *    val rhs = rhsExpr(using sym.asQuotes).asTerm
      *    ValDef(sym, Some(rhs))
      *  ```
      *
      *  ```scala
      *  //{
      *  def inQuotes(using q: Quotes) = {
      *    import q.reflect._
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
    def asQuotes: Nested = raw.asQuotes

    /**
      * Type reference to the symbol usable in the scope of its owner.
      *
      *  To get a reference to a symbol from a specific prefix `tp`, use `tp.select(symbol)` instead.
      *  @see TypeReprMethods.select
      *
      *  @pre symbol.isType returns true
      */
    def typeRef: TypeRepr.NamedType.TypeRef = TypeRepr.NamedType.TypeRef(raw.typeRef)

    /**
      * Term reference to the symbol usable in the scope of its owner.
      *
      *  @pre symbol.isType returns false
      */
    def termRef: TypeRepr.NamedType.TermRef = TypeRepr.NamedType.TermRef(raw.termRef)
    def toTerm: Tree.Statement.Term = this.termRef.toTerm

    def typeTree: Tree.TypeTree =
      Tree.TypeTree.ref(this)

    def typeType: Option[Symbol.TypeType] = Symbol.TypeType.fromSym(this)
    def typeTypeSealed: Option[Symbol.TypeType.Sealed] = Symbol.TypeType.Sealed.fromSym(this)
    def typeTypeCase: Option[Symbol.TypeType.Case] = Symbol.TypeType.Case.fromSym(this)

    def optionalAnnotation[Annot: Type]: Option[Expr[Annot]] = {
      val annotTpe = TypeRepr.of[Annot]
      val annotFlags = annotTpe.typeSymbol.flags

      if (annotFlags.is(Flags.Abstract) || annotFlags.is(Flags.Trait))
        report.errorAndAbort(s"Bad annotation type ${annotTpe.show} is abstract")

      this.getAnnotation(annotTpe.typeSymbol) match
        case Some(tree) if tree.tpe <:< annotTpe => tree.asExprOf[Annot].some
        case _                                   => None
    }

    def requiredAnnotation[Annot: Type]: Expr[Annot] =
      optionalAnnotation[Annot].getOrElse(report.errorAndAbort(s"Missing required annotation `${TypeRepr.of[Annot].show}` for `$this`"))

    def optionalAnnotationValue[Annot: {Type, FromExpr}]: Option[Annot] =
      optionalAnnotation[Annot].map { expr =>
        expr.value.getOrElse(report.errorAndAbort(s"Found annotation `${TypeRepr.of[Annot].show}` for `$this`, but are unable to extract Expr.value\n${expr.show}"))
      }

    def requiredAnnotationValue[Annot: {Type, FromExpr}]: Annot = {
      val expr = requiredAnnotation[Annot]
      expr.value.getOrElse(report.errorAndAbort(s"Found annotation `${TypeRepr.of[Annot].show}` for `$this`, but are unable to extract Expr.value\n${expr.show}"))
    }

  }
  object Symbol {

    sealed abstract class TypeType(final val isScala2: Boolean, final val isEnum: Boolean) {

      final def toSealed: Option[TypeType.Sealed] = this match
        case self: TypeType.Sealed => self.some
        case _: TypeType.Case      => None

      final def toCase: Option[TypeType.Case] = this match
        case self: TypeType.Case => self.some
        case _: TypeType.Sealed  => None

    }
    object TypeType {

      sealed abstract class Sealed(isScala2: Boolean, isEnum: Boolean) extends TypeType(isScala2, isEnum)
      object Sealed {
        def fromSym(sym: Symbol): Option[TypeType.Sealed] = TypeType.fromSym(sym).flatMap(_.toSealed)
      }

      sealed abstract class Case(isScala2: Boolean, isEnum: Boolean, final val isObject: Boolean) extends TypeType(isScala2, isEnum)
      object Case {
        def fromSym(sym: Symbol): Option[TypeType.Case] = TypeType.fromSym(sym).flatMap(_.toCase)
      }

      case object SealedTrait extends TypeType.Sealed(false, false)
      case object SealedAbstractClass extends TypeType.Sealed(false, false)
      case object SealedEnum extends TypeType.Sealed(false, true)
      case object Scala2SealedTrait extends TypeType.Sealed(true, false)
      case object Scala2SealedAbstractClass extends TypeType.Sealed(true, false)

      case object CaseClass extends TypeType.Case(false, false, false)
      case object CaseObject extends TypeType.Case(false, false, true)
      case object EnumCaseClass extends TypeType.Case(false, true, false)
      case object EnumCaseObject extends TypeType.Case(false, true, true)
      case object Scala2CaseClass extends TypeType.Case(true, false, false)
      case object Scala2CaseObject extends TypeType.Case(true, false, true)

      def fromSym(sym: Symbol): Option[TypeType] = {
        val flags = sym.flags

        (
          flags.is(Flags.Scala2x),
          flags.is(Flags.Enum),
          flags.is(Flags.Case),
          flags.is(Flags.Sealed),
          flags.is(Flags.Trait),
          flags.is(Flags.NoInits),
          flags.is(Flags.StableRealizable),
          flags.is(Flags.Module),
        ) match {
          // scala 3
          case (false, false, true, false, false, true, false, false) => CaseClass.some
          case (false, false, true, false, false, false, true, true)  => CaseObject.some
          case (false, true, true, false, false, true, false, false)  => EnumCaseClass.some
          case (false, true, true, false, false, false, true, false)  => EnumCaseObject.some
          case (false, false, false, true, true, _, false, false)     => SealedTrait.some
          case (false, false, false, true, false, _, false, false)    => SealedAbstractClass.some
          case (false, true, false, true, false, _, false, false)     => SealedEnum.some

          // scala 2
          case (true, false, true, false, false, false, false, false) => Scala2CaseClass.some
          case (true, false, true, false, false, false, false, true)  => Scala2CaseObject.some
          case (true, false, false, true, false, false, false, false) => Scala2SealedAbstractClass.some
          case (true, false, false, true, true, false, false, false)  => Scala2SealedTrait.some

          case _ => None
        }
      }

    }

    /**
      * Symbol of the definition that encloses the current splicing context.
      *
      *  For example, the following call to `spliceOwner` would return the symbol `x`.
      *  ```scala sc:nocompile
      *  val x = ${ ... Symbol.spliceOwner ... }
      *  ```
      *
      *  For a macro splice, it is the symbol of the definition where the macro expansion happens.
      */
    def spliceOwner: Symbol =
      Symbol(Raw.Symbol.spliceOwner)

    /** Get package symbol if package is either defined in current compilation run or present on classpath. */
    def requiredPackage(path: String): Symbol =
      Symbol(Raw.Symbol.requiredPackage(path))

    /** Get class symbol if class is either defined in current compilation run or present on classpath. */
    def requiredClass(path: String): Symbol =
      Symbol(Raw.Symbol.requiredClass(path))

    /** Get module symbol if module is either defined in current compilation run or present on classpath. */
    def requiredModule(path: String): Symbol =
      Symbol(Raw.Symbol.requiredModule(path))

    /** Get method symbol if method is either defined in current compilation run or present on classpath. Throws if the method has an overload. */
    def requiredMethod(path: String): Symbol =
      Symbol(Raw.Symbol.requiredMethod(path))

    /** The class Symbol of a global class definition */
    def classSymbol(fullName: String): Symbol =
      Symbol(Raw.Symbol.classSymbol(fullName))

    /**
      * Generates a new class symbol for a class with a parameterless constructor.
      *
      *  Example usage:
      *  ```
      *  val name: String = "myClass"
      *  val parents = List(TypeTree.of[Object], TypeTree.of[Foo])
      *  def decls(cls: Symbol): List[Symbol] =
      *    List(Symbol.newMethod(cls, "foo", MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit])))
      *
      *  val cls = Symbol.newClass(Symbol.spliceOwner, name, parents = parents.map(_.tpe), decls, selfType = None)
      *  val fooSym = cls.declaredMethod("foo").head
      *
      *  val fooDef = DefDef(fooSym, argss => Some('{println(s"Calling foo")}.asTerm))
      *  val clsDef = ClassDef(cls, parents, body = List(fooDef))
      *  val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[Foo])
      *
      *  Block(List(clsDef), newCls).asExprOf[Foo]
      *  ```
      *  constructs the equivalent to
      *   ```
      *  '{
      *    class myClass() extends Object with Foo {
      *      def foo(): Unit = println("Calling foo")
      *    }
      *    new myClass(): Foo
      *  }
      *  ```
      *
      *  @param parent The owner of the class
      *  @param name The name of the class
      *  @param parents The parent classes of the class. The first parent must not be a trait.
      *  @param decls The member declarations of the class provided the symbol of this class
      *  @param selfType The self type of the class if it has one
      *
      *  This symbol starts without an accompanying definition.
      *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
      *  this symbol to the ClassDef constructor.
      *
      *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
      *        direct or indirect children of the reflection context's owner.
      */
    // TODO: add flags and privateWithin
    @experimental def newClass(parent: Symbol, name: String, parents: List[TypeRepr], decls: Symbol => List[Symbol], selfType: Option[TypeRepr]): Symbol =
      Symbol(Raw.Symbol.newClass(parent.raw, name, parents.map(_.raw), i => decls(Symbol(i)).map(_.raw), selfType.map(_.raw)))

    /**
      * Generates a new module symbol with an associated module class symbol,
      *  this is equivalent to an `object` declaration in source code.
      *  This method returns the module symbol. The module class can be accessed calling `moduleClass` on this symbol.
      *
      *  Example usage:
      *  ```scala
      *  //{
      *  given Quotes = ???
      *  import quotes.reflect._
      *  //}
      *  val moduleName: String = Symbol.freshName("MyModule")
      *  val parents = List(TypeTree.of[Object])
      *  def decls(cls: Symbol): List[Symbol] =
      *    List(Symbol.newMethod(cls, "run", MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit]), Flags.EmptyFlags, Symbol.noSymbol))
      *
      *  val mod = Symbol.newModule(Symbol.spliceOwner, moduleName, Flags.EmptyFlags, Flags.EmptyFlags, parents.map(_.tpe), decls, Symbol.noSymbol)
      *  val cls = mod.moduleClass
      *  val runSym = cls.declaredMethod("run").head
      *
      *  val runDef = DefDef(runSym, _ => Some('{ println("run") }.asTerm))
      *  val modDef = ClassDef.module(mod, parents, body = List(runDef))
      *
      *  val callRun = Apply(Select(Ref(mod), runSym), Nil)
      *
      *  Block(modDef.toList, callRun)
      *  ```
      *  constructs the equivalent to
      *  ```scala
      *  //{
      *  given Quotes = ???
      *  import quotes.reflect._
      *  //}
      *  '{
      *    object MyModule$macro$1 extends Object:
      *      def run(): Unit = println("run")
      *    MyModule$macro$1.run()
      *  }
      *  ```
      *
      *  @param parent The owner of the class
      *  @param name The name of the class
      *  @param modFlags extra flags with which the module symbol should be constructed
      *  @param clsFlags extra flags with which the module class symbol should be constructed
      *  @param parents The parent classes of the class. The first parent must not be a trait.
      *  @param decls A function that takes the symbol of the module class as input and return the symbols of its declared members
      *  @param privateWithin the symbol within which this new method symbol should be private. May be noSymbol.
      *
      *  This symbol starts without an accompanying definition.
      *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
      *  this symbol to `ClassDef.module`.
      *
      *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
      *        direct or indirect children of the reflection context's owner.
      *
      *  @syntax markdown
      */
    @experimental def newModule(owner: Symbol, name: String, modFlags: Flags, clsFlags: Flags, parents: List[TypeRepr], decls: Symbol => List[Symbol], privateWithin: Symbol): Symbol =
      Symbol(Raw.Symbol.newModule(owner.raw, name, modFlags.raw, clsFlags.raw, parents.map(_.raw), i => decls(Symbol(i)).map(_.raw), privateWithin.raw))

    /**
      * Generates a new method symbol with the given parent, name and type.
      *
      *  To define a member method of a class, use the `newMethod` within the `decls` function of `newClass`.
      *
      *  @param parent The owner of the method
      *  @param name The name of the method
      *  @param tpe The type of the method (MethodType, PolyType, ByNameType)
      *
      *  This symbol starts without an accompanying definition.
      *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
      *  this symbol to the DefDef constructor.
      *
      *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
      *        direct or indirect children of the reflection context's owner.
      */
    def newMethod(parent: Symbol, name: String, tpe: TypeRepr): Symbol =
      Symbol(Raw.Symbol.newMethod(parent.raw, name, tpe.raw))

    /**
      * Works as the other newMethod, but with additional parameters.
      *
      *  To define a member method of a class, use the `newMethod` within the `decls` function of `newClass`.
      *
      *  @param parent The owner of the method
      *  @param name The name of the method
      *  @param tpe The type of the method (MethodType, PolyType, ByNameType)
      *  @param flags extra flags to with which the symbol should be constructed. `Method` flag will be added. Can be `Private | Protected | Override | Deferred | Final | Method | Implicit | Given | Local | JavaStatic`
      *  @param privateWithin the symbol within which this new method symbol should be private. May be noSymbol.
      */
    // Keep: `flags` doc aligned with QuotesImpl's `validMethodFlags`
    def newMethod(parent: Symbol, name: String, tpe: TypeRepr, flags: Flags, privateWithin: Symbol): Symbol =
      Symbol(Raw.Symbol.newMethod(parent.raw, name, tpe.raw, flags.raw, privateWithin.raw))

    /**
      * Generates a new val/var/lazy val symbol with the given parent, name and type.
      *
      *  This symbol starts without an accompanying definition.
      *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
      *  this symbol to the ValDef constructor.
      *
      *  Note: Also see ValDef.let
      *
      *  @param parent The owner of the val/var/lazy val
      *  @param name The name of the val/var/lazy val
      *  @param tpe The type of the val/var/lazy val
      *  @param flags extra flags to with which the symbol should be constructed. Can be `Private | Protected | Override | Deferred | Final | Param | Implicit | Lazy | Mutable | Local | ParamAccessor | Module | Package | Case | CaseAccessor | Given | Enum | JavaStatic`
      *  @param privateWithin the symbol within which this new method symbol should be private. May be noSymbol.
      *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
      *        direct or indirect children of the reflection context's owner.
      */
    // Keep: `flags` doc aligned with QuotesImpl's `validValFlags`
    def newVal(parent: Symbol, name: String, tpe: TypeRepr, flags: Flags, privateWithin: Symbol): Symbol =
      Symbol(Raw.Symbol.newVal(parent.raw, name, tpe.raw, flags.raw, privateWithin.raw))

    /**
      * Generates a pattern bind symbol with the given parent, name and type.
      *
      *  This symbol starts without an accompanying definition.
      *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
      *  this symbol to the BindDef constructor.
      *
      *  @param parent The owner of the binding
      *  @param name The name of the binding
      *  @param flags extra flags to with which the symbol should be constructed. `Case` flag will be added. Can be `Case`
      *  @param tpe The type of the binding
      *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
      *        direct or indirect children of the reflection context's owner.
      */
    // Keep: `flags` doc aligned with QuotesImpl's `validBindFlags`
    def newBind(parent: Symbol, name: String, flags: Flags, tpe: TypeRepr): Symbol =
      Symbol(Raw.Symbol.newBind(parent.raw, name, flags.raw, tpe.raw))

    /** Definition not available */
    def noSymbol: Symbol =
      Symbol(Raw.Symbol.noSymbol)

    /**
      * A fresh name for class or member symbol names.
      *
      *  Fresh names are constructed using the following format `prefix + "$macro$" + freshIndex`.
      *  The `freshIndex` are unique within the current source file.
      *
      *  Examples: See `scala.annotation.MacroAnnotation`
      *
      *  @param prefix Prefix of the fresh name
      */
    @experimental def freshName(prefix: String): String =
      Raw.Symbol.freshName(prefix)

  }

  final case class Flags(raw: Raw.Flags) {

    /** Is the given flag set a subset of this flag sets */
    def is(that: Flags): Boolean = raw.is(that.raw)

    /** Union of the two flag sets */
    def |(that: Flags): Flags = Flags(this.raw | that.raw)

    /** Intersection of the two flag sets */
    def &(that: Flags): Flags = Flags(this.raw & that.raw)

    /** Shows the flags as a String */
    def show: String = raw.show

  }
  object Flags {

    /** Is this symbol `abstract` */
    def Abstract: Flags = Flags(Raw.Flags.Abstract)

    /**
      * Is this an abstract override method?
      *
      *  This corresponds to a definition declared as "abstract override def" in the source.
      * See https://stackoverflow.com/questions/23645172/why-is-abstract-override-required-not-override-alone-in-subtrait for examples.
      */
    // @experimental def AbsOverride: Flags = Flags(Raw.Flags.AbsOverride)

    /**
      * Is this generated by Scala compiler.
      *  Corresponds to ACC_SYNTHETIC in the JVM.
      */
    def Artifact: Flags = Flags(Raw.Flags.Artifact)

    /** Is this symbol `case` */
    def Case: Flags = Flags(Raw.Flags.Case)

    /** Is this symbol a getter for case class parameter */
    def CaseAccessor: Flags = Flags(Raw.Flags.CaseAccessor)

    /** Is this symbol a type parameter marked as contravariant `-` */
    def Contravariant: Flags = Flags(Raw.Flags.Contravariant)

    /** Is this symbol a type parameter marked as covariant `+` */
    def Covariant: Flags = Flags(Raw.Flags.Covariant)

    /** Is a declared, but not defined member */
    def Deferred: Flags = Flags(Raw.Flags.Deferred)

    /** The empty set of flags */
    def EmptyFlags: Flags = Flags(Raw.Flags.EmptyFlags)

    /** Is this symbol an enum */
    def Enum: Flags = Flags(Raw.Flags.Enum)

    /** Is this symbol `erased` */
    def Erased: Flags = Flags(Raw.Flags.Erased)

    /** Is this symbol exported from provided instance */
    def Exported: Flags = Flags(Raw.Flags.Exported)

    /** Is this symbol a `def` defined in an `extension` */
    def ExtensionMethod: Flags = Flags(Raw.Flags.ExtensionMethod)

    /** Is this symbol a getter or a setter */
    def FieldAccessor: Flags = Flags(Raw.Flags.FieldAccessor)

    /** Is this symbol `final` */
    def Final: Flags = Flags(Raw.Flags.Final)

    /** Is this symbol an inferable ("given") parameter */
    def Given: Flags = Flags(Raw.Flags.Given)

    /** Is this symbol a parameter with a default value? */
    def HasDefault: Flags = Flags(Raw.Flags.HasDefault)

    /** Is this symbol `implicit` */
    def Implicit: Flags = Flags(Raw.Flags.Implicit)

    /** Is an infix method or type */
    def Infix: Flags = Flags(Raw.Flags.Infix)

    /** Is this symbol `inline` */
    def Inline: Flags = Flags(Raw.Flags.Inline)

    /** Is this symbol invisible when typechecking? */
    def Invisible: Flags = Flags(Raw.Flags.Invisible)

    /** Is this symbol defined in a Java class */
    def JavaDefined: Flags = Flags(Raw.Flags.JavaDefined)

    /** Is implemented as a Java static */
    def JavaStatic: Flags = Flags(Raw.Flags.JavaStatic)

    /** Is this an annotation defined in Java */
    @experimental def JavaAnnotation: Flags = Flags(Raw.Flags.JavaAnnotation)

    /** Is this symbol `lazy` */
    def Lazy: Flags = Flags(Raw.Flags.Lazy)

    /** Is this symbol local? Used in conjunction with private/private[T] to mean private[this] extends Modifier protected[this] */
    def Local: Flags = Flags(Raw.Flags.Local)

    /** Is this symbol marked as a macro. An inline method containing top level splices */
    def Macro: Flags = Flags(Raw.Flags.Macro)

    /** Is this symbol `def` */
    def Method: Flags = Flags(Raw.Flags.Method)

    /** Is this symbol an object or its class (used for a ValDef or a ClassDef extends Modifier respectively) */
    def Module: Flags = Flags(Raw.Flags.Module)

    /** Is this symbol a `var` (when used on a ValDef) */
    def Mutable: Flags = Flags(Raw.Flags.Mutable)

    /** Trait does not have fields or initialization code. */
    def NoInits: Flags = Flags(Raw.Flags.NoInits)

    /** Is this symbol `opaque` */
    def Opaque: Flags = Flags(Raw.Flags.Opaque)

    /** Is this symbol `open` */
    def Open: Flags = Flags(Raw.Flags.Open)

    /** Is this symbol `override` */
    def Override: Flags = Flags(Raw.Flags.Override)

    /** Is this symbol a package */
    def Package: Flags = Flags(Raw.Flags.Package)

    /** Is this symbol a parameter */
    def Param: Flags = Flags(Raw.Flags.Param)

    /** Is this symbol a parameter accessor */
    def ParamAccessor: Flags = Flags(Raw.Flags.ParamAccessor)

    /** Is this symbol `private` */
    def Private: Flags = Flags(Raw.Flags.Private)

    /** Is this symbol labeled private[this] */
    def PrivateLocal: Flags = Flags(Raw.Flags.PrivateLocal)

    /** Is this symbol `protected` */
    def Protected: Flags = Flags(Raw.Flags.Protected)

    /** Was this symbol imported from Scala2.x */
    def Scala2x: Flags = Flags(Raw.Flags.Scala2x)

    /** Is this symbol `sealed` */
    def Sealed: Flags = Flags(Raw.Flags.Sealed)

    /** Is this symbol member that is assumed to be stable and realizable */
    def StableRealizable: Flags = Flags(Raw.Flags.StableRealizable)

    /** Is this symbol to be tagged Java Synthetic */
    def Synthetic: Flags = Flags(Raw.Flags.Synthetic)

    /** Is this symbol a trait */
    def Trait: Flags = Flags(Raw.Flags.Trait)

    /** Is a transparent inline method or trait */
    def Transparent: Flags = Flags(Raw.Flags.Transparent)

  }

  type Nested = Quotes

  object report {

    /** Report an error at the position of the macro expansion */
    def error(msg: String): Unit =
      Raw.report.error(msg)

    /** Report an error at the position of `expr` */
    def error(msg: String, expr: Expr[Any]): Unit =
      Raw.report.error(msg, expr)

    /** Report an error message at the given position */
    def error(msg: String, pos: Position): Unit =
      Raw.report.error(msg, pos.raw)

    /** Report an error at the position of the macro expansion and throw a StopMacroExpansion */
    def errorAndAbort(msg: String): Nothing =
      Raw.report.errorAndAbort(msg)

    /** Report an error at the position of `expr` and throw a StopMacroExpansion */
    def errorAndAbort(msg: String, expr: Expr[Any]): Nothing =
      Raw.report.errorAndAbort(msg, expr)

    /** Report an error message at the given position and throw a StopMacroExpansion */
    def errorAndAbort(msg: String, pos: Position): Nothing =
      Raw.report.errorAndAbort(msg, pos.raw)

    /** Report a warning at the position of the macro expansion */
    def warning(msg: String): Unit =
      Raw.report.warning(msg)

    /** Report a warning at the position of `expr` */
    def warning(msg: String, expr: Expr[Any]): Unit =
      Raw.report.warning(msg, expr)

    /** Report a warning message at the given position */
    def warning(msg: String, pos: Position): Unit =
      Raw.report.warning(msg, pos.raw)

    /** Report an info at the position of the macro expansion */
    def info(msg: String): Unit =
      Raw.report.info(msg)

    /** Report an info message at the position of `expr` */
    def info(msg: String, expr: Expr[Any]): Unit =
      Raw.report.info(msg, expr)

    /** Report an info message at the given position */
    def info(msg: String, pos: Position): Unit =
      Raw.report.info(msg, pos.raw)

  }

  // =====|  |=====

  object internal {

    final case class Typed(raw: Raw.Typed) extends Tree.Statement.Term with Tree.TypedOrTest
    object Typed {

      /** Create a type ascription `<x: Term>: <tpt: TypeTree>` */
      def apply(expr: Tree.Statement.Term, tpt: Tree.TypeTree): Typed =
        Typed(Raw.Typed(expr.raw, tpt.raw))

      def copy(original: Tree)(expr: Tree.Statement.Term, tpt: Tree.TypeTree): Typed =
        Typed(Raw.Typed.copy(original.raw)(expr.raw, tpt.raw))

      /** Matches `<expr: Term>: <tpt: TypeTree>` */
      // def unapply(x: Typed): (Term, TypeTree)

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // TODO (KR) : Raw.defn

  object defn {

    lazy val emptyTuple: TypeRepr = TypeRepr.ofType[EmptyTuple]
    lazy val tupleAppend: TypeRepr = TypeRepr.ofType[? *: ?].asInstanceOf[TypeRepr.AppliedType].tycon

    def TupleClass(arity: Int): Symbol = Symbol(Raw.defn.TupleClass(arity))

    def isTupleClass(sym: Symbol): Boolean = Raw.defn.isTupleClass(sym.raw)

  }

  object unapplies {

    // Note: This will not look for `(A, B)`, only `A *: B *: EmptyTuple
    object tuple {

      /**
        * If `tpe` is a tuple, returns the list of types of the tuple elements.
        */
      def unapply(tpe: TypeRepr): Option[List[TypeRepr]] = tpe match
        case _ if tpe.classSymbol == defn.emptyTuple.classSymbol                                              => Nil.some
        case TypeRepr.AppliedType(root, List(head, tail)) if root.classSymbol == defn.tupleAppend.classSymbol => unapply(tail).map(head :: _)
        case TypeRepr.AppliedType(root, args) if defn.isTupleClass(root.typeSymbol)                           => args.some
        case _                                                                                                => None

    }

    // Note: This will not look for `(A, B)`, only `A *: B *: EmptyTuple
    object nonEmptyTuple {

      def unapply(tpe: TypeRepr): Option[NonEmptyList[TypeRepr]] =
        tuple.unapply(tpe).flatMap(NonEmptyList.fromList(_))

    }

    // TODO (KR) : or

    // TODO (KR) : and

  }

}
