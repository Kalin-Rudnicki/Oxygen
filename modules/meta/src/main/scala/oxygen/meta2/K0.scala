package oxygen.meta2

import oxygen.core.RightProjection
import oxygen.core.instances.listOrd
import oxygen.core.syntax.extra.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.annotation.{tailrec, Annotation}
import scala.quoted.*

object K0 {

  type Const[A] = [_] =>> A
  type Id[A] = A

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Child
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Entity[A] {

    val label: String
    final def name: String = label

    val sym: Symbol
    val typeRepr: TypeRepr

    final given tpe: Type[A] = typeRepr.asTypeOf[A]

    def pos: Position

    def annotations(using Quotes): AnnotationsTyped[A]

    final def summonTypeClass[TC[_]: Type](using quotes: Quotes): Expr[TC[A]] =
      Implicits.search(TypeRepr.of[TC[A]]) match
        case ImplicitSearchSuccess(tree)        => tree.asExprOf[TC[A]]
        case ImplicitSearchFailure(explanation) => report.errorAndAbort(s"Error summoning ${TypeRepr.of[TC[A]].show}\n\n$explanation", pos)

    final def summonTypeClassOrDerive[TC[_]: Type](f: => Type[A] ?=> Expr[TC[A]])(using quotes: Quotes): Expr[TC[A]] =
      Implicits.search(TypeRepr.of[TC[A]]) match
        case ImplicitSearchSuccess(tree) => tree.asExprOf[TC[A]]
        case ImplicitSearchFailure(_)    => f(using tpe)

  }
  object Entity {

    trait Child[B, A] extends Entity[B] {

      val idx: Int
      val childType: String

      final def getExpr[F[_]](expressions: Expressions[F, A])(using Quotes): Expr[F[B]] =
        expressions.at[B](idx)

    }
    object Child {

      abstract class Deferred[A, B](entity: Entity[A]) extends Child[A, B] {
        override final val label: String = entity.label
        override final val sym: Symbol = entity.sym
        override final val typeRepr: TypeRepr = entity.typeRepr
        override final def pos: Position = entity.pos
        override final def annotations(using Quotes): AnnotationsTyped[A] = entity.annotations
      }

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Caching
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class ValDefinitions[F[_], A](
      fTpe: Type[F],
      aTpe: Type[A],
      make: Quotes ?=> Contiguous[(ValDef, Type[?])],
  ) {

    private given Type[F] = fTpe

    def defineAndUse[B: Type](f: Quotes ?=> Expressions[F, A] => Expr[B])(using quotes: Quotes): Expr[B] = {
      val vals: Contiguous[(ValDef, Type[?])] = make(using quotes)
      val newQuotes: Quotes = vals.lastOption.fold(quotes)(_._1.symbol.asQuotes) // this might be unnecessary
      val expressions: Expressions[F, A] =
        Expressions[F, A](
          fTpe,
          aTpe,
          vals.map { case (v, tpe) =>
            type C
            given cTpe: Type[C] = tpe.asInstanceOf[Type[C]]
            Expressions.Elem(
              cTpe,
              v.valRef.asExprOf[F[C]],
            )
          },
        )
      val res: Expr[B] = f(using newQuotes)(expressions)
      Block.companion.apply(vals.map(_._1).toList, res.toTerm(using newQuotes)).asExprOf[B]
    }

  }

  // FIX-PRE-MERGE (KR) : Have a concept like `Expressions`, but without F being restricted to `[b] =>> Expr[F[b]]`

  final class Expressions[F[_], A](
      fTpe: Type[F],
      aTpe: Type[A],
      expressions: Contiguous[Expressions.Elem[F, ?]],
  ) {

    private given Type[F] = fTpe

    def at[B: Type](idx: Int)(using Quotes): Expr[F[B]] =
      expressions.at(idx).expr.asExprOf[F[B]]

    def mapK[G[_]: Type as gTpe](f: [b] => Type[b] ?=> Expr[F[b]] => Expr[G[b]]): Expressions[G, A] =
      Expressions[G, A](
        gTpe,
        aTpe,
        expressions.map(elem => elem.mapK(f(_))),
      )

  }
  object Expressions {

    final case class Elem[F[_], B](
        bTpe: Type[B],
        expr: Expr[F[B]],
    ) {

      def mapK[G[_]](f: Type[B] ?=> Expr[F[B]] => Expr[G[B]]): Elem[G, B] =
        Elem[G, B](bTpe, f(using bTpe)(expr))

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Generic[A] extends Entity[A] {

    type Bound <: Any
    type Child[B <: Bound] <: Entity.Child[B, A]
    final type AnyChild = Child[Bound]

    type ChildFunction0[O[_]] = [b <: Bound] => Type[b] ?=> Child[b] => O[b]
    object ChildFunction0 {
      type Tupled[O1[_], O2[_]] = ChildFunction0[[b] =>> (O1[b], O2[b])]
      type Nested[O1[_], O2[_]] = ChildFunction0[[b] =>> O1[O2[b]]]
      type IdExpr = ChildFunction0[Expr]
      type FExpr[F[_]] = ChildFunction0.Nested[Expr, F]

      def run[O[_]]( // TODO (KR) :
          f: ChildFunction0[O],
      ): Growable[O[Bound]] =
        Growable.many(children).map { child0 =>
          type B <: Bound
          val child: Child[B] = child0.asInstanceOf[Child[B]]
          given Type[B] = child.tpe
          val value: O[B] = f[B](child)
          value.asInstanceOf[O[Bound]]
        }

      def tupled[O1[_], O2[_]](
          f1: ChildFunction0[O1],
          f2: ChildFunction0[O2],
      ): ChildFunction0.Tupled[O1, O2] = {
        [b <: Bound] =>
          _ ?=>
            (child: Child[b]) =>
              // stop fmting
              (f1[b](child), f2[b](child))
      }

      def getExpr[F[_]](exprs: Expressions[F, A])(using Quotes): ChildFunction0.FExpr[F] = {
        [b <: Bound] =>
          _ ?=>
            (child: Child[b]) =>
              // stop fmting
              child.getExpr(exprs)
      }

    }

    type ChildFunction1[I[_], O[_]] = [b <: Bound] => Type[b] ?=> (Child[b], I[b]) => O[b]
    object ChildFunction1 {
      type Nested[I[_], O1[_], O2[_]] = ChildFunction1[I, [b] =>> O1[O2[b]]]
      type IdExpr[I[_]] = ChildFunction1[I, Expr]
      type FExpr[I[_], F[_]] = ChildFunction1.Nested[I, Expr, F]

      def run[I[_], O[_]](
          i: ChildFunction0[I],
          f: ChildFunction1[I, O],
      ): Growable[O[Bound]] =
        Growable.many(children).map { child0 =>
          type B <: Bound
          val child: Child[B] = child0.asInstanceOf[Child[B]]
          given Type[B] = child.tpe
          val value: O[B] = f[B](child, i[B](child))
          value.asInstanceOf[O[Bound]]
        }

    }

    val typeType: TypeType
    def children: Contiguous[AnyChild]

    override final def pos: Position = sym.pos.get

    override final def annotations(using Quotes): AnnotationsTyped[A] = AnnotationsTyped(typeRepr.annotations.all, typeRepr.show)

    @scala.annotation.nowarn("msg=unused import")
    final def showTypeClassInstances[F[_]: Type](using Quotes): Unit =
      children.foreach { child0 =>
        type B <: Bound
        val child: Child[B] = child0.asInstanceOf[Child[B]]
        import child.tpe

        def msg(label: String, str: String): String =
          s"""${child.childType}: ${child.label}
             |type: ${child.typeRepr.show}
             |$label: $str""".stripMargin

        Implicits.search(TypeRepr.of[F[B]]) match
          case success: ImplicitSearchSuccess => report.info(msg("instance", success.tree.show), child.pos)
          case failure: ImplicitSearchFailure => report.warning(msg("explanation", failure.explanation), child.pos)
      }

    /////// CacheVals ///////////////////////////////////////////////////////////////

    class CacheVals {

      final def apply[F[_]: Type as fTpe](
          valName: String => String = n => s"value_$n",
          valType: ValType = ValType.Val,
      )(
          f: ChildFunction0.FExpr[F],
      )(using Quotes): ValDefinitions[F, A] = {
        val flags: Flags = valType match
          case ValType.Val     => Flags.EmptyFlags
          case ValType.LazyVal => Flags.Lazy
          case ValType.Var     => Flags.Mutable

        // FIX-PRE-MERGE (KR) : if the vals dont have the parent as their symbol, could this just be a Map?
        @tailrec
        def rec(queue: List[AnyChild], acc: Growable[(ValDef, Type[?])]): Contiguous[(ValDef, Type[?])] =
          queue match {
            case child0 :: tail =>
              type B <: Bound
              val child: Child[B] = child0.asInstanceOf[Child[B]]
              given bTpe: Type[B] = child.tpe
              val value: Expr[F[B]] = f[B](using bTpe)(child)
              val newSym: Symbol = Symbol.companion.newVal(Symbol.spliceOwner, valName(child.name), TypeRepr.of[F[B]], flags, Symbol.noSymbol)
              val newDef: ValDef = ValDef.companion.apply(newSym, value.toTerm.some)
              rec(tail, acc :+ (newDef, bTpe))
            case Nil =>
              acc.to[Contiguous]
          }

        new ValDefinitions[F, A](fTpe, tpe, _ ?=> rec(children.toList, Growable.empty))
      }

      final def summonTypeClasses[F[_]: Type](
          valName: String => String = n => s"instance_$n",
          valType: ValType = ValType.LazyVal,
      )(using quotes: Quotes): ValDefinitions[F, A] =
        apply[F](valName = valName, valType = valType) { [b <: Bound] => _ ?=> (child: Child[b]) => child.summonTypeClass[F] }

      final def summonTypeClassesOrDerive[F[_]: Type](
          valName: String => String = n => s"instance_$n",
          valType: ValType = ValType.LazyVal,
      )(
          f: ChildFunction0.FExpr[F],
      )(using quotes: Quotes): ValDefinitions[F, A] =
        apply[F](valName = valName, valType = valType) { [b <: Bound] => _ ?=> (child: Child[b]) => child.summonTypeClassOrDerive[F] { f(child) } }

    }

    val cacheVals: CacheVals = new CacheVals

    /////// MapChildren ///////////////////////////////////////////////////////////////

    class MapChildren {

      def map[Out](f: ChildFunction0[K0.Const[Out]]): Growable[Out] = ChildFunction0.run(f)
      def mapExpr[Out](f: ChildFunction0[K0.Const[Expr[Out]]]): Growable[Expr[Out]] = map[Expr[Out]](f)

      def flatMap[S[_]: SeqOps, Out](f: ChildFunction0[K0.Const[S[Out]]]): Growable[Out] = ??? // FIX-PRE-MERGE (KR) :
      def flatMapExpr[S[_]: SeqOps, Out](f: ChildFunction0[K0.Const[S[Expr[Out]]]]): Growable[Expr[Out]] = flatMap[S, Expr[Out]](f)

    }

    val mapChildren: MapChildren = new MapChildren

  }
  object Generic {

    def of[A: Type](config: Derivable.Config)(using Quotes): Generic[A] =
      TypeRepr.of[A].typeType match {
        case Some(_: TypeType.Case)   => ProductGeneric.unsafeOf[A](config)
        case Some(_: TypeType.Sealed) => SumGeneric.unsafeOf[A](config)
        case None                     => report.errorAndAbort(s"Type ${TypeRepr.of[A].show} is not a product or sum type")
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ProductGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait ProductGeneric[A] private extends Generic[A] { generic =>

    override final type Bound = Any
    override final type Child[B] = Field[B]

    override val typeType: TypeType.Case

    def fields: Contiguous[Field[?]]

    override final def children: Contiguous[AnyChild] = fields.asInstanceOf[Contiguous[AnyChild]]

    def fieldsToInstance[S[_]: SeqOps](exprs: S[Expr[?]])(using Quotes): Expr[A]

    /////// Field ///////////////////////////////////////////////////////////////

    final case class Field[B](
        idx: Int,
        typeRepr: TypeRepr,
        constructorValDef: ValDef,
        fieldValDef: ValDef,
    ) extends Entity.Child[B, A] {

      override val childType: String = "field"

      override val label: String = constructorValDef.name

      override val sym: Symbol = constructorValDef.symbol

      override def pos: Position = constructorValDef.pos

      def annotations(using Quotes): AnnotationsTyped[B] = AnnotationsTyped(constructorValDef.symbol.annotations.all, constructorValDef.show)

      def get(parent: Expr[A])(using quotes: Quotes): Expr[B] =
        parent.toTerm.select(fieldValDef.symbol).asExprOf[B]

    }

    /////// Instantiate ///////////////////////////////////////////////////////////////

    class Instantiate {

      def apply(
          f: ChildFunction0.IdExpr,
      )(using Quotes): Expr[A] =
        fieldsToInstance(ChildFunction0.run(f))

      def monad[F[_]: {ExprMonad as monad, Type}](
          f: ChildFunction0.FExpr[F],
      )(using Quotes): Expr[F[A]] = {
        def rec(queue: List[AnyChild], acc: Growable[Expr[?]])(using Quotes): Expr[F[A]] =
          queue match {
            case child0 :: tail =>
              type B
              val child: Field[B] = child0.asInstanceOf[Field[B]]
              @scala.annotation.unused
              given Type[B] = child.tpe
              val value: Expr[F[B]] = f[B](child)

              tail match {
                case Nil => monad.mapE(value) { a => fieldsToInstance((acc :+ a).to[Contiguous]) }
                case _   => monad.flatMapE(value) { a => rec(tail, acc :+ a) }
              }
            case Nil =>
              monad.pure(fieldsToInstance(acc.to[Contiguous]))
          }

        rec(children.toList, Growable.empty)
      }

      def option(
          f: ChildFunction0.FExpr[Option],
      )(using Quotes): Expr[Option[A]] =
        monad[Option](f)

      def either[Left: Type](
          f: ChildFunction0.FExpr[RightProjection[Left]],
      )(using Quotes): Expr[Either[Left, A]] =
        monad[RightProjection[Left]](f)

    }

    val instantiate: Instantiate = new Instantiate

  }
  object ProductGeneric {

    trait CaseObjectGeneric[A] extends ProductGeneric[A] {

      override final val fields: Contiguous[Field[?]] = Contiguous.empty

      override val typeType: TypeType.Case.Object

      /////// Instantiate ///////////////////////////////////////////////////////////////

      class CaseObjectInstantiate extends Instantiate {

        def instance(using Quotes): Expr[A] = fieldsToInstance(Nil)

      }

      override val instantiate: CaseObjectInstantiate = new CaseObjectInstantiate

    }
    object CaseObjectGeneric {

      private[ProductGeneric] def unsafeOf[A: Type](using Quotes): CaseObjectGeneric[A] = {
        val _typeRepr: TypeRepr = TypeRepr.of[A]
        val _termSym: Symbol = _typeRepr.termSymbol

        new CaseObjectGeneric[A] {

          override val label: String = _termSym.name
          override val sym: Symbol = _termSym
          override val typeRepr: TypeRepr = _typeRepr
          override val typeType: TypeType.Case.Object = _typeRepr.typeTypeCaseObject.get

          override def fieldsToInstance[S[_]: SeqOps](exprs: S[Expr[?]])(using Quotes): Expr[A] =
            if (exprs.into[Contiguous].nonEmpty)
              report.errorAndAbort("attempted to instantiate case object with non-empty fields")
            else
              _termSym.toTerm.asExprOf[A]

        }
      }

    }

    trait CaseClassGeneric[A] extends ProductGeneric[A] {
      override val typeType: TypeType.Case.Class
    }
    object CaseClassGeneric {

      private[ProductGeneric] def unsafeOf[A: Type](using Quotes): CaseClassGeneric[A] = {
        val _typeRepr: TypeRepr = TypeRepr.of[A]
        val _typeSym: Symbol = _typeRepr.typeSymbol
        val _primaryConstructorSym: Symbol = _typeSym.primaryConstructor
        val _primaryConstructor: DefDef = _primaryConstructorSym.tree.narrow[DefDef]

        val (constructorTypes, constructorTerms): (Option[TypeParamClause], TermParamClause) =
          _primaryConstructor.paramss match
            case List(types: TypeParamClause, terms: TermParamClause) => (types.some, terms)
            case List(terms: TermParamClause)                         => (None, terms)
            case _                                                    => report.errorAndAbort("Invalid case class structure. Expected single param group.")

        val constructorVals: List[ValDef] = constructorTerms.params
        val fieldVals: List[ValDef] =
          _typeSym.caseFields.map(_.tree.narrow[ValDef]("case field not a val def?"))

        if (constructorVals.size != fieldVals.size)
          report.errorAndAbort("Primary constructor size differs from case fields size?")

        val typeArgs: List[TypeRepr] = _typeRepr.dealias match
          case appTpe: AppliedType => appTpe.args
          case _                   => Nil

        // TODO (KR) : consider making this type replacement a shared utility
        val alterRepr: TypeRepr => TypeRepr =
          constructorTypes match {
            case Some(constructorTypes) =>
              val typeArgsSymbols: List[Symbol] = constructorTypes.params.map { s => _typeSym.typeMember(s.name) }

              if (typeArgsSymbols.size != typeArgs.size)
                report.errorAndAbort("Type param symbols and reprs have different size?")

              _.substituteTypes(typeArgsSymbols, typeArgs)
            case None =>
              identity
          }

        val fieldTuple: Contiguous[(Int, TypeRepr, ValDef, ValDef)] =
          Contiguous.from(constructorVals.zip(fieldVals)).zipWithIndex.map { case ((constructorVal, fieldVal), idx) =>
            if (constructorVal.name != fieldVal.name)
              report.errorAndAbort("vals are not in same order?")

            val typeRepr: TypeRepr = alterRepr(fieldVal.tpt.tpe)

            (idx, typeRepr, constructorVal, fieldVal)
          }

        val constructorAwaitingArgs: Term = {
          val pc = New.companion.apply(TypeTree.ref(_typeSym)).select(_primaryConstructorSym)
          constructorTypes match {
            case Some(_) => pc.appliedToTypes(typeArgs)
            case None    => pc
          }
        }

        new CaseClassGeneric[A] {

          override val label: String = _typeSym.name
          override val sym: Symbol = _typeSym
          override val typeRepr: TypeRepr = _typeRepr
          override val typeType: TypeType.Case.Class = _typeSym.typeTypeCaseClass.get

          override val fields: Contiguous[Field[?]] =
            fieldTuple.map { case (idx, typeRepr, constructorVal, fieldVal) =>
              Field(
                idx = idx,
                typeRepr = typeRepr,
                constructorValDef = constructorVal,
                fieldValDef = fieldVal,
              )
            }

          override def fieldsToInstance[S[_]: SeqOps](exprs: S[Expr[?]])(using quotes: Quotes): Expr[A] = {
            val exprSize = exprs.size
            if (exprSize != fields.length)
              report.errorAndAbort(s"Provided exprs ($exprSize) != num fields (${fields.length})")

            constructorAwaitingArgs
              .appliedToArgs(exprs.map(_.toTerm).into[List])
              .asExprOf[A]
          }

        }
      }

    }

    trait AnyValGeneric[A, B] extends CaseClassGeneric[A] {

      val field: Field[B]

      given bTpe: Type[B] = field.tpe

      override final lazy val fields: Contiguous[Field[?]] = Contiguous.single(field)

      class AnyValUtil {

        def wrap(value: Expr[B])(using Quotes): Expr[A] = fieldsToInstance(value :: Nil)

        def unwrap(value: Expr[A])(using Quotes): Expr[B] = field.get(value)

      }

      val anyVal: AnyValUtil = new AnyValUtil

    }
    object AnyValGeneric {

      private[ProductGeneric] def unsafeOf[A: Type](using Quotes): AnyValGeneric[A, ?] = {
        val g: CaseClassGeneric[A] = CaseClassGeneric.unsafeOf[A]

        val _gField: g.Field[?] = g.fields match {
          case Contiguous(field) => field
          case _                 => report.errorAndAbort("AnyVal has non-single param?")
        }

        type B
        val gField: g.Field[B] = _gField.asInstanceOf[g.Field[B]]

        new AnyValGeneric[A, B] {

          override val label: String = g.label
          override val sym: Symbol = g.sym
          override val typeRepr: TypeRepr = g.typeRepr

          override val typeType: TypeType.Case.Class = g.typeType
          override val field: Field[B] =
            Field(
              idx = gField.idx,
              typeRepr = gField.typeRepr,
              constructorValDef = gField.constructorValDef,
              fieldValDef = gField.fieldValDef,
            )

          override def fieldsToInstance[S[_]: SeqOps](exprs: S[Expr[?]])(using Quotes): Expr[A] =
            g.fieldsToInstance(exprs)

        }
      }

    }

    private[K0] def unsafeOf[A: Type](@scala.annotation.unused config: Derivable.Config)(using Quotes): ProductGeneric[A] = {
      println(1)
      val repr = TypeRepr.of[A]
      println(2)
      repr.typeTypeCase match {
        case Some(_: TypeType.Case.Class) =>
          repr.typeSymbol.tree match {
            case cdef: ClassDef if cdef.parents.headOption.flatMap(_.narrowOpt[TypeTree]).exists(_.tpe =:= TypeRepr.of[AnyVal]) =>
              AnyValGeneric.unsafeOf[A]
            case _ =>
              CaseClassGeneric.unsafeOf[A]
          }
        case Some(_: TypeType.Case.Object) =>
          println(3)
          CaseObjectGeneric.unsafeOf[A]
        case None => report.errorAndAbort(s"Not a product type: ${repr.show}")
      }
    }

    def of[A: Type](config: Derivable.Config)(using Quotes): ProductGeneric[A] =
      TypeRepr.of[A].typeType match {
        case Some(_: TypeType.Case)   => ProductGeneric.unsafeOf[A](config)
        case Some(_: TypeType.Sealed) => report.errorAndAbort(s"${TypeRepr.of[A].show} is not a product type")
        case None                     => report.errorAndAbort(s"Type ${TypeRepr.of[A].show} is not a product or sum type")
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      SumGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait SumGeneric[A] private extends Generic[A] { generic =>

    override final type Bound = A
    type Gen[b] <: Generic[b]
    override final type Child[B <: A] = Case[B]

    override val typeType: TypeType.Sealed

    def cases: Contiguous[Case[? <: A]]

    override final def children: Contiguous[AnyChild] = cases.asInstanceOf[Contiguous[AnyChild]]

    /////// Case ///////////////////////////////////////////////////////////////

    final case class Case[B <: A](
        idx: Int,
        generic: Gen[B],
    ) extends Entity.Child.Deferred[B, A](generic) {

      override val childType: String = "case"

    }

  }
  object SumGeneric {

    /**
      * How should ordinals be assigned?
      *
      * ```scala
      * enum MyEnum {
      *   case A
      *   case C
      *   case B
      * }
      * ```
      *
      * SourcePosition: A, C, B
      * Lexicographical: A, B, C
      */
    enum OrdinalStrategy {
      case SourcePosition, Lexicographical

      final def ord: Ordering[Generic[?]] = this match
        case OrdinalStrategy.SourcePosition  => Ordering.by { _.pos.start }
        case OrdinalStrategy.Lexicographical => Ordering.by { _.sym.fullName.split("[.]").reverse.toList }

    }

    /**
      * How to handle sealed trait hierarchies.
      *
      * ```scala
      * sealed trait Root
      *
      * sealed trait Child1 extends Root
      * case object A extends Child1
      * case object B extends Child1
      *
      * sealed trait Child2 extends Root
      * case object D extends Child2
      * case object C extends Child2
      * ```
      *
      * Unroll: SumGeneric(Root)(ProductGeneric(A), ProductGeneric(B), ProductGeneric(C), ProductGeneric(D))
      * Nested: SumGeneric(Root)(SumGeneric(Child1)(ProductGeneric(A), ProductGeneric(B)), SumGeneric(Child2)(ProductGeneric(C), ProductGeneric(D)))
      */
    enum UnrollStrategy { case Unroll, Nested }

    trait FlatGeneric[A] extends SumGeneric[A] {
      override type Gen[b] <: ProductGeneric[b]
    }

    private[SumGeneric] trait FlatNonEnumGeneric[A] extends FlatGeneric[A] {
      override final type Gen[b] = ProductGeneric[b]
    }

    trait EnumGeneric[A] extends FlatGeneric[A] {
      override final type Gen[b] = ProductGeneric.CaseObjectGeneric[b]
    }

    trait NestedGeneric[A] extends SumGeneric[A] {
      override final type Gen[b] = Generic[b]
    }

    private[K0] def unsafeOf[A: Type](config: Derivable.Config)(using Quotes): SumGeneric[A] = {
      val _typeRepr: TypeRepr = TypeRepr.of[A]
      val _typeSym: Symbol = _typeRepr.typeSymbol

      def unroll(isRoot: Boolean): Boolean =
        config.defaultUnrollStrategy match
          case UnrollStrategy.Unroll => true
          case UnrollStrategy.Nested => isRoot

      def childGenericsRec(isRoot: Boolean, typeSym: Symbol): Growable[Generic[? <: A]] =
        typeSym.typeType match {
          case Some(_: TypeType.Case.Class) =>
            val classDef: ClassDef = typeSym.tree.narrow[ClassDef]
            if (classDef.constructor.paramss.exists { case _: TypeParamClause => true; case _ => false })
              report.errorAndAbort("Type params on sum types is not yet supported")

            type B <: A
            val typeRepr: TypeRepr = typeSym.typeRef
            given Type[B] = typeRepr.asTypeOf

            Growable.single(ProductGeneric.unsafeOf[B](config))

          /*
            [error]    |    oxygen.meta.NewDeriveShowSpec.CaseObject1
            [error]    |
            [error]    |    lazy val CaseObject1: oxygen.meta.NewDeriveShowSpec.CaseObject1.type
            [error]    |
            [error]    |    [TermRef]:
            [error]    |        prefix: [ThisType]:
            [error]    |            tref: [TypeRef]:
            [error]    |                prefix: [ThisType]:
            [error]    |                    tref: [TypeRef]:
            [error]    |                        prefix: [NoPrefix]:
            [error]    |                        myDesignator: module class meta
            [error]    |                myDesignator: module class NewDeriveShowSpec$
            [error]    |        myDesignator: object CaseObject1
           */

          case Some(TypeType.EnumCaseObject) =>

            type B <: A
            val typeRepr: TypeRepr = typeSym.termRef
            given Type[B] = typeRepr.asTypeOf

            /*
            val valDef: ValDef = typeSym.tree.narrow[ValDef]
            report.errorAndAbort(
              s"""${typeRepr.show}
                 |
                 |${typeSym.flags.show}
                 |
                 |${valDef.show}
                 |
                 |${valDef.tpt.show}
                 |
                 |${valDef.tpt.tpe.show}
                 |
                 |${valDef.toIndentedString.toStringColorized}""".stripMargin,
            )
             */

            Growable.single(ProductGeneric.unsafeOf[B](config))

          case Some(TypeType.CaseObject) =>

            type B <: A
            val typeRepr: TypeRepr = typeSym.termRef
            given Type[B] = typeRepr.asTypeOf

            /*
            val valDef: ValDef = typeSym.tree.narrow[ValDef]
            report.errorAndAbort(
              s"""${typeRepr.show}
                 |
                 |${typeSym.flags.show}
                 |
                 |${valDef.show}
                 |
                 |${valDef.tpt.show}
                 |
                 |${valDef.tpt.tpe.show}
                 |
                 |${valDef.toIndentedString.toStringColorized}""".stripMargin,
            )
             */

            Growable.single(ProductGeneric.unsafeOf[B](config))

          case Some(TypeType.Scala2CaseObject) =>
            // FIX-PRE-MERGE (KR) :
            report.errorAndAbort("TODO : support scala-2 case object")

          case Some(_: TypeType.Sealed) =>
            val classDef: ClassDef = typeSym.tree.narrow[ClassDef]
            if (classDef.constructor.paramss.exists { case _: TypeParamClause => true; case _ => false })
              report.errorAndAbort("Type params on sum types is not yet supported")

            if (unroll(isRoot)) {
              val children = typeSym.children
              if (children.isEmpty)
                report.errorAndAbort(s"Sum type ${typeSym.name} has no children", typeSym.pos)

              Growable.many(children).flatMap(childGenericsRec(false, _))
            } else {
              type B <: A
              val typeRepr: TypeRepr = typeSym.typeRef
              given Type[B] = typeRepr.asTypeOf
              Growable.single(Generic.of[B](config))
            }

          case None => report.errorAndAbort(s"Type ${typeSym.name} is not a product or sum type", typeSym.pos)
        }

      val childGenerics: Contiguous[Generic[? <: A]] =
        childGenericsRec(true, _typeSym).toContiguous.sorted(using config.defaultOrdinalStrategy.ord)

      val filteredGenerics: Either[Contiguous[Generic[? <: A]], Either[Contiguous[ProductGeneric[? <: A]], Contiguous[ProductGeneric.CaseObjectGeneric[? <: A]]]] =
        childGenerics
          .traverse {
            case g: ProductGeneric[? <: A] => g.some
            case _                         => None
          }
          .toRight(childGenerics)
          .map { childGenerics =>
            childGenerics
              .traverse {
                case g: ProductGeneric.CaseObjectGeneric[? <: A] => g.some
                case _                                           => None
              }
              .toRight(childGenerics)
          }

      filteredGenerics match {
        case Right(Right(enums)) =>
          new SumGeneric.EnumGeneric[A] {
            override val label: String = _typeSym.name
            override val sym: Symbol = _typeSym
            override val typeRepr: TypeRepr = _typeRepr
            override val typeType: TypeType.Sealed = _typeSym.typeTypeSealed.get
            override val cases: Contiguous[Case[? <: A]] =
              enums.zipWithIndex.map { case (g, i) => Case(i, g) }
          }
        case Right(Left(products)) =>
          new SumGeneric.FlatNonEnumGeneric[A] {
            override val label: String = _typeSym.name
            override val sym: Symbol = _typeSym
            override val typeRepr: TypeRepr = _typeRepr
            override val typeType: TypeType.Sealed = _typeSym.typeTypeSealed.get
            override val cases: Contiguous[Case[? <: A]] =
              products.zipWithIndex.map { case (g, i) => Case(i, g) }
          }
        case Left(generics) =>
          new SumGeneric.NestedGeneric[A] {
            override val label: String = _typeSym.name
            override val sym: Symbol = _typeSym
            override val typeRepr: TypeRepr = _typeRepr
            override val typeType: TypeType.Sealed = _typeSym.typeTypeSealed.get
            override val cases: Contiguous[Case[? <: A]] =
              generics.zipWithIndex.map { case (g, i) => Case(i, g) }
          }
      }
    }

    def of[A: Type](config: Derivable.Config)(using Quotes): SumGeneric[A] =
      TypeRepr.of[A].typeType match {
        case Some(_: TypeType.Sealed) => SumGeneric.unsafeOf[A](config)
        case Some(_: TypeType.Case)   => report.errorAndAbort(s"${TypeRepr.of[A].show} is not a sum type")
        case None                     => report.errorAndAbort(s"Type ${TypeRepr.of[A].show} is not a product or sum type")
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Derivable
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Derivable[F[_]] {

    // TODO (KR) : support this:
    //           : type ProductF[A] <: F[A]
    //           : type SumF[A] <: F[A]

    protected val deriveConfig: Derivable.Config = Derivable.Config()
    protected def productDeriver[A](using Quotes, Type[F], Type[A], ProductGeneric[A], Derivable[F]): Derivable.ProductDeriver[F, A]
    protected def sumDeriver[A](using Quotes, Type[F], Type[A], SumGeneric[A], Derivable[F]): Derivable.SumDeriver[F, A]

    private[meta2] final def deriveFromGenericImpl[A](g: Generic[A])(using Quotes, Type[F], Type[A]): Expr[F[A]] = {
      given Derivable[F] = this
      val res: Expr[F[A]] = g match {
        case g: ProductGeneric[A] =>
          given ProductGeneric[A] = g
          productDeriver[A].derive
        case g: SumGeneric[A] =>
          given SumGeneric[A] = g
          sumDeriver[A].derive
      }

      g.annotations.optionalOf[annotation.showDerivation[F]].foreach { annot =>
        report.info(s"derivation for ${TypeRepr.of[F[A]].show}:\n\n${res.toTerm.show(using Printer.TreeAnsiCode)}", annot.toTerm.pos)
      }

      res
    }

    protected final def derivedImpl[A](using Quotes, Type[F], Type[A]): Expr[F[A]] =
      deriveFromGenericImpl(Generic.of[A](deriveConfig))

    /**
      * Should always be `= ${ derivedImpl[A] }`.
      * Annoying limitation with scala-3 macros that the compiler doesn't allow that to be set here, and inherited.
      */
    inline def derived[A]: F[A]

  }
  object Derivable {

    final case class Config(
        defaultOrdinalStrategy: SumGeneric.OrdinalStrategy = SumGeneric.OrdinalStrategy.SourcePosition,
        defaultUnrollStrategy: SumGeneric.UnrollStrategy = SumGeneric.UnrollStrategy.Unroll,
    )

    abstract class ProductDeriver[F[_], A](using
        final val quotes: Quotes,
        final val fTpe: Type[F],
        final val aTpe: Type[A],
        final val generic: ProductGeneric[A],
    ) {

      def derive: Expr[F[A]]

    }
    object ProductDeriver {

      final class NotSupported[F[_], A](using Quotes, Type[F], Type[A], ProductGeneric[A]) extends ProductDeriver[F, A] {
        override def derive: Expr[F[A]] = report.errorAndAbort(s"Auto derivation of product-types is not supported for ${Type.show[F]}")
      }

      def notSupported[F[_], A](using Quotes, Type[F], Type[A], ProductGeneric[A]): ProductDeriver[F, A] = new NotSupported[F, A]

      final class WithInstances[F[_], A](f: Quotes ?=> Expressions[F, A] => ProductDeriver[F, A])(using Quotes, Type[F], Type[A], ProductGeneric[A]) extends ProductDeriver[F, A] {
        override def derive: Expr[F[A]] =
          generic.cacheVals.summonTypeClasses[F]().defineAndUse { f(_).derive }
      }

      def withInstances[F[_], A](f: Quotes ?=> Expressions[F, A] => ProductDeriver[F, A])(using Quotes, Type[F], Type[A], ProductGeneric[A]) =
        new WithInstances[F, A](f)

      abstract class Split[F[_], A](using Quotes, Type[F], Type[A], ProductGeneric[A]) extends ProductDeriver[F, A] {

        def deriveCaseClass(generic: ProductGeneric.CaseClassGeneric[A]): Expr[F[A]]

        def deriveAnyVal[B: Type](generic: ProductGeneric.AnyValGeneric[A, B]): Expr[F[A]] = deriveCaseClass(generic)

        def deriveCaseObject(generic: ProductGeneric.CaseObjectGeneric[A]): Expr[F[A]]

        override final def derive: Expr[F[A]] = generic match {
          case generic0: ProductGeneric.AnyValGeneric[A, _] =>
            type B
            val generic: ProductGeneric.AnyValGeneric[A, B] = generic0.asInstanceOf[ProductGeneric.AnyValGeneric[A, B]]
            given Type[B] = generic.bTpe
            deriveAnyVal(generic)
          case generic: ProductGeneric.CaseClassGeneric[A]  => deriveCaseClass(generic)
          case generic: ProductGeneric.CaseObjectGeneric[A] => deriveCaseObject(generic)
        }

      }

    }

    abstract class SumDeriver[F[_], A](using
        final val quotes: Quotes,
        final val fTpe: Type[F],
        final val aTpe: Type[A],
        final val generic: SumGeneric[A],
    ) {

      def derive: Expr[F[A]]

    }
    object SumDeriver {

      final class NotSupported[F[_], A](using Quotes, Type[F], Type[A], SumGeneric[A]) extends SumDeriver[F, A] {
        override def derive: Expr[F[A]] = report.errorAndAbort(s"Auto derivation of sum-types is not supported for ${Type.show[F]}")
      }

      def notSupported[F[_], A](using Quotes, Type[F], Type[A], SumGeneric[A]): SumDeriver[F, A] = new NotSupported[F, A]

      final class WithInstances[F[_]: Derivable as derivable, A](f: Quotes ?=> Expressions[F, A] => SumDeriver[F, A])(using Quotes, Type[F], Type[A], SumGeneric[A]) extends SumDeriver[F, A] {
        override def derive: Expr[F[A]] =
          generic.cacheVals.summonTypeClassesOrDerive[F]() { [b <: A] => _ ?=> (kase: generic.Case[b]) => derivable.deriveFromGenericImpl(kase.generic) }.defineAndUse { f(_).derive }
      }

      def withInstances[F[_], A](f: Quotes ?=> Expressions[F, A] => SumDeriver[F, A])(using Quotes, Type[F], Type[A], SumGeneric[A], Derivable[F]): SumDeriver[F, A] = new WithInstances[F, A](f)

      abstract class Split[F[_], A](using Quotes, Type[F], Type[A], SumGeneric[A]) extends SumDeriver[F, A] {

        def deriveFlat(generic: SumGeneric.FlatGeneric[A]): Expr[F[A]]

        def deriveEnum(generic: SumGeneric.EnumGeneric[A]): Expr[F[A]] = deriveFlat(generic)

        def deriveNested(generic: SumGeneric.NestedGeneric[A]): Expr[F[A]]

        override final def derive: Expr[F[A]] = generic match
          case generic: SumGeneric.EnumGeneric[A]   => deriveEnum(generic)
          case generic: SumGeneric.FlatGeneric[A]   => deriveFlat(generic)
          case generic: SumGeneric.NestedGeneric[A] => deriveNested(generic)

      }

    }

  }

  object annotation {

    final class showDerivation[F[_]] extends Annotation

  }

}
