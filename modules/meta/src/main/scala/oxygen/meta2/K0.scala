package oxygen.meta2

import oxygen.core.RightProjection
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

  final class ChildMapper[Bound, A, Child[B <: Bound] <: Entity.Child[B, A], F[_] <: Tuple](children: Contiguous[(Child[Bound], F[Bound])]) {

    def mapKTup[F2[_] <: Tuple](f: [b <: Bound] => Type[b] ?=> (child: Child[b], value: F[b]) => F2[b]): ChildMapper[Bound, A, Child, F2] =
      ChildMapper[Bound, A, Child, F2] {
        children.map { tup =>
          type B <: Bound
          val child: Child[B] = tup._1.asInstanceOf[Child[B]]
          val value: F[B] = tup._2.asInstanceOf[F[B]]
          val value2: F2[Bound] = f[B](using child.tpe)(child, value).asInstanceOf[F2[Bound]]
          (tup._1, value2)
        }
      }

    def mapK[F2[_]](f: [b <: Bound] => Type[b] ?=> (child: Child[b], value: F[b]) => F2[b]): ChildMapper[Bound, A, Child, [b] =>> Tuple1[F2[b]]] =
      mapKTup[[b] =>> Tuple1[F2[b]]] { [b <: Bound] => _ ?=> (child: Child[b], value: F[b]) => Tuple1(f(child, value)) }

    def addContext[F2[_]](f: [b <: Bound] => Type[b] ?=> (child: Child[b], value: F[b]) => F2[b]): ChildMapper[Bound, A, Child, [b] =>> F2[b] *: F[b]] =
      mapKTup[[b] =>> F2[b] *: F[b]] { [b <: Bound] => _ ?=> (child: Child[b], value: F[b]) => f(child, value) *: value }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Generic[A] extends Entity[A] {

    type Bound <: Any
    type Child[B <: Bound] <: Entity.Child[B, A]
    final type AnyChild = Child[Bound]
    final type ChildMapper[F[_] <: Tuple] = K0.ChildMapper[Bound, A, Child, F]

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
          f: [b <: Bound] => (Quotes, Type[b]) ?=> Child[b] => Expr[F[b]],
      )(using Quotes): ValDefinitions[F, A] = {
        val flags: Flags = valType match
          case ValType.Val     => Flags.EmptyFlags
          case ValType.LazyVal => Flags.Lazy
          case ValType.Var     => Flags.Mutable

        @tailrec
        def rec(queue: List[AnyChild], acc: Growable[(ValDef, Type[?])]): Contiguous[(ValDef, Type[?])] =
          queue match {
            case child0 :: tail =>
              type B <: Bound
              val child: Child[B] = child0.asInstanceOf[Child[B]]
              given bTpe: Type[B] = child.tpe
              val newSym: Symbol = Symbol.companion.newVal(Symbol.spliceOwner, valName(child.name), TypeRepr.of[F[B]], flags, Symbol.noSymbol)
              val newDef: ValDef = ValDef.companion.apply(newSym, f[B](using quotes, child.tpe)(child).toTerm(using quotes).some)
              rec(tail, acc :+ (newDef, child.tpe))
            case Nil =>
              acc.to[Contiguous]
          }

        new ValDefinitions[F, A](fTpe, tpe, (quotes: Quotes) ?=> rec(children.toList, Growable.empty))
      }

      final def summonTypeClasses[F[_]: Type](
          valName: String => String = n => s"instance_$n",
          valType: ValType = ValType.LazyVal,
      )(using quotes: Quotes): ValDefinitions[F, A] =
        cacheVals[F](valName = valName, valType = valType) { [b <: Bound] => (_, _) ?=> (child: Child[b]) => child.summonTypeClass[F] }

      final def summonTypeClassesOrDerive[F[_]: Type](
          valName: String => String = n => s"instance_$n",
          valType: ValType = ValType.LazyVal,
      )(f: [b <: Bound] => Type[b] ?=> Child[b] => Expr[F[b]])(using quotes: Quotes): ValDefinitions[F, A] =
        cacheVals[F](valName = valName, valType = valType) { [b <: Bound] => (_, _) ?=> (child: Child[b]) => child.summonTypeClassOrDerive[F](f(child)) }

    }

    val cacheVals: CacheVals = new CacheVals

  }
  object Generic {

    def of[A: Type](config: Derivable.Config)(using Quotes): Generic[A] =
      TypeRepr.of[A].typeType match {
        case Some(_: TypeType.Case)   => ProductGeneric.of[A]
        case Some(_: TypeType.Sealed) => SumGeneric.of[A](config)
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

    /////// Util ///////////////////////////////////////////////////////////////

    class Util {

      def map[Out](f: [b] => Type[b] ?=> Field[b] => Out): Growable[Out] =
        Growable.many(fields).map { _field =>
          type _b
          val field: Field[_b] = _field.asInstanceOf[Field[_b]]

          f[_b](using field.tpe)(field)
        }

      def mapExpr[Out](f: [b] => Type[b] ?=> Field[b] => Expr[Out]): Growable[Expr[Out]] =
        map[Expr[Out]](f)

      def flatMap[S[_]: SeqOps, Out](f: [b] => Type[b] ?=> Field[b] => S[Out]): Growable[Out] =
        Growable.many(fields).flatMap { _field =>
          type _b
          val field: Field[_b] = _field.asInstanceOf[Field[_b]]

          Growable.many(f[_b](using field.tpe)(field))
        }

      def flatMapExpr[S[_]: SeqOps, Out](f: [b] => Type[b] ?=> Field[b] => S[Expr[Out]]): Growable[Expr[Out]] =
        flatMap[S, Expr[Out]](f)

      def instantiate(f: [b] => Type[b] ?=> Field[b] => Expr[b])(using Quotes): Expr[A] =
        fieldsToInstance(
          fields.map { _field =>
            type B
            val field: Field[B] = _field.asInstanceOf[Field[B]]
            f[B](using field.tpe)(field)
          },
        )

      def instantiateM[F[_]: {ExprMonad as monad, Type}](f: [b] => Type[b] ?=> Field[b] => Expr[F[b]])(using Quotes): Expr[F[A]] = {
        def rec(queue: List[Field[?]], acc: Growable[Expr[?]])(using Quotes): Expr[F[A]] =
          queue match {
            case head :: Nil =>
              type B
              val field: Field[B] = head.asInstanceOf[Field[B]]
              given Type[B] = field.tpe
              val res: Expr[F[B]] = f[B](field)
              monad.mapE(res) { a =>
                fieldsToInstance((acc :+ a).to[Contiguous])
              }
            case head :: tail =>
              type B
              val field: Field[B] = head.asInstanceOf[Field[B]]
              given Type[B] = field.tpe
              val res: Expr[F[B]] = f[B](field)
              monad.flatMapE(res) { a =>
                rec(tail, acc :+ a)
              }
            case Nil =>
              monad.pure(fieldsToInstance(acc.to[Contiguous]))
          }

        rec(fields.toList, Growable.empty)
      }

      def instantiateOption(f: [b] => Type[b] ?=> Field[b] => Expr[Option[b]])(using Quotes): Expr[Option[A]] =
        instantiateM[Option](f)

      def instantiateEither[Left: Type](f: [b] => Type[b] ?=> Field[b] => Expr[Either[Left, b]])(using Quotes): Expr[Either[Left, A]] =
        instantiateM[RightProjection[Left]](f)

    }

    val util: Util = new Util

  }
  object ProductGeneric {

    trait CaseObjectGeneric[A] extends ProductGeneric[A] {

      override final val fields: Contiguous[Field[?]] = Contiguous.empty

      class CaseObjectUtil extends Util {

        def instance(using Quotes): Expr[A] = fieldsToInstance(Nil)

      }

      override val util: CaseObjectUtil = new CaseObjectUtil

    }
    object CaseObjectGeneric {

      private[ProductGeneric] def of[A: Type](using Quotes): CaseObjectGeneric[A] = {
        val _typeRepr: TypeRepr = TypeRepr.of[A]
        val _termSym: Symbol = _typeRepr.termSymbol
        new CaseObjectGeneric[A] {

          override val label: String = _termSym.name
          override val sym: Symbol = _termSym
          override val typeRepr: TypeRepr = _typeRepr
          override val typeType: TypeType.Case = _typeRepr.typeTypeCase.get

          override def fieldsToInstance[S[_]: SeqOps](exprs: S[Expr[?]])(using Quotes): Expr[A] =
            if (exprs.into[Contiguous].nonEmpty)
              report.errorAndAbort("attempted to instantiate case object with non-empty fields")
            else
              _termSym.toTerm.asExprOf[A]

        }
      }

    }

    trait CaseClassGeneric[A] extends ProductGeneric[A] {

      class CaseClassUtil extends Util

      override val util: CaseClassUtil = new CaseClassUtil

    }
    object CaseClassGeneric {

      private[ProductGeneric] def of[A: Type](using Quotes): CaseClassGeneric[A] = {
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
          _typeSym.caseFields.map {
            _.tree match {
              case valDef: ValDef => valDef
              case t              => report.errorAndAbort(s"case field not a val def?\n${t.unwrap}", t.pos)
            }
          }

        if (constructorVals.size != fieldVals.size)
          report.errorAndAbort("Primary constructor size differs from case fields size?")

        val typeArgs: List[TypeRepr] = _typeRepr match
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
          override val typeType: TypeType.Case = _typeSym.typeTypeCase.get

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

      class AnyValUtil extends CaseClassUtil {

        def wrap(value: Expr[B])(using Quotes): Expr[A] = fieldsToInstance(value :: Nil)

        def unwrap(value: Expr[A])(using Quotes): Expr[B] = field.get(value)

      }

      override val util: AnyValUtil = new AnyValUtil

    }
    object AnyValGeneric {

      private[ProductGeneric] def of[A: Type](using Quotes): AnyValGeneric[A, ?] = {
        val g: CaseClassGeneric[A] = CaseClassGeneric.of[A]

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

          override val typeType: TypeType.Case = g.typeType
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

    def of[A: Type](using Quotes): ProductGeneric[A] = {
      val repr = TypeRepr.of[A]
      repr.typeTypeCase match {
        case Some(TypeType.CaseClass | TypeType.EnumCaseClass | TypeType.Scala2CaseClass) =>
          repr.typeSymbol.tree match {
            case cdef: ClassDef if cdef.parents.headOption.flatMap(_.narrowOpt[TypeTree]).exists(_.tpe =:= TypeRepr.of[AnyVal]) =>
              AnyValGeneric.of[A]
            case _ =>
              CaseClassGeneric.of[A]
          }
        case Some(TypeType.CaseObject | TypeType.EnumCaseObject | TypeType.Scala2CaseObject) => CaseObjectGeneric.of[A]
        case None                                                                            => report.errorAndAbort(s"Not a product type: ${repr.show}")
      }
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      SumGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait SumGeneric[A] private extends Generic[A] { generic =>

    override final type Bound = A
    override final type Child[B <: A] = Case[B]

    override val typeType: TypeType.Sealed

    def cases: Contiguous[Case[? <: A]]

    override final def children: Contiguous[AnyChild] = cases.asInstanceOf[Contiguous[AnyChild]]

    /////// Case ///////////////////////////////////////////////////////////////

    final case class Case[B <: A](
        idx: Int,
        generic: Generic[B],
    ) extends Entity.Child.Deferred[B, A](generic) {

      override val childType: String = "case"

    }

    /////// Util ///////////////////////////////////////////////////////////////

    class Util {

      def map[Out](f: [b <: A] => Type[b] ?=> Case[b] => Out): Growable[Out] =
        Growable.many(cases).map { _case =>
          type _b <: A
          val `case`: Case[_b] = _case.asInstanceOf[Case[_b]]

          f[_b](using `case`.tpe)(`case`)
        }

      def mapExpr[Out](f: [b <: A] => Type[b] ?=> Case[b] => Expr[Out]): Growable[Expr[Out]] =
        map[Expr[Out]](f)

    }

    val util: Util = new Util

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
    enum OrdinalStrategy { case SourcePosition, Lexicographical }

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

    trait FlatGeneric[A] extends SumGeneric[A]

    trait EnumGeneric[A] extends FlatGeneric[A]

    trait NestedGeneric[A] extends SumGeneric[A]

    def of[A: Type](config: Derivable.Config)(using Quotes): SumGeneric[A] =
      ??? // TODO (KR) :

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
