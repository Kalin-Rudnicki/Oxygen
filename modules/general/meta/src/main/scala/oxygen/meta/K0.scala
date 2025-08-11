package oxygen.meta

import oxygen.core.RightProjection
import oxygen.core.instances.listOrd
import oxygen.core.syntax.extra.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.annotation.Annotation
import scala.quoted.*
import scala.reflect.ClassTag

object K0 {

  type Const[A] = [_] =>> A
  type Id[A] = A
  type IdBounded[Bound] = [A <: Bound] =>> A

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Child
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Entity[SelfBound, A <: SelfBound] {

    type SelfType[A2 <: SelfBound] <: Entity[SelfBound, A2]

    val label: String
    final def name: String = label

    val sym: Symbol
    val typeRepr: TypeRepr

    final given tpe: Type[A] = typeRepr.asTypeOf[A]

    def pos: Position

    def annotations(using Quotes): AnnotationsTyped[A]

    /**
      * Meant to be used in the following manner:
      *
      * Entity in this entity could be any of Generic, ProductGeneric, SumGeneric, Field, Case
      * ```scala
      * val untyped: Entity[?] = ???
      *
      * type A
      * val typed: Entity[A] = untyped.typedAs[A]
      * ```
      *
      * Entity in this case could be any of Generic, ProductGeneric, SumGeneric, Field, Case
      */
    final def typedAs[TypeName <: SelfBound]: SelfType[TypeName] = this.asInstanceOf[SelfType[TypeName]]

    def summonTypeClass[TC[_]: Type](using quotes: Quotes): Expr[TC[A]] =
      Implicits.search(TypeRepr.of[TC[A]]) match
        case ImplicitSearchSuccess(tree)        => tree.asExprOf[TC[A]]
        case ImplicitSearchFailure(explanation) => report.errorAndAbort(s"Error summoning ${TypeRepr.of[TC[A]].show}\n\n$explanation", pos)

    final def summonTypeClassOrDerive[TC[_]: Type](f: => Type[A] ?=> Expr[TC[A]])(using quotes: Quotes): Expr[TC[A]] =
      Implicits.search(TypeRepr.of[TC[A]]) match
        case ImplicitSearchSuccess(tree) => tree.asExprOf[TC[A]]
        case ImplicitSearchFailure(_)    => f(using tpe)

  }
  object Entity {

    trait Child[ParentBound, B <: ParentBound, A] extends Entity[ParentBound, B] {

      override type SelfType[A2 <: ParentBound] <: Child[ParentBound, A2, A]

      val idx: Int
      val childType: String

      def parentGeneric: Generic[A]

      override def summonTypeClass[TC[_]: Type](using quotes: Quotes): Expr[TC[B]] =
        Implicits.search(TypeRepr.of[TC[B]]) match
          case ImplicitSearchSuccess(tree)        => tree.asExprOf[TC[B]]
          case ImplicitSearchFailure(explanation) =>
            report.errorAndAbort(s"Error summoning ${TypeRepr.of[TC[B]].show} for $childType `$name` in parent ${parentGeneric.typeRepr.show}\n\n$explanation", pos)

      final def getExpr[F[_]](expressions: Expressions[F, A])(using Quotes): Expr[F[B]] =
        expressions.at[B](idx)

    }
    object Child {

      abstract class Deferred[ParentBound, B <: ParentBound, A](entity: Entity[Any, B]) extends Child[ParentBound, B, A] {
        override final val label: String = entity.label
        override final val sym: Symbol = entity.sym
        override final val typeRepr: TypeRepr = entity.typeRepr
        override final def pos: Position = entity.pos
        override final def annotations(using Quotes): AnnotationsTyped[B] = entity.annotations
      }

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Caching
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class ValDefinitions[F[_], A](
      make: [b] => (Type[b], Quotes) ?=> (Expressions[F, A] => Expr[b]) => Expr[b],
  ) {

    def defineAndUse[B: Type](f: Expressions[F, A] => Expr[B])(using quotes: Quotes): Expr[B] =
      make(f)

  }

  final class ValDefinitionsWith[F[_], A, With](
      make: [b] => (Type[b], Quotes) ?=> ((Expressions[F, A], Expr[With]) => Expr[b]) => Expr[b],
  ) {

    def defineAndUse[B: Type](f: (Expressions[F, A], Expr[With]) => Expr[B])(using quotes: Quotes): Expr[B] =
      make(f)

  }

  // TODO (KR) : Have a concept like `Expressions`, but without F being restricted to `[b] =>> Expr[F[b]]`

  /**
    * Represents a set of expressions, which are accessible using [[Entity.Child.getExpr]].
    */
  final class Expressions[F[_], A](
      private[K0] val fTpe: Type[F],
      private[K0] val aTpe: Type[A],
      private[K0] val expressions: Contiguous[Expressions.Elem[F, ?]],
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

  sealed trait Generic[A] extends Entity[Any, A] {

    /////// Types ///////////////////////////////////////////////////////////////

    final type AType = A
    final type SelfBound = Any
    override type SelfType[A2] <: Generic[A2]
    type ChildBound <: Any
    type Child[B <: ChildBound] <: Entity.Child[ChildBound, B, A] { type SelfType[A2 <: ChildBound] = Child[A2] }
    final type AnyChild = Child[ChildBound]

    /////// Basic Members ///////////////////////////////////////////////////////////////

    def children: Contiguous[AnyChild]

    override final def pos: Position = sym.pos.get
    override final def annotations(using Quotes): AnnotationsTyped[A] = AnnotationsTyped(typeRepr.annotations.all, typeRepr.show)

    /////// ChildFunction ///////////////////////////////////////////////////////////////

    type ChildFunction0[O[_ <: ChildBound]] = [b <: ChildBound] => (Quotes, Type[b]) ?=> Child[b] => O[b]
    object ChildFunction0 {
      type Tupled[O1[_ <: ChildBound], O2[_ <: ChildBound]] = ChildFunction0[[b <: ChildBound] =>> (O1[b], O2[b])]
      type Nested[O1[_], O2[_ <: ChildBound]] = ChildFunction0[[b <: ChildBound] =>> O1[O2[b]]]
      type IdExpr = ChildFunction0[Expr]
      type FExpr[F[_ <: ChildBound]] = ChildFunction0.Nested[Expr, F]

      def run[O[_]](
          f: ChildFunction0[O],
      )(using Quotes): Growable[O[ChildBound]] =
        Growable.many(children).map { child0 =>
          type B <: ChildBound
          val child: Child[B] = child0.typedAs[B]
          given Type[B] = child.tpe
          val value: O[B] = f[B](child)
          value.asInstanceOf[O[ChildBound]]
        }

      def tupled[O1[_], O2[_]](
          f1: ChildFunction0[O1],
          f2: ChildFunction0[O2],
      ): ChildFunction0.Tupled[O1, O2] = { [b <: ChildBound] => (_, _) ?=> (child: Child[b]) =>
        // stop fmting
        (f1[b](child), f2[b](child))
      }

      def getExpr[F[_]](exprs: Expressions[F, A]): ChildFunction0.FExpr[F] = { [b <: ChildBound] => (_, _) ?=> (child: Child[b]) =>
        // stop fmting
        child.getExpr(exprs)
      }

    }

    // TODO (KR) : add bounds
    type ChildFunction1[I[_], O[_]] = [b <: ChildBound] => (Quotes, Type[b]) ?=> (Child[b], I[b]) => O[b]
    object ChildFunction1 {
      type Nested[I[_], O1[_], O2[_]] = ChildFunction1[I, [b] =>> O1[O2[b]]]
      type IdExpr[I[_]] = ChildFunction1[I, Expr]
      type FExpr[I[_], F[_]] = ChildFunction1.Nested[I, Expr, F]

      def run[I[_], O[_]](
          i: ChildFunction0[I],
          f: ChildFunction1[I, O],
      )(using Quotes): Growable[O[ChildBound]] =
        Growable.many(children).map { child0 =>
          type B <: ChildBound
          val child: Child[B] = child0.typedAs[B]
          given Type[B] = child.tpe
          val value: O[B] = f[B](child, i[B](child))
          value.asInstanceOf[O[ChildBound]]
        }

    }

    // TODO (KR) : add bounds
    type ChildFunction2[I1[_], I2[_], O[_]] = [b <: ChildBound] => (Quotes, Type[b]) ?=> (Child[b], I1[b], I2[b]) => O[b]
    object ChildFunction2 {
      type Nested[I1[_], I2[_], O1[_], O2[_]] = ChildFunction2[I1, I2, [b] =>> O1[O2[b]]]
      type IdExpr[I1[_], I2[_]] = ChildFunction2[I1, I2, Expr]
      type FExpr[I1[_], I2[_], F[_]] = ChildFunction2.Nested[I1, I2, Expr, F]

      def run[I1[_], I2[_], O[_]](
          i1: ChildFunction0[I1],
          i2: ChildFunction0[I2],
          f: ChildFunction2[I1, I2, O],
      )(using Quotes): Growable[O[ChildBound]] =
        Growable.many(children).map { child0 =>
          type B <: ChildBound
          val child: Child[B] = child0.typedAs[B]
          given Type[B] = child.tpe
          val value: O[B] = f[B](child, i1[B](child), i2[B](child))
          value.asInstanceOf[O[ChildBound]]
        }

    }

    @scala.annotation.nowarn("msg=unused import")
    final def showTypeClassInstances[F[_]: Type](using Quotes): Unit =
      children.foreach { child0 =>
        type B <: ChildBound
        val child: Child[B] = child0.typedAs[B]
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
          valType: ValDef.ValType = ValDef.ValType.Val,
      )(
          makeVal: ChildFunction0.FExpr[F],
      ): ValDefinitions[F, A] = {
        def rec[Out: Type](
            queue: List[AnyChild],
            acc: Growable[Expressions.Elem[F, ?]],
            build: Expressions[F, A] => Expr[Out],
        )(using Quotes): Term =
          queue match {
            case child0 :: tail =>
              type B <: ChildBound
              val child: Child[B] = child0.typedAs[B]
              given Type[B] = child.tpe

              ValDef.let(
                Symbol.spliceOwner,
                valName(child.name),
                makeVal(child).toTerm,
                valType.toFlags,
              ) { valRef =>
                rec(
                  tail,
                  acc :+ Expressions.Elem(child.tpe, valRef.asExprOf[F[B]]),
                  build,
                )
              }
            case Nil =>
              val exprs: Expressions[F, A] = new Expressions[F, A](fTpe, tpe, acc.toContiguous)
              build(exprs).toTerm
          }

        new ValDefinitions[F, A]([b] =>
          (_, _) ?=>
            (build: Expressions[F, A] => Expr[b]) =>
              rec(
                children.toList,
                Growable.empty,
                build,
              ).asExprOf[b],
        )
      }

      final def summonTypeClasses[F[_]: Type](
          valName: String => String = n => s"instance_$n",
          valType: ValDef.ValType = ValDef.ValType.LazyVal,
      ): ValDefinitions[F, A] =
        apply[F](valName = valName, valType = valType) { [b <: ChildBound] => (_, _) ?=> (child: Child[b]) => child.summonTypeClass[F] }

      final def summonTypeClassesOrDerive[F[_]: Type](
          valName: String => String = n => s"instance_$n",
          valType: ValDef.ValType = ValDef.ValType.LazyVal,
      )(
          f: ChildFunction0.FExpr[F],
      ): ValDefinitions[F, A] =
        apply[F](valName = valName, valType = valType) { [b <: ChildBound] => (_, _) ?=> (child: Child[b]) => child.summonTypeClassOrDerive[F] { f(child) } }

      /**
        * Sets each childs value to f(prevValue, field).
        */
      def foldLeft[V: Type](
          valName: String => String = n => s"value_$n",
          valType: ValDef.ValType = ValDef.ValType.Val,
      )(zero: Expr[V])(
          f: ChildFunction1[K0.Const[Expr[V]], K0.Const[Expr[V]]],
      ): ValDefinitions[K0.Const[V], A] = {
        def rec[Out: Type](
            queue: List[AnyChild],
            prevValue: Expr[V],
            acc: Growable[Expressions.Elem[K0.Const[V], ?]],
            build: Expressions[K0.Const[V], A] => Expr[Out],
        )(using Quotes): Term =
          queue match {
            case child0 :: tail =>
              type B <: ChildBound
              val child: Child[B] = child0.typedAs[B]
              given Type[B] = child.tpe

              val newValue: Expr[V] = f(child, prevValue)

              ValDef.let(
                Symbol.spliceOwner,
                valName(child.name),
                newValue.toTerm, // val _ = `newValue`
                valType.toFlags,
              ) { valRef =>
                rec(
                  tail,
                  newValue, // pass `newValue`
                  acc :+ Expressions.Elem(child.tpe, valRef.asExprOf[V]),
                  build,
                )
              }
            case Nil =>
              val exprs: Expressions[K0.Const[V], A] = new Expressions[K0.Const[V], A](Type.of[K0.Const[V]], tpe, acc.toContiguous)
              build(exprs).toTerm
          }

        new ValDefinitions[K0.Const[V], A]([b] =>
          (_, _) ?=>
            (build: Expressions[K0.Const[V], A] => Expr[b]) =>
              rec(
                children.toList,
                zero,
                Growable.empty,
                build,
              ).asExprOf[b],
        )
      }

      /**
        * Sets the first childs value to [[zero]].
        * Sets each following childs value to f(prevValue, prevField).
        * Also exposes the value of f(lastValue, lastField).
        */
      def foldLeftDelayed[V: Type](
          valName: String => String = n => s"value_$n",
          valType: ValDef.ValType = ValDef.ValType.Val,
      )(zero: Expr[V])(
          f: ChildFunction1[K0.Const[Expr[V]], K0.Const[Expr[V]]],
      ): ValDefinitionsWith[K0.Const[V], A, V] = {
        def rec[Out: Type](
            queue: List[AnyChild],
            prevValue: Expr[V],
            acc: Growable[Expressions.Elem[K0.Const[V], ?]],
            build: (Expressions[K0.Const[V], A], Expr[V]) => Expr[Out],
        )(using Quotes): Term =
          queue match {
            case child0 :: tail =>
              type B <: ChildBound
              val child: Child[B] = child0.typedAs[B]
              given Type[B] = child.tpe

              val newValue: Expr[V] = f(child, prevValue)

              ValDef.let(
                Symbol.spliceOwner,
                valName(child.name),
                prevValue.toTerm, // val _ = `prevValue`
                valType.toFlags,
              ) { valRef =>
                rec(
                  tail,
                  newValue, // pass `newValue`
                  acc :+ Expressions.Elem(child.tpe, valRef.asExprOf[V]),
                  build,
                )
              }
            case Nil =>
              val exprs: Expressions[K0.Const[V], A] = new Expressions[K0.Const[V], A](Type.of[K0.Const[V]], tpe, acc.toContiguous)
              build(exprs, prevValue).toTerm
          }

        new ValDefinitionsWith[K0.Const[V], A, V]([b] =>
          (_, _) ?=>
            (build: (Expressions[K0.Const[V], A], Expr[V]) => Expr[b]) =>
              rec(
                children.toList,
                zero,
                Growable.empty,
                build,
              ).asExprOf[b],
        )
      }

    }

    val cacheVals: CacheVals = new CacheVals

    /////// MapChildren ///////////////////////////////////////////////////////////////

    class MapChildren {

      def map[Out](f: ChildFunction0[K0.Const[Out]])(using Quotes): Growable[Out] = ChildFunction0.run(f)
      def mapExpr[Out](f: ChildFunction0[K0.Const[Expr[Out]]])(using Quotes): Growable[Expr[Out]] = map[Expr[Out]](f)

      def flatMap[S[_]: SeqOps, Out](f: ChildFunction0[K0.Const[S[Out]]])(using Quotes): Growable[Out] = map(f).flatMap { inner => Growable.many(inner: S[Out]) }
      def flatMapExpr[S[_]: SeqOps, Out](f: ChildFunction0[K0.Const[S[Expr[Out]]]])(using Quotes): Growable[Expr[Out]] = flatMap[S, Expr[Out]](f)

      def foldLeft[Out](zero: Out)(f: ChildFunction1[K0.Const[Out], K0.Const[Out]])(using Quotes): Out =
        children.foldLeft(zero) { (acc, child0) =>
          type T <: ChildBound
          val child: Child[T] = child0.typedAs[T]
          given Type[T] = child.tpe
          f(child, acc)
        }
      def foldLeftExpr[Out: Type](zero: Expr[Out])(f: ChildFunction1[K0.Const[Expr[Out]], K0.Const[Expr[Out]]])(using Quotes): Expr[Out] =
        foldLeft[Expr[Out]](zero)(f)

    }

    val mapChildren: MapChildren = new MapChildren

  }
  object Generic {

    def apply[A: Generic as g]: Generic[A] = g

    def of[A](using Type[A], Quotes): ProductOrSumGeneric[A] = Generic.of[A](Derivable.Config())
    def of[A](config: Derivable.Config)(using Type[A], Quotes): ProductOrSumGeneric[A] = {
      val repr: TypeRepr = TypeRepr.of[A]
      val sym: Symbol = repr.typeOrTermSymbol
      sym.typeType match {
        case Some(_: TypeType.Case)   => ProductGeneric.unsafeOf[A](repr, sym, config)
        case Some(_: TypeType.Sealed) => SumGeneric.unsafeOf[A](repr, sym, config)
        case None                     => report.errorAndAbort(s"Type ${TypeRepr.of[A].show} is not a product or sum type")
      }
    }

  }

  sealed trait ProductOrSumGeneric[A] extends Generic[A] {
    val typeType: TypeType
    val derivedFromConfig: Derivable.Config
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ProductGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait ProductGeneric[A] private extends ProductOrSumGeneric[A] { productGeneric =>

    override final type ChildBound = Any
    override final type Child[B] = Field[B]

    override type SelfType[A2] <: ProductGeneric[A2]

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
    ) extends Entity.Child[Any, B, A] {

      override type SelfType[A2] = Field[A2]

      override val childType: String = "field"

      override val label: String = constructorValDef.name

      override val sym: Symbol = constructorValDef.symbol

      override def parentGeneric: ProductGeneric[A] = productGeneric

      override def pos: Position = constructorValDef.pos

      override def annotations(using Quotes): AnnotationsTyped[B] = AnnotationsTyped(constructorValDef.symbol.annotations.all, constructorValDef.show)

      def fromParentTerm(parent: Term): Term = parent.select(fieldValDef.symbol)

      /**
        * Go from a product type to its field.
        * final case class Person(first: String, last: String)
        *
        * field.fromParent(p) -> `p.first`
        */
      def fromParent(parent: Expr[A])(using quotes: Quotes): Expr[B] = fromParentTerm(parent.toTerm).asExprOf[B]
      def fromParentExpr(using quotes: Quotes): Expr[A => B] = '{ (a: A) => ${ fromParent('a) } }

      /**
        * Will get the default value in a situation like `final case class Person(first: String = "F")` (returns "F").
        * If you use `-Yretain-trees` compiler flag, you might be able to retrieve the actual expr "F".
        * This is not guaranteed, and if the actual expr itself can not be retrieved, you will get a reference to a function that returns the default value.
        */
      def constructorDefault: Option[Expr[B]] = {
        val defaultFunctionName: String = s"$$lessinit$$greater$$default$$${idx + 1}"
        val optTerm: Option[Term] =
          productGeneric.sym.companionModule.declaredMethod(defaultFunctionName).headOption.map { sym =>
            sym.tree.narrowOpt[DefDef].flatMap(_.rhs).getOrElse(sym.toTerm)
          }

        optTerm.map(_.asExprOf[B])
      }

    }

    /////// Instantiate ///////////////////////////////////////////////////////////////

    class Instantiate {

      def fieldsToInstance[S[_]: SeqOps](exprs: S[Expr[?]])(using Quotes): Expr[A] =
        productGeneric.fieldsToInstance(exprs)

      def id(
          f: ChildFunction0.IdExpr,
      )(using Quotes): Expr[A] =
        productGeneric.fieldsToInstance(ChildFunction0.run(f))

      def monad[F[_]: {ExprMonad as monad, Type}](
          f: ChildFunction0.FExpr[F],
      )(using Quotes): Expr[F[A]] = {
        def rec(queue: List[AnyChild], acc: Growable[Expr[?]])(using Quotes): Expr[F[A]] =
          queue match {
            case child0 :: tail =>
              type B
              val child: Field[B] = child0.typedAs[B]
              @scala.annotation.unused
              given Type[B] = child.tpe
              val value: Expr[F[B]] = f[B](child)

              tail match {
                case Nil => monad.mapE(value) { a => productGeneric.fieldsToInstance((acc :+ a).to[Contiguous]) }
                case _   => monad.flatMapE(value) { a => rec(tail, acc :+ a) }
              }
            case Nil =>
              monad.pure(productGeneric.fieldsToInstance(acc.to[Contiguous]))
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

    /////// Misc ///////////////////////////////////////////////////////////////

    def filtered[SubsetT](f: ChildFunction0[K0.Const[Boolean]])(using Quotes): ProductGeneric.Subset[A, SubsetT] = {
      val fieldSubset: Contiguous[Field[?]] = fields.filter { field0 =>
        type T
        val field: Field[T] = field0.typedAs[T]
        given Type[T] = field.tpe
        f(field)
      }

      fieldSubset match {
        case Contiguous() =>
          val _unitTypeRepr: TypeRepr = TypeRepr.of[Unit]
          val empty: ProductGeneric.Subset.Empty[A] =
            new ProductGeneric.Subset.Empty[A] {
              override val unitTypeRepr: TypeRepr = _unitTypeRepr
              override val aGeneric: ProductGeneric[A] = productGeneric
            }
          empty.typedAs[SubsetT]
        case Contiguous(aField0) =>
          val _aField: Field[SubsetT] = aField0.typedAs[SubsetT]
          new ProductGeneric.Subset.Single[A, SubsetT] {
            override val aGeneric: productGeneric.type = productGeneric
            override val aField: aGeneric.Field[SubsetT] = _aField
          }
        case _aFields =>
          val tupleTypeRepr: TypeRepr = TypeRepr.tuplePreferTupleN(_aFields.map(_.typeRepr).toList)
          new ProductGeneric.Subset.Many[A, SubsetT] {
            override val aGeneric: productGeneric.type = productGeneric
            override val aFields: Contiguous[productGeneric.Field[?]] = _aFields
            override val bGeneric: ProductGeneric.CaseClassGeneric[SubsetT] =
              ProductGeneric.CaseClassGeneric.unsafeOf[SubsetT](tupleTypeRepr, tupleTypeRepr.typeSymbol, productGeneric.derivedFromConfig)
          }
      }
    }

  }
  object ProductGeneric {

    def apply[A: ProductGeneric as g]: ProductGeneric[A] = g

    trait CaseObjectGeneric[A] extends ProductGeneric[A] { generic =>

      override final type SelfType[A2] = CaseObjectGeneric[A2]

      override final val fields: Contiguous[Field[?]] = Contiguous.empty

      override val typeType: TypeType.Case.Object

      def fieldsToInstance0(using Quotes): Expr[A]
      override final def fieldsToInstance[S[_]: SeqOps](exprs: S[Expr[?]])(using Quotes): Expr[A] =
        exprs.into[List] match {
          case Nil   => fieldsToInstance0
          case exprs => report.errorAndAbort(s"attempted to instantiate case object with non-empty fields (${exprs.size})")
        }

      /////// Instantiate ///////////////////////////////////////////////////////////////

      class CaseObjectInstantiate extends Instantiate {

        def instance(using Quotes): Expr[A] = generic.fieldsToInstance(Nil)

      }

      override val instantiate: CaseObjectInstantiate = new CaseObjectInstantiate

    }
    object CaseObjectGeneric {

      private[ProductGeneric] def unsafeOf[A](
          _typeRepr: TypeRepr,
          _termSym: Symbol,
          config: Derivable.Config,
      ): CaseObjectGeneric[A] =
        new CaseObjectGeneric[A] {

          override val label: String = _termSym.name
          override val sym: Symbol = _termSym
          override val typeRepr: TypeRepr = _typeRepr
          override val typeType: TypeType.Case.Object = _typeRepr.typeTypeCaseObject.get
          override val derivedFromConfig: Derivable.Config = config

          override def fieldsToInstance0(using Quotes): Expr[A] =
            _termSym.toTerm.asExprOf[A]

        }

    }

    trait CaseClassGeneric[A] extends ProductGeneric[A] {
      override val typeType: TypeType.Case.Class
    }
    object CaseClassGeneric {

      private[ProductGeneric] def unsafeOf[A](
          _typeRepr: TypeRepr,
          _typeSym: Symbol,
          config: Derivable.Config,
      )(using Quotes): CaseClassGeneric[A] = {
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
          override val derivedFromConfig: Derivable.Config = config

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

      def anyValFieldTypedAs[TypeName]: AnyValGeneric[A, TypeName] = this.asInstanceOf[AnyValGeneric[A, TypeName]]

      val field: Field[B]

      given bTpe: Type[B] = field.tpe

      override final lazy val fields: Contiguous[Field[?]] = Contiguous.single(field)

      def fieldsToInstance1(expr: Expr[B])(using Quotes): Expr[A]
      override final def fieldsToInstance[S[_]: SeqOps](exprs: S[Expr[?]])(using Quotes): Expr[A] =
        exprs.into[List] match {
          case expr :: Nil => fieldsToInstance1(expr.asExprOf[B])
          case exprs       => report.errorAndAbort(s"attempted to instantiate AnyVal with non-single fields (${exprs.size})")
        }

      class AnyValUtil {

        def wrap(value: Expr[B])(using Quotes): Expr[A] = fieldsToInstance1(value)

        def unwrap(value: Expr[A])(using Quotes): Expr[B] = field.fromParent(value)

      }

      val anyVal: AnyValUtil = new AnyValUtil

    }
    object AnyValGeneric {

      private[ProductGeneric] def unsafeOf[A](
          _typeRepr: TypeRepr,
          _typeSym: Symbol,
          config: Derivable.Config,
      )(using Quotes): AnyValGeneric[A, ?] = {
        val g: CaseClassGeneric[A] = CaseClassGeneric.unsafeOf[A](_typeRepr, _typeSym, config)

        val _gField: g.Field[?] = g.fields match {
          case Contiguous(field) => field
          case _                 => report.errorAndAbort("AnyVal has non-single param?")
        }

        type B
        val gField: g.Field[B] = _gField.typedAs[B]

        new AnyValGeneric[A, B] {

          override val label: String = g.label
          override val sym: Symbol = g.sym
          override val typeRepr: TypeRepr = g.typeRepr
          override val derivedFromConfig: Derivable.Config = g.derivedFromConfig

          override val typeType: TypeType.Case.Class = g.typeType
          override val field: Field[B] =
            Field(
              idx = gField.idx,
              typeRepr = gField.typeRepr,
              constructorValDef = gField.constructorValDef,
              fieldValDef = gField.fieldValDef,
            )

          override def fieldsToInstance1(expr: Expr[B])(using Quotes): Expr[A] =
            g.fieldsToInstance(expr :: Nil)

        }
      }

    }

    sealed trait Subset[A, B] { self =>

      final def typedAs[B2]: Subset[A, B2] = this.asInstanceOf[Subset[A, B2]]

      val subsetType: String
      def aGeneric: ProductGeneric[A]
      def bGeneric: Generic[B]

      final given aTpe: Type[A] = aGeneric.tpe
      final given bTpe: Type[B] = bGeneric.tpe

      def convert(a: Expr[A])(using Quotes): Expr[B]
      final def convertExpr(using Quotes): Expr[A => B] = '{ (a: A) => ${ convert('a) } }

      def convertExpressions[F[_]](expressions: Expressions[F, A])(using Quotes): Expressions[F, B]

      private def toSpecific[S <: Subset[A, ?]](filterType: String)(f: PartialFunction[Subset[A, B], S])(using Quotes): S =
        f.applyOrElse(
          this,
          _ => report.errorAndAbort(s"Unable to filter $this to SubsetGeneric.$filterType"),
        )

      final def toEmpty(using Quotes): Subset.Empty[A] = toSpecific("Empty") { case s: Subset.Empty[A @unchecked] => s }
      final def toNonEmpty(using Quotes): Subset.NonEmpty[A, B] = toSpecific("NonEmpty") { case s: Subset.NonEmpty[A, B] => s }
      final def toSingle(using Quotes): Subset.Single[A, B] = toSpecific("Single") { case s: Subset.Single[A, B] => s }
      final def toMany(using Quotes): Subset.Many[A, B] = toSpecific("Many") { case s: Subset.Many[A, B] => s }

      object subInstance {

        /**
          * This only works if the [[Derivable.ProductDeriver]] in your [[Derivable]] uses [[Derivable.ProductDeriver.withInstances]].
          */
        final def fromDerivable[F[_]: Type as fTpe](
            derivable: Derivable[F],
            aInstances: Expressions[F, A],
        )(using quotes: Quotes): Expr[F[B]] = {
          def deriveWithInstances(generic: ProductGeneric[B]): Expr[F[B]] =
            derivable
              .productDeriverInternal[B](using quotes, fTpe, bTpe, generic, derivable)
              .deriveWithInstances(convertExpressions(aInstances))

          self match
            case subset: Subset.Empty[A @unchecked] => deriveWithInstances(subset.bGenericTyped[B])
            case subset: Subset.Single[A, B]        => subset.aField.getExpr(aInstances)
            case subset: Subset.Many[A, B]          => deriveWithInstances(subset.bGeneric)
        }

        /**
          * This only works if the [[Derivable.ProductDeriver]] in your [[Derivable]] uses [[Derivable.ProductDeriver.withInstances]].
          */
        final def fromDerivable[F[_]: Type as fTpe](
            derivable: Derivable[F],
            aInstances: Expressions[F, A],
            emptyInstance: => Expr[F[Unit]],
        )(using quotes: Quotes): Expr[F[B]] = {
          def deriveWithInstances(generic: ProductGeneric[B]): Expr[F[B]] =
            derivable
              .productDeriverInternal[B](using quotes, fTpe, bTpe, generic, derivable)
              .deriveWithInstances(convertExpressions(aInstances))

          self match
            case _: Subset.Empty[A @unchecked] => emptyInstance.asExprOf[F[B]]
            case subset: Subset.Single[A, B]   => subset.aField.getExpr(aInstances)
            case subset: Subset.Many[A, B]     => deriveWithInstances(subset.bGeneric)
        }

        final def fromDeriver[F[_]: Type as fTpe](
            productDeriver: (Quotes, Type[F], Type[B], ProductGeneric[B]) ?=> Expressions[F, B] => Derivable.ProductDeriver[F, B],
            aInstances: Expressions[F, A],
        )(using quotes: Quotes): Expr[F[B]] = {
          def deriveWithInstances(generic: ProductGeneric[B]): Expr[F[B]] =
            productDeriver(using quotes, fTpe, bTpe, generic)(convertExpressions(aInstances)).derive

          self match
            case subset: Subset.Empty[A @unchecked] => deriveWithInstances(subset.bGenericTyped[B])
            case subset: Subset.Single[A, B]        => subset.aField.getExpr(aInstances)
            case subset: Subset.Many[A, B]          => deriveWithInstances(subset.bGeneric)
        }

        final def fromDeriver[F[_]: Type as fTpe](
            productDeriver: (Quotes, Type[F], Type[B], ProductGeneric[B]) ?=> Expressions[F, B] => Derivable.ProductDeriver[F, B],
            aInstances: Expressions[F, A],
            emptyInstance: => Expr[F[Unit]],
        )(using quotes: Quotes): Expr[F[B]] = {
          def deriveWithInstances(generic: ProductGeneric[B]): Expr[F[B]] =
            productDeriver(using quotes, fTpe, bTpe, generic)(convertExpressions(aInstances)).derive

          self match
            case _: Subset.Empty[A @unchecked] => emptyInstance.asExprOf[F[B]]
            case subset: Subset.Single[A, B]   => subset.aField.getExpr(aInstances)
            case subset: Subset.Many[A, B]     => deriveWithInstances(subset.bGeneric)
        }

      }

      override final def toString: String =
        s"Subset.$subsetType[${aGeneric.typeRepr.showCode}, ${bGeneric.typeRepr.showCode}]"

    }
    object Subset {

      trait Empty[A] extends Subset[A, Unit] {

        // do not do `override val unitTypeRepr: TypeRepr = TypeRepr.of[Unit]`. `given bTpe: Type[B = Unit]` will mess this up.
        val unitTypeRepr: TypeRepr
        override final val subsetType: String = "Empty"

        override final lazy val bGeneric: ProductGeneric.CaseObjectGeneric[Unit] =
          new ProductGeneric.CaseObjectGeneric[Unit] {
            override val label: String = "Unit"
            override val typeRepr: TypeRepr = unitTypeRepr
            override val sym: Symbol = typeRepr.termSymbol
            override val typeType: TypeType.Case.Object = TypeType.CaseObject
            override val derivedFromConfig: Derivable.Config = aGeneric.derivedFromConfig
            override def fieldsToInstance0(using Quotes): Expr[Unit] = '{ () }
          }

        final def bGenericTyped[B]: ProductGeneric.CaseObjectGeneric[B] = bGeneric.typedAs[B]

        override final def convert(a: Expr[A])(using Quotes): Expr[Unit] = bGeneric.instantiate.instance

        override final def convertExpressions[F[_]](expressions: Expressions[F, A])(using Quotes): Expressions[F, Unit] =
          new Expressions[F, Unit](expressions.fTpe, bGeneric.tpe, Contiguous.empty)

      }

      sealed trait NonEmpty[A, T] extends Subset[A, T]

      trait Single[A, B] extends Subset.NonEmpty[A, B] {

        override final val subsetType: String = "Single"
        override val aGeneric: ProductGeneric[A]
        val aField: aGeneric.Field[B]
        override final lazy val bGeneric: IdentityGeneric[B] =
          new IdentityGeneric[B] {
            override val label: String = aField.label
            override val sym: Symbol = aField.typeRepr.typeSymbol
            override val typeRepr: TypeRepr = aField.typeRepr
          }

        override final def convert(a: Expr[A])(using Quotes): Expr[B] = aField.fromParent(a)

        override final def convertExpressions[F[_]](expressions: Expressions[F, A])(using Quotes): Expressions[F, B] =
          new Expressions[F, B](expressions.fTpe, bGeneric.tpe, Contiguous.single(expressions.expressions.at(aField.idx)))

      }

      trait Many[A, B] extends Subset.NonEmpty[A, B] {
        override final val subsetType: String = "Many"
        override val aGeneric: ProductGeneric[A]
        val aFields: Contiguous[aGeneric.Field[?]]
        override val bGeneric: ProductGeneric.CaseClassGeneric[B]

        override final def convert(a: Expr[A])(using Quotes): Expr[B] =
          bGeneric.instantiate.id { [b] => (_, _) ?=> (bField: bGeneric.Field[b]) =>
            aFields
              .at(bField.idx)
              .typedAs[b]
              .fromParent(a)
          }

        override final def convertExpressions[F[_]](expressions: Expressions[F, A])(using Quotes): Expressions[F, B] =
          new Expressions[F, B](expressions.fTpe, bGeneric.tpe, aFields.map { aField => expressions.expressions.at(aField.idx) })

      }

    }

    private[K0] def unsafeOf[A](
        repr: TypeRepr,
        sym: Symbol,
        config: Derivable.Config,
    )(using Quotes): ProductGeneric[A] =
      sym.typeTypeCase match {
        case Some(_: TypeType.Case.Class) =>
          repr.typeSymbol.tree match {
            case cdef: ClassDef if cdef.parents.headOption.flatMap(_.narrowOpt[TypeTree]).exists(_.tpe =:= TypeRepr.of[AnyVal]) =>
              AnyValGeneric.unsafeOf[A](repr, sym, config)
            case _ =>
              CaseClassGeneric.unsafeOf[A](repr, sym, config)
          }
        case Some(_: TypeType.Case.Object) =>
          CaseObjectGeneric.unsafeOf[A](repr, sym, config)
        case None => report.errorAndAbort(s"Not a product type: ${repr.show}")
      }

    def of[A](using Type[A], Quotes): ProductGeneric[A] = ProductGeneric.of[A](Derivable.Config())
    def of[A](config: Derivable.Config)(using Type[A], Quotes): ProductGeneric[A] =
      Generic.of[A](config) match
        case g: ProductGeneric[A] => g
        case _                    => report.errorAndAbort(s"Not a product type: ${TypeRepr.of[A].show}", TypeRepr.of[A].typeOrTermSymbol.pos)

    def ofTuple[A](tupleTypes: List[TypeRepr], config: Derivable.Config = Derivable.Config())(using Quotes): ProductGeneric[A] = {
      if (tupleTypes.length < 2) report.errorAndAbort("`ProductGeneric.ofTuple` only works with tuple size >= 2")

      val typeRepr: TypeRepr = TypeRepr.tuplePreferTupleN(tupleTypes)
      val typeSym = typeRepr.typeSymbol
      ProductGeneric.unsafeOf[A](typeRepr, typeSym, config)
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      SumGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait SumGeneric[A] private extends ProductOrSumGeneric[A] { sumGeneric =>

    override final type ChildBound = A
    type Gen[b] <: ProductOrSumGeneric[b]
    override type SelfType[A2] <: SumGeneric[A2]
    override final type Child[B <: A] = Case[B]

    override val typeType: TypeType.Sealed

    def cases: Contiguous[Case[? <: A]]

    override final def children: Contiguous[AnyChild] = cases.asInstanceOf[Contiguous[AnyChild]]

    /////// Case ///////////////////////////////////////////////////////////////

    final case class Case[B <: A](
        idx: Int,
        generic: Gen[B],
    ) extends Entity.Child.Deferred[A, B, A](generic) {

      override type SelfType[A2 <: A] = Case[A2]

      override val childType: String = "case"

      override def parentGeneric: SumGeneric[A] = sumGeneric

      def caseExtractor: CaseExtractor[B, Expr[B]] =
        caseExtractor(s => s"case_$s")

      def caseExtractor(bindName: String => String): CaseExtractor[B, Expr[B]] =
        caseExtractor(bindName(name))

      def caseExtractor(bindName: String): CaseExtractor[B, Expr[B]] =
        generic match {
          case caseObject: ProductGeneric.CaseObjectGeneric[B] =>
            CaseExtractor.const[B](caseObject.instantiate.instance).map { _ => caseObject.instantiate.instance }
          case generic: Generic[B] =>
            CaseExtractor.extract[B](bindName)
        }

    }

    /////// Matching ///////////////////////////////////////////////////////////////

    class Matcher {

      // TODO (KR) : support a generic combinator that would look something like:
      //           : (matcher.instance ++ matcher.instance ++ matcher.value[String]).make[Out] { _ => ??? }

      def make[In[_ <: A], Out: Type](
          f: ChildFunction0[[b <: A] =>> MatchBuilder[In[b], Out]],
      )(using Quotes): MatchBuilder[In[A], Out] = {
        val widened: ChildFunction0[K0.Const[MatchBuilder[In[A], Out]]] = f.asInstanceOf[ChildFunction0[K0.Const[MatchBuilder[In[A], Out]]]]
        MatchBuilder.merge(mapChildren.map(widened))
      }

      def value[In: Type, Out: Type](expr: Expr[In])(
          f: ChildFunction0[K0.Const[MatchBuilder[In, Out]]],
      )(
          elseCase: Quotes ?=> Expr[Out],
      )(using Quotes): Expr[Out] =
        make[K0.Const[In], Out](f).withWildcard(elseCase).matchOn(expr)

      def instance[Out: Type](expr: Expr[A])(
          f: ChildFunction0[[b <: A] =>> MatchBuilder[b, Out]],
      )(using Quotes): Expr[Out] =
        make[K0.Id, Out](f).matchOn(expr)

      def instance2[Out: Type](expr: Expr[(A, A)])(
          f: ChildFunction0[[b <: A] =>> MatchBuilder[(b, b), Out]],
      )(
          elseCase: Quotes ?=> Expr[Out],
      )(using Quotes): Expr[Out] =
        make[[b] =>> (b, b), Out](f).withWildcard(elseCase).matchOn(expr)

      def instance2[Out: Type](expr1: Expr[A], expr2: Expr[A])(
          f: ChildFunction0[[b <: A] =>> MatchBuilder[(b, b), Out]],
      )(
          elseCase: Quotes ?=> Expr[Out],
      )(using Quotes): Expr[Out] =
        instance2('{ ($expr1, $expr2) })(f)(elseCase)

      def instance3[Out: Type](expr: Expr[(A, A, A)])(
          f: ChildFunction0[[b <: A] =>> MatchBuilder[(b, b, b), Out]],
      )(
          elseCase: Quotes ?=> Expr[Out],
      )(using Quotes): Expr[Out] =
        make[[b] =>> (b, b, b), Out](f).withWildcard(elseCase).matchOn(expr)

      def instance3[Out: Type](expr1: Expr[A], expr2: Expr[A], expr3: Expr[A])(
          f: ChildFunction0[[b <: A] =>> MatchBuilder[(b, b, b), Out]],
      )(
          elseCase: Quotes ?=> Expr[Out],
      )(using Quotes): Expr[Out] =
        instance3('{ ($expr1, $expr2, $expr3) })(f)(elseCase)

    }

    val matcher: Matcher = new Matcher

  }
  object SumGeneric {

    def apply[A: SumGeneric as g]: SumGeneric[A] = g

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
      override type SelfType[A2] <: FlatGeneric[A2]
    }
    object FlatGeneric {

      def of[A](using Type[A], Quotes): SumGeneric.FlatGeneric[A] =
        SumGeneric.of[A](Derivable.Config(defaultUnrollStrategy = SumGeneric.UnrollStrategy.Unroll)) match
          case gen: SumGeneric.FlatGeneric[A] => gen
          case gen                            => report.errorAndAbort(s"internal defect : FlatGeneric.of did not return a FlagGeneric:\n$gen")

    }

    private[SumGeneric] trait FlatNonEnumGeneric[A] extends FlatGeneric[A] {
      override final type Gen[b] = ProductGeneric[b]
      override final type SelfType[A2] = FlatNonEnumGeneric[A2]
    }

    trait EnumGeneric[A] extends FlatGeneric[A] {
      override final type Gen[b] = ProductGeneric.CaseObjectGeneric[b]
      override final type SelfType[A2] = EnumGeneric[A2]
    }
    object EnumGeneric {

      def of[A](using Type[A], Quotes): SumGeneric.EnumGeneric[A] =
        SumGeneric.of[A](Derivable.Config(defaultUnrollStrategy = SumGeneric.UnrollStrategy.Unroll)) match
          case gen: SumGeneric.EnumGeneric[A] => gen
          case gen                            => report.errorAndAbort(s"internal defect : EnumGeneric.of did not return an EnumGeneric:\n$gen")

      private def extractSum[A: Type](using Quotes): ArraySeq[ProductGeneric[? <: A]] =
        SumGeneric.FlatGeneric.of[A].children.map(_.generic).toArraySeq

      private def extract[A: Type](validateNumCaseClasses: Int => Boolean, expectedSize: String)(using Quotes): Expr[ArraySeq[A]] = {
        val typeRepr: TypeRepr = TypeRepr.of[A].dealias
        val children: ArraySeq[ProductGeneric[? <: A]] =
          typeRepr match {
            case or: OrType =>
              ArraySeq.from(or.orChildren).map { t =>
                type B <: A
                ProductGeneric.unsafeOf[B](t, t.typeOrTermSymbol, Derivable.Config())
              }
            case _ => extractSum[A]
          }

        val (caseObjects, caseClasses) = children.partitionMap {
          case caseObject: ProductGeneric.CaseObjectGeneric[? <: A] => caseObject.asLeft
          case caseClass: ProductGeneric.CaseClassGeneric[? <: A]   => caseClass.asRight
        }

        if (caseObjects.isEmpty)
          report.errorAndAbort(s"No case objects returned for parent ${typeRepr.showAnsiCode}")

        if (!validateNumCaseClasses(caseClasses.length)) {
          val showCaseClasses = caseClasses.map { ccg => s"\n  - ${ccg.typeRepr.showAnsiCode}" }.mkString
          report.errorAndAbort(s"Invalid number of case classes detected for parent ${typeRepr.showAnsiCode}, expected $expectedSize, but found ${caseClasses.length}:$showCaseClasses")
        }

        val values: ArraySeq[Expr[A]] = caseObjects.map { _.instantiate.instance }

        val ctExpr: Expr[ClassTag[A]] = Implicits.searchRequiredIgnoreMessage[ClassTag[A]]
        '{ ArraySeq.apply[A](${ Expr.ofSeq(values) }*)(using $ctExpr).distinct }
      }

      object deriveEnum {

        /**
          * Expects a strict enum, all case object children.
          */
        object strictEnum {
          private def valuesImpl[A: Type](using Quotes): Expr[ArraySeq[A]] = extract[A](_ == 0, "exactly 0")
          inline def values[A]: ArraySeq[A] = ${ valuesImpl[A] }
          inline def map[A, B](f: A => B): Map[B, A] = values[A].map { a => (f(a), a) }.toMap
        }

        /**
          * Expects all case object children, except exactly 1 case class.
          * Useful for an enum with a representation for Other(_).
          */
        object ignoreSingleCaseClass {
          private def valuesImpl[A: Type](using Quotes): Expr[ArraySeq[A]] = extract[A](_ == 1, "exactly 1")
          inline def values[A]: ArraySeq[A] = ${ valuesImpl[A] }
          inline def map[A, B](f: A => B): Map[B, A] = values[A].map { a => (f(a), a) }.toMap
        }

        /**
          * Expects case object children, with more than 1 case class.
          */
        object ignoreManyCaseClasses {
          private def valuesImpl[A: Type](using Quotes): Expr[ArraySeq[A]] = extract[A](_ > 1, "more than 1")
          inline def values[A]: ArraySeq[A] = ${ valuesImpl[A] }
          inline def map[A, B](f: A => B): Map[B, A] = values[A].map { a => (f(a), a) }.toMap
        }

        /**
          * Expects case object children, and does not care about the number of case class children.
          */
        object lax {
          private def valuesImpl[A: Type](using Quotes): Expr[ArraySeq[A]] = extract[A](_ => true, "???")
          inline def values[A]: ArraySeq[A] = ${ valuesImpl[A] }
          inline def map[A, B](f: A => B): Map[B, A] = values[A].map { a => (f(a), a) }.toMap
        }

      }

    }

    trait NestedGeneric[A] extends SumGeneric[A] {
      override final type Gen[b] = ProductOrSumGeneric[b]
      override final type SelfType[A2] = NestedGeneric[A2]
    }
    object NestedGeneric {

      def of[A](using Type[A], Quotes): SumGeneric.NestedGeneric[A] =
        SumGeneric.of[A](Derivable.Config(defaultUnrollStrategy = SumGeneric.UnrollStrategy.Nested)) match
          case gen: SumGeneric.NestedGeneric[A] => gen
          case gen                              => report.errorAndAbort(s"internal defect : NestedGeneric.of did not return a NestedGeneric:\n$gen")

    }

    private[K0] def unsafeOf[A](
        _typeRepr: TypeRepr,
        _typeSym: Symbol,
        config: Derivable.Config,
    )(using Quotes): SumGeneric[A] = {

      def unroll(isRoot: Boolean): Boolean =
        config.defaultUnrollStrategy match
          case UnrollStrategy.Unroll => true
          case UnrollStrategy.Nested => isRoot

      def childGenericsRec(isRoot: Boolean, sym: Symbol): Growable[ProductOrSumGeneric[? <: A]] =
        sym.typeType match {
          case Some(_: TypeType.Case.Class) =>
            val classDef: ClassDef = sym.tree.narrow[ClassDef]
            if (classDef.constructor.paramss.exists { case _: TypeParamClause => true; case _ => false })
              report.errorAndAbort("Type params on sum types is not yet supported")

            type B <: A
            Growable.single(ProductGeneric.unsafeOf[B](sym.typeRef, sym, config))

          case Some(TypeType.CaseObject) =>
            type B <: A
            Growable.single(ProductGeneric.unsafeOf[B](sym.termRef, sym, config))

          case Some(TypeType.EnumCaseObject) =>
            type B <: A
            Growable.single(ProductGeneric.unsafeOf[B](Singleton.companion.apply(sym.termRef.toTerm).tpe, sym, config))

          case Some(TypeType.Scala2CaseObject) =>
            // TODO (KR) :
            report.errorAndAbort("TODO : support scala-2 case object")

          case Some(_: TypeType.Sealed) =>
            val classDef: ClassDef = sym.tree.narrow[ClassDef]
            if (classDef.constructor.paramss.exists { case _: TypeParamClause => true; case _ => false })
              report.errorAndAbort("Type params on sum types is not yet supported")

            if (unroll(isRoot)) {
              val children = sym.children
              if (children.isEmpty)
                report.errorAndAbort(s"Sum type ${sym.name} has no children", sym.pos)

              Growable.many(children).flatMap(childGenericsRec(false, _))
            } else {
              type B <: A
              val typeRepr: TypeRepr = sym.typeRef
              given Type[B] = typeRepr.asTypeOf
              Growable.single(Generic.of[B](config))
            }

          case None => report.errorAndAbort(s"Type ${sym.name} is not a product or sum type", sym.pos)
        }

      val childGenerics: Contiguous[ProductOrSumGeneric[? <: A]] =
        childGenericsRec(true, _typeSym).toContiguous.sorted(using config.defaultOrdinalStrategy.ord)

      val filteredGenerics: Either[Contiguous[ProductOrSumGeneric[? <: A]], Either[Contiguous[ProductGeneric[? <: A]], Contiguous[ProductGeneric.CaseObjectGeneric[? <: A]]]] =
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
            override val derivedFromConfig: Derivable.Config = config
            override val cases: Contiguous[Case[? <: A]] =
              enums.zipWithIndex.map { case (g, i) => Case(i, g) }
          }
        case Right(Left(products)) =>
          new SumGeneric.FlatNonEnumGeneric[A] {
            override val label: String = _typeSym.name
            override val sym: Symbol = _typeSym
            override val typeRepr: TypeRepr = _typeRepr
            override val typeType: TypeType.Sealed = _typeSym.typeTypeSealed.get
            override val derivedFromConfig: Derivable.Config = config
            override val cases: Contiguous[Case[? <: A]] =
              products.zipWithIndex.map { case (g, i) => Case(i, g) }
          }
        case Left(generics) =>
          new SumGeneric.NestedGeneric[A] {
            override val label: String = _typeSym.name
            override val sym: Symbol = _typeSym
            override val typeRepr: TypeRepr = _typeRepr
            override val typeType: TypeType.Sealed = _typeSym.typeTypeSealed.get
            override val derivedFromConfig: Derivable.Config = config
            override val cases: Contiguous[Case[? <: A]] =
              generics.zipWithIndex.map { case (g, i) => Case(i, g) }
          }
      }
    }

    def of[A](using Type[A], Quotes): SumGeneric[A] = SumGeneric.of[A](Derivable.Config())
    def of[A](config: Derivable.Config)(using Type[A], Quotes): SumGeneric[A] =
      Generic.of[A](config) match
        case g: SumGeneric[A] => g
        case _                => report.errorAndAbort(s"Not a sum type: ${TypeRepr.of[A].show}", TypeRepr.of[A].typeOrTermSymbol.pos)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      IdentityGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait IdentityGeneric[A] extends Generic[A] { identityGeneric =>
    override final type ChildBound = A
    override type Child[B <: ChildBound] = IdentityChild[B]
    override final type SelfType[A2] = IdentityGeneric[A2]

    final class IdentityChild[B <: A] extends K0.Entity.Child[A, B, A] {
      override type SelfType[A2 <: A] = IdentityChild[A2]
      override val idx: Int = 0
      override val childType: String = "identity"
      override val label: String = identityGeneric.label
      override val sym: Symbol = identityGeneric.sym
      override val typeRepr: TypeRepr = identityGeneric.typeRepr
      override def parentGeneric: IdentityGeneric[A] = identityGeneric
      override def pos: Position = identityGeneric.pos
      override def annotations(using Quotes): AnnotationsTyped[B] = AnnotationsTyped(identityGeneric.annotations.all, typeRepr.show)
    }

    final val child: IdentityChild[A] = new IdentityChild[A]

    override final val children: Contiguous[AnyChild] = Contiguous.single(child)

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

    private[K0] final def productDeriverInternal[A](using Quotes, Type[F], Type[A], ProductGeneric[A], Derivable[F]): Derivable.ProductDeriver[F, A] = productDeriver[A]
    private[K0] final def sumDeriverInternal[A](using Quotes, Type[F], Type[A], SumGeneric[A], Derivable[F]): Derivable.SumDeriver[F, A] = sumDeriver[A]

    private[meta] final def deriveFromGenericImpl[A](g: ProductOrSumGeneric[A])(using Quotes, Type[F], Type[A]): Expr[F[A]] = {
      given Derivable[F] = this
      val res: Expr[F[A]] = g match {
        case g: ProductGeneric[A] =>
          given ProductGeneric[A] = g
          productDeriver[A].derive
        case g: SumGeneric[A] =>
          given SumGeneric[A] = g
          sumDeriver[A].derive
      }

      K0.annotation.showSpecific[F, F[A]](g.annotations, res)

      res
    }

    protected final def derivedImpl[A](using Quotes, Type[F], Type[A]): Expr[F[A]] =
      deriveFromGenericImpl(Generic.of[A](deriveConfig))

    /**
      * Unfortunately, scala macros do not allow this to be implemented in [[Derivable]].
      * Therefore, every companion object that extends [[Derivable]] must implement this function with the following body:
      *      ${ derivedImpl[A] }
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

      /**
        * This only works if the [[Derivable.ProductDeriver]] in your [[Derivable]] uses [[Derivable.ProductDeriver.withInstances]].
        */
      def deriveWithInstances(exprs: Expressions[F, A])(using quotes: Quotes): Expr[F[A]] =
        report.errorAndAbort("`deriveWithInstances` is only supported when you use `ProductDeriver.withInstances`")

    }
    object ProductDeriver {

      final class NotSupported[F[_], A](using Quotes, Type[F], Type[A], ProductGeneric[A]) extends ProductDeriver[F, A] {
        override def derive: Expr[F[A]] = report.errorAndAbort(s"Auto derivation of product-types is not supported for ${Type.show[F]}")
      }

      def notSupported[F[_], A](using Quotes, Type[F], Type[A], ProductGeneric[A]): ProductDeriver[F, A] = new NotSupported[F, A]

      final class WithInstances[F[_], A](f: Quotes ?=> Expressions[F, A] => ProductDeriver[F, A])(using Quotes, Type[F], Type[A], ProductGeneric[A]) extends ProductDeriver[F, A] {

        override def derive: Expr[F[A]] = generic.cacheVals.summonTypeClasses[F]().defineAndUse { f(_).derive }

        override def deriveWithInstances(exprs: Expressions[F, A])(using quotes: Quotes): Expr[F[A]] =
          f(using quotes)(exprs).derive

      }

      def withInstances[F[_], A](f: Quotes ?=> Expressions[F, A] => ProductDeriver[F, A])(using Quotes, Type[F], Type[A], ProductGeneric[A]) =
        new WithInstances[F, A](f)

      final class WithDisjointInstances[ParentF[_], ChildF[_], A](f: Quotes ?=> Expressions[ChildF, A] => ProductDeriver[ParentF, A])(using
          Quotes,
          Type[ParentF],
          Type[ChildF],
          Type[A],
          ProductGeneric[A],
      ) extends ProductDeriver[ParentF, A] {

        override def derive: Expr[ParentF[A]] = generic.cacheVals.summonTypeClasses[ChildF]().defineAndUse { f(_).derive }

      }

      def withDisjointInstances[ParentF[_], ChildF[_], A](f: Quotes ?=> Expressions[ChildF, A] => ProductDeriver[ParentF, A])(using Quotes, Type[ParentF], Type[ChildF], Type[A], ProductGeneric[A]) =
        new WithDisjointInstances[ParentF, ChildF, A](f)

      final class Impl[F[_], A](value: () => Expr[F[A]])(using Quotes, Type[F], Type[A], ProductGeneric[A]) extends ProductDeriver[F, A] {
        override def derive: Expr[F[A]] = value()
      }

      def impl[F[_], A](value: => Expr[F[A]])(using Quotes, Type[F], Type[A], ProductGeneric[A]): ProductDeriver[F, A] =
        new Impl[F, A](() => value)

      abstract class Split[F[_], A](using Quotes, Type[F], Type[A], ProductGeneric[A]) extends ProductDeriver[F, A] {

        def deriveCaseClass(generic: ProductGeneric.CaseClassGeneric[A]): Expr[F[A]]

        def deriveAnyVal[B: Type](generic: ProductGeneric.AnyValGeneric[A, B]): Expr[F[A]] = deriveCaseClass(generic)

        def deriveCaseObject(generic: ProductGeneric.CaseObjectGeneric[A]): Expr[F[A]]

        override final def derive: Expr[F[A]] = generic match {
          case generic0: ProductGeneric.AnyValGeneric[A, _] =>
            type B
            val generic: ProductGeneric.AnyValGeneric[A, B] = generic0.anyValFieldTypedAs[B]
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
          generic.cacheVals.summonTypeClassesOrDerive[F]() { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) => derivable.deriveFromGenericImpl(kase.generic) }.defineAndUse { f(_).derive }
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

    private def specificPos[F[_]: Type](a: Annotations)(using Quotes): Option[Position] = a.optionalOf[annotation.showDerivation[F]].map(_.toTerm.pos)
    private def generalPos(a: Annotations)(using Quotes): Option[Position] = a.optionalOf[annotation.showAllDerivation].map(_.toTerm.pos)

    def showSpecific[F[_]: Type, T: Type](a: Annotations, expr: Expr[?])(using Quotes): Unit =
      specificPos[F](a).orElse(generalPos(a)).foreach { showPos =>
        report.info(s"derivation for ${TypeRepr.of[T].showAnsiCode}:\n\n${expr.showAnsiCode}", showPos)
      }
    def showGeneral[T: Type](a: Annotations, expr: Expr[?])(using Quotes): Unit =
      generalPos(a).foreach { showPos =>
        report.info(s"derivation for ${TypeRepr.of[T].showAnsiCode}:\n\n${expr.showAnsiCode}", showPos)
      }

    final class showAllDerivation extends Annotation
    final class showDerivation[F[_]] extends Annotation

  }

}
