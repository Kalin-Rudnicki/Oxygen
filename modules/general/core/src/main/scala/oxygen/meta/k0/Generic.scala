package oxygen.meta.k0

import oxygen.core.*
import oxygen.core.collection.*
import oxygen.core.typeclass.*
import oxygen.meta.*
import oxygen.meta.k0 as PKG
import oxygen.quoted.*
import scala.collection.immutable.ArraySeq
import scala.quoted.*

sealed trait Generic[A] extends Entity[Any, A] {

  /////// Types ///////////////////////////////////////////////////////////////

  final type AType = A
  final type SelfBound = Any
  override type SelfType[A2] <: Generic[A2]
  type ChildBound <: Any
  type Child[B <: ChildBound] <: Entity.Child[ChildBound, B, A] { type SelfType[A2 <: ChildBound] = Child[A2] }
  final type AnyChild = Child[ChildBound]

  /////// Basic Members ///////////////////////////////////////////////////////////////

  def children: ArraySeq[AnyChild]

  override final def pos: Position = sym.pos.get
  override final def annotations(using Quotes): AnnotationsTyped[A] = AnnotationsTyped(typeRepr.annotations.all, typeRepr.show)

  final def showTypeClassInstances[F[_]: Type](using Quotes): Unit =
    children.foreach { child0 =>
      type B <: ChildBound
      val child: Child[B] = child0.typedAs[B]
      given Type[B] = child.tpe

      def msg(label: String, str: String): String =
        s"""${child.childType}: ${child.label}
           |type: ${child.typeRepr.show}
           |$label: $str""".stripMargin

      Implicits.search(TypeRepr.of[F[B]]) match
        case success: ImplicitSearchSuccess => report.info(msg("instance", success.tree.show), child.pos)
        case failure: ImplicitSearchFailure => report.warning(msg("explanation", failure.explanation), child.pos)
    }

  val cacheVals: CacheVals = new CacheVals

  val mapChildren: MapChildren = new MapChildren

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

    def getExpr[F[_]](exprs: Expressions[F, A]): ChildFunction0.FExpr[F] = //
      { [b <: ChildBound] => (_, _) ?=> (child: Child[b]) =>
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

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Inner Classes
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /////// MapChildren ///////////////////////////////////////////////////////////////

  class MapChildren {

    def map[Out](f: ChildFunction0[PKG.Const[Out]])(using Quotes): Growable[Out] = ChildFunction0.run(f)
    def mapExpr[Out](f: ChildFunction0[PKG.Const[Expr[Out]]])(using Quotes): Growable[Expr[Out]] = map[Expr[Out]](f)

    def mapToSeqExpr[S[_]: {SeqOps, Type}, Out: Type](f: ChildFunction0[PKG.Const[Expr[Out]]])(using Quotes): Expr[S[Out]] = mapExpr[Out](f).seqToExprOf[S]

    def flatMap[S[_]: SeqOps, Out](f: ChildFunction0[PKG.Const[S[Out]]])(using Quotes): Growable[Out] = map(f).flatMap { inner => Growable.many(inner: S[Out]) }
    def flatMapExpr[S[_]: SeqOps, Out](f: ChildFunction0[PKG.Const[S[Expr[Out]]]])(using Quotes): Growable[Expr[Out]] = flatMap[S, Expr[Out]](f)

    def foldLeft[Out](zero: Out)(f: ChildFunction1[PKG.Const[Out], PKG.Const[Out]])(using Quotes): Out =
      children.foldLeft(zero) { (acc, child0) =>
        type T <: ChildBound
        val child: Child[T] = child0.typedAs[T]
        given Type[T] = child.tpe
        f(child, acc)
      }
    def foldLeftExpr[Out: Type](zero: Expr[Out])(f: ChildFunction1[PKG.Const[Expr[Out]], PKG.Const[Expr[Out]]])(using Quotes): Expr[Out] =
      foldLeft[Expr[Out]](zero)(f)

  }

  /////// CacheVals ///////////////////////////////////////////////////////////////

  class CacheVals {

    final def apply[F[_]: Type as fTpe](
        valName: String => String = n => s"value_$n",
        valType: ValDef.ValType = ValDef.ValType.Val,
    )(
        makeVal: ChildFunction0.FExpr[F],
    ): ValDefinitions[F, A] = {
      def makePairs(using Quotes): ArraySeq[(ValDef, Expressions.Elem[F, ?])] =
        mapChildren.map[(ValDef, Expressions.Elem[F, ?])] { [b <: ChildBound] => (_, _) ?=> (child: Child[b]) =>
          val childExpr: Expr[F[b]] = makeVal(child)
          val childTerm: Term = childExpr.toTerm
          val valDef: ValDef = ValDef.newVal(valName(child.name), valType)(childTerm)
          val exprElem: Expressions.Elem[F, b] = Expressions.Elem[F, b](summon[Type[b]], valDef.valRef.asExprOf[F[b]])
          (valDef, exprElem)
        }.toArraySeq

      new ValDefinitions[F, A](
        { [b] => (_, _) ?=> (build: Expressions[F, A] => Expr[b]) =>
          val pairs: ArraySeq[(ValDef, Expressions.Elem[F, ?])] = makePairs
          val valDefs: List[ValDef] = pairs.map(_._1).toList
          val exprs: Expressions[F, A] = new Expressions[F, A](fTpe, tpe, pairs.map(_._2))

          val out: Expr[b] = build(exprs)
          val outWithValDefs: Term = Block.companion.apply(valDefs, out.toTerm)
          outWithValDefs.asExprOf[b]
        },
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
        f: ChildFunction1[PKG.Const[Expr[V]], PKG.Const[Expr[V]]],
    ): ValDefinitions[PKG.Const[V], A] = {
      def rec[Out: Type](
          queue: List[AnyChild],
          prevValue: Expr[V],
          acc: Growable[Expressions.Elem[PKG.Const[V], ?]],
          build: Expressions[PKG.Const[V], A] => Expr[Out],
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
            val exprs: Expressions[PKG.Const[V], A] = new Expressions[PKG.Const[V], A](Type.of[PKG.Const[V]], tpe, acc.toArraySeq)
            build(exprs).toTerm
        }

      new ValDefinitions[PKG.Const[V], A]([b] =>
        (_, _) ?=>
          (build: Expressions[PKG.Const[V], A] => Expr[b]) =>
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
        f: ChildFunction1[PKG.Const[Expr[V]], PKG.Const[Expr[V]]],
    ): ValDefinitionsWith[PKG.Const[V], A, V] = {
      def rec[Out: Type](
          queue: List[AnyChild],
          prevValue: Expr[V],
          acc: Growable[Expressions.Elem[PKG.Const[V], ?]],
          build: (Expressions[PKG.Const[V], A], Expr[V]) => Expr[Out],
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
            val exprs: Expressions[PKG.Const[V], A] = new Expressions[PKG.Const[V], A](Type.of[PKG.Const[V]], tpe, acc.toArraySeq)
            build(exprs, prevValue).toTerm
        }

      new ValDefinitionsWith[PKG.Const[V], A, V]([b] =>
        (_, _) ?=>
          (build: (Expressions[PKG.Const[V], A], Expr[V]) => Expr[b]) =>
            rec(
              children.toList,
              zero,
              Growable.empty,
              build,
            ).asExprOf[b],
      )
    }

  }

}
object Generic {

  def apply[A: Generic as g]: Generic[A] = g

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Summon Instance
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def of[A](using Type[A], Quotes): ProductOrSumGeneric[A] = Generic.of[A](Derivable.Config())
  def of[A](config: Derivable.Config)(using Type[A], Quotes): ProductOrSumGeneric[A] = {
    val repr: TypeRepr = TypeRepr.of[A]
    val sym: Symbol = repr.typeOrTermSymbol
    sym.typeType.required match
      case _: TypeType.Case   => PKG.ProductGeneric.unsafeOf[A](repr, sym, config)
      case _: TypeType.Sealed => PKG.SumGeneric.unsafeOf[A](repr, sym, config)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Leaf Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait ProductGeneric[A] private[k0] () extends ProductOrSumGeneric[A]
  object ProductGeneric {

    extension [A](self: Generic.ProductGeneric[A])
      def productGenericSelf: PKG.ProductGeneric[A] = self match
        case generic: PKG.ProductGeneric[A] => generic
        case _                              => throw new RuntimeException(s"Internal Defect : Someone besides k0.ProductGeneric has extended k0.Generic.ProductGeneric : ${self.getClass.getName}")

  }

  trait SumGeneric[A] private[k0] () extends ProductOrSumGeneric[A]
  object SumGeneric {

    extension [A](self: Generic.SumGeneric[A])
      def sumGenericSelf: PKG.SumGeneric[A] = self match
        case generic: PKG.SumGeneric[A] => generic
        case _                          => throw new RuntimeException(s"Internal Defect : Someone besides k0.SumGeneric has extended k0.Generic.SumGeneric : ${self.getClass.getName}")

  }

  trait IdentityGeneric[A] private[k0] () extends Generic[A]
  object IdentityGeneric {

    extension [A](self: Generic.IdentityGeneric[A])
      def identityGenericSelf: PKG.IdentityGeneric[A] = self match
        case generic: PKG.IdentityGeneric[A] => generic
        case _                               => throw new RuntimeException(s"Internal Defect : Someone besides k0.IdentityGeneric has extended k0.Generic.IdentityGeneric : ${self.getClass.getName}")

  }

  extension [A](self: Generic[A])
    def genericSelf: PKG.ProductGeneric[A] | PKG.SumGeneric[A] | PKG.IdentityGeneric[A] = self match
      case generic: Generic.ProductGeneric[A]  => generic.productGenericSelf
      case generic: Generic.SumGeneric[A]      => generic.sumGenericSelf
      case generic: Generic.IdentityGeneric[A] => generic.identityGenericSelf

}

sealed trait ProductOrSumGeneric[A] extends Generic[A] {
  val typeType: TypeType
  val derivedFromConfig: Derivable.Config
}
object ProductOrSumGeneric {

  extension [A](self: ProductOrSumGeneric[A])
    def productOrSumGenericSelf: PKG.ProductGeneric[A] | PKG.SumGeneric[A] = self match
      case generic: Generic.ProductGeneric[A] => generic.productGenericSelf
      case generic: Generic.SumGeneric[A]     => generic.sumGenericSelf

}
