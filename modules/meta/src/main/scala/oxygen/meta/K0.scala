package oxygen.meta

import Tuple.++
import oxygen.meta.annotation.*
import oxygen.predef.core.*
import scala.quoted.*
import scala.reflect.ClassTag

final class K0[Q <: Quotes](val meta: Meta[Q]) {
  given Quotes = meta.quotes
  import meta.*

  final class LazyTypeClasses[TC[_]] private[K0] (private[K0] val instances: IArray[Expr[TC[Any]]])

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Generic[A] {

    val label: String

    val typeType: Symbol.TypeType
    val typeRepr: TypeRepr
    val symRepr: Symbol

    final given tpe: Type[A] = typeRepr.asTyped

    final def optionalAnnotation[Annot: Type]: Option[Expr[Annot]] =
      typeRepr.optionalAnnotation[Annot]

    final def requiredAnnotation[Annot: Type]: Expr[Annot] =
      typeRepr.requiredAnnotation[Annot]

    final def optionalAnnotationT[Annot[_]: Type]: Option[Expr[Annot[A]]] =
      typeRepr.optionalAnnotation[Annot[A]]

    final def requiredAnnotationT[Annot[_]: Type]: Expr[Annot[A]] =
      typeRepr.requiredAnnotation[Annot[A]]

    final def optionalAnnotationValue[Annot: {Type, FromExpr}]: Option[Annot] =
      typeRepr.optionalAnnotationValue[Annot]

    final def requiredAnnotationValue[Annot: {Type, FromExpr}]: Annot =
      typeRepr.requiredAnnotationValue[Annot]

  }
  object Generic {

    def of[A](using Type[A]): Generic[A] =
      Generic.attemptOf[A] match {
        case Right(value) => value
        case Left((productError, sumError)) =>
          report.errorAndAbort(
            s"""Unable to derive ProductGeneric[${TypeRepr.of[A].show}]:
               |  $productError
               |
               |Unable to derive SumGeneric[${TypeRepr.of[A].show}]:
               |  $sumError""".stripMargin,
          )
      }

    def attemptOf[A](using Type[A]): Either[(String, String), Generic[A]] =
      (ProductGeneric.attemptOf[A], SumGeneric.attemptOf[A]) match {
        case (Right(product), _)                  => product.asRight
        case (_, Right(sum))                      => sum.asRight
        case (Left(productError), Left(sumError)) => (productError, sumError).asLeft
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ProductGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  @scala.annotation.nowarn("msg=unused import")
  trait ProductGeneric[A] extends Generic[A] {

    // =====| Abstract |=====

    val fields: IArray[Field[?]]

    def fieldsToInstance(exprs: IArray[Expr[?]]): Expr[A]

    override val typeType: Symbol.TypeType.Case

    // =====| Inner |=====

    final case class Field[I](
        idx: Int,
        symRepr: Symbol,
        constructorSymRepr: Symbol,
        typeRepr: TypeRepr,
        tpe: Type[I],
        valDef: Tree.Statement.Definition.ValDef,
        get: Expr[A] => Expr[I],
    ) {

      given Type[I] = tpe

      def name: String = valDef.name

      def typeClassInstance[TC[_]](instances: LazyTypeClasses[TC]): Expr[TC[I]] =
        instances.instances(idx).asInstanceOf[Expr[TC[I]]]

      def optionalAnnotation[Annot: Type]: Option[Expr[Annot]] =
        constructorSymRepr.optionalAnnotation[Annot]

      def requiredAnnotation[Annot: Type]: Expr[Annot] =
        constructorSymRepr.requiredAnnotation[Annot]

      def optionalAnnotationT[Annot[_]: Type]: Option[Expr[Annot[I]]] =
        constructorSymRepr.optionalAnnotation[Annot[I]]

      def requiredAnnotationT[Annot[_]: Type]: Expr[Annot[I]] =
        constructorSymRepr.requiredAnnotation[Annot[I]]

      def optionalAnnotationValue[Annot: {Type, FromExpr}]: Option[Annot] =
        constructorSymRepr.optionalAnnotationValue[Annot]

      def requiredAnnotationValue[Annot: {Type, FromExpr}]: Annot =
        constructorSymRepr.requiredAnnotationValue[Annot]

      def optionalAnnotationTValue[Annot[_]: Type](using FromExpr[Annot[I]]): Option[Annot[I]] =
        constructorSymRepr.optionalAnnotationValue[Annot[I]]

      def requiredAnnotationTValue[Annot[_]: Type](using FromExpr[Annot[I]]): Annot[I] =
        constructorSymRepr.requiredAnnotationValue[Annot[I]]

    }

    // =====| Functions |=====

    private def summonTypeClass[TC[_]: Type]: IArray[Expr[TC[Any]]] =
      fields.map { _field =>
        type _T
        val field: Field[_T] = _field.asInstanceOf[Field[_T]]
        import field.given

        Expr
          .summon[TC[_T]]
          .getOrElse(report.errorAndAbort(s"Unable to find instance `${TypeRepr.of[TC[_T]].show}` for field `${field.name}` in type ${typeRepr.show}"))
          .asInstanceOf[Expr[TC[Any]]]
      }

    object builders {

      /**
        * This will summon instances for all fields.
        * If no instance is found, you will receive a compile error.
        */
      def withLazyTypeClasses[TC[_]: Type, O: Type](
          useTypeClassInstances: LazyTypeClasses[TC] => Expr[O],
      ): Expr[O] = {
        def loop(
            queue: List[(Field[?], Expr[TC[Any]])],
            acc: IArray[Expr[TC[Any]]],
        ): Expr[O] =
          queue match {
            case (_field, i) :: tail =>
              type _T
              val field: Field[_T] = _field.asInstanceOf[Field[_T]]
              import field.given

              '{
                lazy val inst: TC[_T] = ${ i.asExprOf[TC[_T]] }
                ${ loop(tail, acc :+ 'inst.asInstanceOf[Expr[TC[Any]]]) }
              }
            case Nil =>
              useTypeClassInstances(new LazyTypeClasses[TC](acc))
          }

        loop(fields.zip(summonTypeClass[TC]).toList, IArray.empty)
      }

      /**
        * Same as [[instanceFromLazyTypeClasses]], except typed for the most common case of:
        * summon[ TC[_] ] => TC[_]
        */
      def instanceFromLazyTypeClasses[TC[_]: Type](
          useTypeClassInstances: LazyTypeClasses[TC] => Expr[TC[A]],
      ): Expr[TC[A]] =
        withLazyTypeClasses[TC, TC[A]](useTypeClassInstances)

      /**
        * This is useful for when you want to produce an `A` as an output.
        * As long as you can produce an instance of all the fields of `A`, then you can produce an `A`.
        * If you need a fallible `Option[A]` or `Either[E, A]`, see [[optionMapToInstance]] or [[eitherMapToInstance]].
        *
        * Hint: You are almost certainly going to want to call [[ProductGeneric.typeClassInstance]] from within [[makeFieldValue]].
        */
      def mapToInstance(makeFieldValue: [i] => Field[i] => Expr[i]): Expr[A] =
        fieldsToInstance(fields.map(makeFieldValue(_)))

      // TODO (KR) : Create a more generic version of this using Monad.
      /**
        * This is useful for when you want to produce an `A` as an output.
        * As long as you can produce an instance of all the fields of `A`, then you can produce an `A`.
        * This differs from [[mapToInstance]] in that it allows a fallible `Option[A]`.
        *
        * Hint: You are almost certainly going to want to call [[ProductGeneric.typeClassInstance]] from within [[makeFieldValue]].
        */
      def optionMapToInstance(makeFieldValue: [i] => Field[i] => Expr[Option[i]]): Expr[Option[A]] = {
        def rec(
            queue: List[(Field[?], Expr[Option[?]])],
            acc: IArray[Expr[?]],
        ): Expr[Option[A]] =
          queue match {
            case (_field, last) :: Nil =>
              type _T
              val field: Field[_T] = _field.asInstanceOf[Field[_T]]
              import field.given

              val _last: Expr[Option[_T]] = last.asExprOf[Option[_T]]

              '{
                val res: Option[_T] = $_last
                res.map { v => ${ fieldsToInstance(acc :+ 'v) } }
              }
            case (_field, head) :: tail =>
              type _T
              val field: Field[_T] = _field.asInstanceOf[Field[_T]]
              import field.given

              val _head: Expr[Option[_T]] = head.asExprOf[Option[_T]]

              '{
                val res: Option[_T] = $_head
                res.flatMap { v => ${ rec(tail, acc :+ 'v) } }
              }
            case Nil =>
              '{ Some(${ fieldsToInstance(acc) }) }
          }

        rec(
          fields.map { _field =>
            type _T
            val field: Field[_T] = _field.asInstanceOf[Field[_T]]
            import field.given

            (field, makeFieldValue(field))
          }.toList,
          IArray.empty,
        )
      }

      // TODO (KR) : Create a more generic version of this using Monad.
      /**
        * This is useful for when you want to produce an `A` as an output.
        * As long as you can produce an instance of all the fields of `A`, then you can produce an `A`.
        * This differs from [[mapToInstance]] in that it allows a fallible `Either[E, A]`.
        *
        * Hint: You are almost certainly going to want to call [[ProductGeneric.typeClassInstance]] from within [[makeFieldValue]].
        */
      def eitherMapToInstance[E: Type](makeFieldValue: [i] => Field[i] => Expr[Either[E, i]]): Expr[Either[E, A]] = {
        def rec(
            queue: List[(Field[?], Expr[Either[E, ?]])],
            acc: IArray[Expr[?]],
        ): Expr[Either[E, A]] =
          queue match {
            case (_field, last) :: Nil =>
              type _T
              val field: Field[_T] = _field.asInstanceOf[Field[_T]]
              import field.given

              val _last: Expr[Either[E, _T]] = last.asExprOf[Either[E, _T]]

              '{
                val res: Either[E, _T] = $_last
                res.map { v => ${ fieldsToInstance(acc :+ 'v) } }
              }
            case (_field, head) :: tail =>
              type _T
              val field: Field[_T] = _field.asInstanceOf[Field[_T]]
              import field.given

              val _head: Expr[Either[E, _T]] = head.asExprOf[Either[E, _T]]

              '{
                val res: Either[E, _T] = $_head
                res.flatMap { v => ${ rec(tail, acc :+ 'v) } }
              }
            case Nil =>
              '{ Right(${ fieldsToInstance(acc) }) }
          }

        rec(
          fields.map { _field =>
            type _T
            val field: Field[_T] = _field.asInstanceOf[Field[_T]]
            import field.given

            (field, makeFieldValue(field))
          }.toList,
          IArray.empty,
        )
      }

      /**
        * This is useful when you want to get some `Seq` of "something else" out of your `A`.
        *
        * Hint: You are almost certainly going to want to call [[ProductGeneric.typeClassInstance]] from within [[mapField]].
        */
      def mapToSeq[B](mapField: [i] => Field[i] => B): Seq[B] =
        fields.toSeq.map(mapField(_))

    }

  }
  object ProductGeneric {

    def of[A](using Type[A]): ProductGeneric[A] =
      ProductGeneric.attemptOf[A].getOrAbort(s"Unable to derive ProductGeneric[${TypeRepr.of[A].show}]:\n  ")

    private def attemptOfCaseObject[A](_typeRepr: TypeRepr, typeTypeCase: Symbol.TypeType.Case)(using Type[A]): Either[String, ProductGeneric[A]] = {
      val _symRepr: Symbol = _typeRepr.termSymbol
      val g: ProductGeneric[A] =
        new ProductGeneric[A] {

          override val label: String = _symRepr.name

          override val typeRepr: TypeRepr = _typeRepr

          override val symRepr: Symbol = _symRepr

          override val typeType: Symbol.TypeType.Case = typeTypeCase

          override val fields: IArray[Field[?]] = IArray.empty

          override def fieldsToInstance(exprs: IArray[Expr[?]]): Expr[A] = _symRepr.toTerm.asExprOf

        }

      g.asRight
    }

    private def attemptOfCaseClass[A](_typeRepr: TypeRepr, typeTypeCase: Symbol.TypeType.Case)(using Type[A]): Either[String, ProductGeneric[A]] =
      for {
        _symRepr: Symbol <- _typeRepr.typeSymbol.asRight
        primaryConstructor: Symbol = _symRepr.primaryConstructor

        (typeArgSymbols0: List[Symbol], typeArgReprs: List[TypeRepr]) <-
          (_typeRepr, primaryConstructor.paramSymss) match {
            case (_, _ :: Nil)                                                                => (Nil, Nil).asRight
            case (at: TypeRepr.AppliedType, tsyms :: _ :: Nil) if tsyms.forall(_.isTypeParam) => (tsyms, at.args).asRight
            case (_, symss) =>
              s"""has non-single constructor arg groups.
                 |        allowed: MyCaseClass(...)
                 |    not allowed: MyCaseClass(...)(...)
                 |    not allowed: MyCaseClass(...)(...)(...)
                 |
                 |    params(${symss.size}):${symss.map { s => s"\n        - ${s.mkString(", ")}" }.mkString}""".stripMargin.asLeft
          }
        typeArgSymbols: List[Symbol] = typeArgSymbols0.map { s => _symRepr.typeMember(s.name) }

        caseFieldSymbols: List[Symbol] = _symRepr.caseFields
        caseFieldValDefs: List[Tree.Statement.Definition.ValDef] <- // NOTE : This might need to be relaxed to a `collect` if unforeseen cases arise
          caseFieldSymbols.traverse { sym =>
            sym.tree match {
              case valDef: Tree.Statement.Definition.ValDef => valDef.asRight
              case _                                        => s"Somehow not a `ValDef`: ${sym.fullName}".asLeft
            }
          }

        constructorSymMap = primaryConstructor.paramSymss.flatten.filter(_.isTerm).map { s => s.name -> s }.toMap

      } yield new ProductGeneric[A] { self =>
        override val label: String = _symRepr.name

        override val typeRepr: TypeRepr = _typeRepr

        override val symRepr: Symbol = _symRepr

        override val typeType: Symbol.TypeType.Case = typeTypeCase

        override val fields: IArray[Field[?]] =
          IArray.from {
            caseFieldSymbols.zip(caseFieldValDefs).zipWithIndex.map { case ((sym, valDef), idx) =>
              type _T
              val typeRepr: TypeRepr = valDef.tpt.tpe.substituteTypes(typeArgSymbols, typeArgReprs)
              given tpe: Type[_T] = typeRepr.asTyped[_T]

              self.Field[_T](
                idx = idx,
                symRepr = sym,
                constructorSymRepr = constructorSymMap(sym.name),
                typeRepr = typeRepr,
                tpe = tpe,
                valDef = valDef,
                get = _.toTerm.select(sym).asExprOf[_T],
              )
            }
          }

        override def fieldsToInstance(exprs: IArray[Expr[?]]): Expr[A] =
          if (typeArgSymbols.isEmpty)
            Tree.Statement.Term
              .New(Tree.TypeTree.ref(symRepr))
              .select(primaryConstructor)
              .appliedToArgs(exprs.map(_.toTerm).toList)
              .asExprTyped
          else
            Tree.Statement.Term
              .New(Tree.TypeTree.Applied(Tree.TypeTree.ref(symRepr), typeArgReprs.map(Tree.TypeTree.Inferred(_))))
              .select(primaryConstructor)
              .appliedToTypes(typeArgReprs)
              .appliedToArgs(exprs.map(_.toTerm).toList)
              .asExprTyped

      }

    def attemptOf[A](using Type[A]): Either[String, ProductGeneric[A]] = {
      val _typeRepr: TypeRepr = TypeRepr.of[A]
      _typeRepr.typeTypeCase.toRight(s"not a `case object` or `case class`\n  [${_typeRepr.typeOrTermSymbol.flags.show}]").flatMap { typeTypeCase =>
        if (typeTypeCase.isObject) attemptOfCaseObject[A](_typeRepr, typeTypeCase)
        else attemptOfCaseClass[A](_typeRepr, typeTypeCase)
      }
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      SumGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  @scala.annotation.nowarn("msg=unused import")
  trait SumGeneric[A] extends Generic[A] {

    // =====| Abstract |=====

    override val typeType: Symbol.TypeType.Sealed

    val cases: IArray[Case[? <: A]]

    // =====| Inner |=====

    final case class Case[I <: A](
        idx: Int,
        productGeneric: ProductGeneric[I],
    ) {

      given tpe: Type[I] = productGeneric.tpe

      def name: String = productGeneric.label

      def typeClassInstance[TC[_]](instances: LazyTypeClasses[TC]): Expr[TC[I]] =
        instances.instances(idx).asInstanceOf[Expr[TC[I]]]

      def optionalAnnotation[Annot: Type]: Option[Expr[Annot]] =
        productGeneric.optionalAnnotation[Annot]

      def requiredAnnotation[Annot: Type]: Expr[Annot] =
        productGeneric.requiredAnnotation[Annot]

      def optionalAnnotationT[Annot[_]: Type]: Option[Expr[Annot[I]]] =
        productGeneric.optionalAnnotation[Annot[I]]

      def requiredAnnotationT[Annot[_]: Type]: Expr[Annot[I]] =
        productGeneric.requiredAnnotation[Annot[I]]

      def optionalAnnotationValue[Annot: {Type, FromExpr}]: Option[Annot] =
        productGeneric.optionalAnnotationValue[Annot]

      def requiredAnnotationValue[Annot: {Type, FromExpr}]: Annot =
        productGeneric.requiredAnnotationValue[Annot]

      def optionalAnnotationTValue[Annot[_]: Type](using FromExpr[Annot[I]]): Option[Annot[I]] =
        productGeneric.optionalAnnotationValue[Annot[I]]

      def requiredAnnotationTValue[Annot[_]: Type](using FromExpr[Annot[I]]): Annot[I] =
        productGeneric.requiredAnnotationValue[Annot[I]]

    }

    trait MatchBuilder[CaseMatch[_ <: A] <: Tuple, RhsParams[_ <: A] <: Tuple] {

      final def ++[CaseMatch2[_ <: A] <: Tuple, RhsParams2[_ <: A] <: Tuple](that: MatchBuilder[CaseMatch2, RhsParams2]): MatchBuilder.Tupled[CaseMatch, RhsParams, CaseMatch2, RhsParams2] =
        new MatchBuilder.Tupled(this, that)

      // TODO (KR) : improve this interface
      private def buildGeneric[B: Type](
          makeCaseMatch: [i <: A] => Case[i] => CaseMatch[i],
      )(
          makeRhs: [i <: A] => (Case[i], RhsParams[i]) => Expr[B],
      )(
          default: Option[Expr[B]],
      ): Expr[B] = {
        val params: IArray[MatchBuilder.Generated[RhsParams, MatchBuilder.AnyA]] = makeParams(1)(makeCaseMatch)

        val defaultCaseDef: Option[Tree.CaseDef] =
          default.map { default =>
            Tree.CaseDef(
              Tree.Statement.Term.Ref.Ident.Wildcard(),
              None,
              default.toTerm,
            )
          }

        lhsInputTrees.toList match {
          case lhs :: Nil =>
            val inputExpr: Tree.Statement.Term =
              lhs.tree

            val caseDefs: IArray[Tree.CaseDef] =
              cases.zip(params).map { case (_kase, _params) =>
                type _T <: A
                val kase: Case[_T] = _kase.asInstanceOf[Case[_T]]
                val params: MatchBuilder.Generated[RhsParams, _T] = _params.asInstanceOf[MatchBuilder.Generated[RhsParams, _T]]
                import kase.given

                Tree.CaseDef(
                  params.lhsTrees.head.tree,
                  None,
                  makeRhs(kase, params.rhsParams).toTerm,
                )
              }

            Tree.Statement.Term
              .Match(
                inputExpr,
                caseDefs.toList ++ defaultCaseDef.toList,
              )
              .asExprOf[B]
          case lhss =>
            val tupleSym = defn.TupleClass(lhss.size).companionModule
            val applySym = tupleSym.declaredMethod("apply").head
            val unapplySym = tupleSym.declaredMethod("unapply").head

            val inputExpr: Tree.Statement.Term =
              tupleSym.toTerm
                .select(applySym)
                .appliedToTypes(lhss.map(_.tpe.typeRepr))
                .appliedToArgs(lhss.map(_.tree))

            val caseDefs: IArray[Tree.CaseDef] =
              cases.zip(params).map { case (_kase, _params) =>
                type _T <: A
                val kase: Case[_T] = _kase.asInstanceOf[Case[_T]]
                val params: MatchBuilder.Generated[RhsParams, _T] = _params.asInstanceOf[MatchBuilder.Generated[RhsParams, _T]]
                import kase.given

                val unapplyTree: Tree.Unapply =
                  Tree.Unapply(
                    tupleSym.toTerm
                      .select(unapplySym)
                      .appliedToTypes(params.lhsTrees.toList.map(_.tpe.typeRepr)),
                    Nil,
                    params.lhsTrees.toList.map(_.tree),
                  )

                Tree.CaseDef(
                  unapplyTree,
                  None,
                  makeRhs(kase, params.rhsParams).toTerm,
                )
              }

            Tree.Statement.Term
              .Match(
                inputExpr,
                caseDefs.toList ++ defaultCaseDef.toList,
              )
              .asExprOf[B]
        }

      }

      // TODO (KR) : improve this interface
      final def build[B: Type](
          makeCaseMatch: [i <: A] => Case[i] => CaseMatch[i],
      )(
          makeRhs: [i <: A] => (Case[i], RhsParams[i]) => Expr[B],
      )(
          default: Expr[B],
      ): Expr[B] =
        buildGeneric[B](makeCaseMatch)(makeRhs)(default.some)

      // TODO (KR) : improve this interface
      final def buildNoDefault[B: Type](
          makeCaseMatch: [i <: A] => Case[i] => CaseMatch[i],
      )(
          makeRhs: [i <: A] => (Case[i], RhsParams[i]) => Expr[B],
      ): Expr[B] =
        buildGeneric[B](makeCaseMatch)(makeRhs)(None)

      private[MatchBuilder] val lhsInputTrees: NonEmptyList[MatchBuilder.Generated.LhsTree[Tree.Statement.Term, ?]]

      private[MatchBuilder] def makeParams(offset: Int)(makeCaseMatch: [i <: A] => Case[i] => CaseMatch[i]): IArray[MatchBuilder.Generated[RhsParams, MatchBuilder.AnyA]]

    }
    object MatchBuilder {

      private type AnyA <: A

      private final case class Generated[RhsParams[_ <: A] <: Tuple, I <: A](
          lhsTrees: NonEmptyList[Generated.LhsTree[Tree, ?]],
          rhsParams: RhsParams[I],
      ) {

        def ++[RhsParams2[_ <: A] <: Tuple](that: Generated[RhsParams2, I]): Generated[[i <: A] =>> RhsParams[i] ++ RhsParams2[i], I] =
          Generated(this.lhsTrees ++ that.lhsTrees, this.rhsParams ++ that.rhsParams)

      }
      private object Generated {

        final case class LhsTree[TreeT <: Tree, T](
            tree: TreeT,
            tpe: Type[T],
        )

      }

      final class Tupled[
          CaseMatch1[_ <: A] <: Tuple,
          RhsParams1[_ <: A] <: Tuple,
          CaseMatch2[_ <: A] <: Tuple,
          RhsParams2[_ <: A] <: Tuple,
      ] private[MatchBuilder] (
          _1: MatchBuilder[CaseMatch1, RhsParams1],
          _2: MatchBuilder[CaseMatch2, RhsParams2],
      ) extends MatchBuilder[
            [i <: A] =>> CaseMatch1[i] ++ CaseMatch2[i],
            [i <: A] =>> RhsParams1[i] ++ RhsParams2[i],
          ] {

        override private[MatchBuilder] val lhsInputTrees: NonEmptyList[MatchBuilder.Generated.LhsTree[Tree.Statement.Term, ?]] =
          _1.lhsInputTrees ++ _2.lhsInputTrees

        override private[MatchBuilder] def makeParams(offset: Int)(
            makeCaseMatch: [i <: A] => Case[i] => CaseMatch1[i] ++ CaseMatch2[i],
        ): IArray[MatchBuilder.Generated[[i <: A] =>> RhsParams1[i] ++ RhsParams2[i], MatchBuilder.AnyA]] = {
          val additionalOffset: Int = _1.lhsInputTrees.length

          val params1: IArray[MatchBuilder.Generated[RhsParams1, MatchBuilder.AnyA]] =
            _1.makeParams(offset) { [i <: A] => (kase: Case[i]) => makeCaseMatch(kase).take(additionalOffset).asInstanceOf[CaseMatch1[i]] }
          val params2: IArray[MatchBuilder.Generated[RhsParams2, MatchBuilder.AnyA]] =
            _2.makeParams(offset + additionalOffset) { [i <: A] => (kase: Case[i]) => makeCaseMatch(kase).drop(additionalOffset).asInstanceOf[CaseMatch2[i]] }

          params1.zip(params2).map { _ ++ _ }
        }

      }

      final case class Instance private[MatchBuilder] (a: Expr[A]) extends MatchBuilder[[_ <: A] =>> EmptyTuple, [i <: A] =>> Tuple1[Expr[i]]] {

        override private[MatchBuilder] val lhsInputTrees: NonEmptyList[MatchBuilder.Generated.LhsTree[Tree.Statement.Term, ?]] =
          NonEmptyList.one(MatchBuilder.Generated.LhsTree(a.toTerm, tpe))

        override private[MatchBuilder] def makeParams(offset: Int)(
            makeCaseMatch: [i <: A] => Case[i] => EmptyTuple,
        ): IArray[MatchBuilder.Generated[[i <: A] =>> Tuple1[Expr[i]], AnyA]] =
          cases.map { _kase =>
            type _T <: A
            val kase: Case[_T] = _kase.asInstanceOf[Case[_T]]
            import kase.given

            val (tree, rhs) =
              if (kase.productGeneric.typeType.isObject) {
                val term = kase.productGeneric.symRepr.toTerm
                (term, term.asExprOf[_T])
              } else {
                val bindSymbol: Symbol = Symbol.newBind(Symbol.spliceOwner, s"value_$offset", Flags.EmptyFlags, kase.productGeneric.typeRepr)
                val bind: Tree.Bind =
                  Tree.Bind(
                    bindSymbol,
                    Tree.Statement.Term.Typed(
                      Tree.Statement.Term.Ref.Ident.Wildcard(),
                      kase.productGeneric.typeRepr.typeTree,
                    ),
                  )
                val term: Tree.Statement.Term = Tree.Statement.Term.Ref(bindSymbol)

                (
                  bind,
                  term.asExprOf[_T],
                )
              }

            MatchBuilder.Generated[[i <: A] =>> Tuple1[Expr[i]], AnyA](
              lhsTrees = NonEmptyList.one(MatchBuilder.Generated.LhsTree(tree, kase.tpe)),
              rhsParams = Tuple1(rhs.asInstanceOf[Expr[AnyA]]),
            )
          }

      }

      final case class Value[B: {Type as bTpe}] private[MatchBuilder] (v: Expr[B]) extends MatchBuilder[[_ <: A] =>> Tuple1[Expr[B]], [_ <: A] =>> EmptyTuple] {

        override private[MatchBuilder] val lhsInputTrees: NonEmptyList[MatchBuilder.Generated.LhsTree[Tree.Statement.Term, ?]] =
          NonEmptyList.one(MatchBuilder.Generated.LhsTree(v.toTerm, bTpe))

        override private[MatchBuilder] def makeParams(offset: Int)(
            makeCaseMatch: [i <: A] => Case[i] => Tuple1[Expr[B]],
        ): IArray[MatchBuilder.Generated[[_ <: A] =>> EmptyTuple, AnyA]] =
          cases.map { _kase =>
            type _T <: A
            val kase: Case[_T] = _kase.asInstanceOf[Case[_T]]
            import kase.given

            val bExpr = makeCaseMatch(kase)._1

            MatchBuilder.Generated[[_ <: A] =>> EmptyTuple, AnyA](
              lhsTrees = NonEmptyList.one(MatchBuilder.Generated.LhsTree(bExpr.toTerm, bTpe)),
              rhsParams = EmptyTuple,
            )
          }

      }

      def instance(a: Expr[A]): MatchBuilder.Instance =
        MatchBuilder.Instance(a)

      def value[B: Type](v: Expr[B]): MatchBuilder.Value[B] =
        MatchBuilder.Value(v)

    }

    // =====| Functions |=====

    private def summonTypeClass[TC[_]: Type](
        autoDeriveChildren: Option[[i <: A] => ProductGeneric[i] => Expr[TC[i]]],
    ): IArray[Expr[TC[Any]]] =
      cases.map { _kase =>
        type _T <: A
        val kase: Case[_T] = _kase.asInstanceOf[Case[_T]]
        import kase.given

        val inst: Expr[TC[_T]] =
          (Expr.summon[TC[_T]], autoDeriveChildren) match
            case (Some(expr), _)                  => expr
            case (None, Some(autoDeriveChildren)) => autoDeriveChildren(kase.productGeneric)
            case (None, None)                     => report.errorAndAbort(s"Unable to summon child instance `${TypeRepr.of[TC[_T]].show}`")

        inst.asInstanceOf[Expr[TC[Any]]]
      }

    object builders {

      /**
        * This will summon instances for all case children.
        * If no instance is found, it will auto-derive the child instance in place.
        *
        * You should call `withLazyTypeClasses[TC]` when you are trying to derive a `TC[A]`.
        * You should not call `withLazyTypeClasses[OtherTC]` when you are trying to derive a `TC[A]`.
        */
      def withLazyTypeClasses[TC[_]: Type, O: Type](
          autoDeriveChildren: [i <: A] => ProductGeneric[i] => Expr[TC[i]],
      )(
          useTypeClassInstances: LazyTypeClasses[TC] => Expr[O],
      ): Expr[O] = {
        def loop(
            queue: List[(Case[? <: A], Expr[TC[Any]])],
            acc: IArray[Expr[TC[Any]]],
        ): Expr[O] =
          queue match {
            case (_kase, i) :: tail =>
              type _T <: A
              val kase: Case[_T] = _kase.asInstanceOf[Case[_T]]
              import kase.given

              '{
                lazy val inst: TC[_T] = ${ i.asExprOf[TC[_T]] }
                ${ loop(tail, acc :+ 'inst.asInstanceOf[Expr[TC[Any]]]) }
              }
            case Nil =>
              useTypeClassInstances(new LazyTypeClasses[TC](acc))
          }

        loop(cases.zip(summonTypeClass[TC](autoDeriveChildren.some)).toList, IArray.empty)
      }

      /**
        * Same as [[instanceFromLazyTypeClasses]], except typed for the most common case of:
        * summon[ TC[_] ] => TC[_]
        */
      def instanceFromLazyTypeClasses[TC[_]: Type](
          autoDeriveChildren: [i <: A] => ProductGeneric[i] => Expr[TC[i]],
      )(
          useTypeClassInstances: LazyTypeClasses[TC] => Expr[TC[A]],
      ): Expr[TC[A]] =
        withLazyTypeClasses[TC, TC[A]](autoDeriveChildren)(useTypeClassInstances)

      /**
        * Similar to [[withLazyTypeClasses]], except if no instance is found, a compile error will be created instead of auto-deriving.
        *
        * You should not call `withLazyTypeClassesNoAutoDerive[TC]` when you are trying to derive a `TC[A]`.
        * You should call `withLazyTypeClassesNoAutoDerive[OtherTC]` when you are trying to derive a `TC[A]`.
        */
      def withLazyTypeClassesNoAutoDerive[TC[_]: Type, O: Type](
          useTypeClassInstances: LazyTypeClasses[TC] => Expr[O],
      ): Expr[O] = {
        def loop(
            queue: List[(Case[? <: A], Expr[TC[Any]])],
            acc: IArray[Expr[TC[Any]]],
        ): Expr[O] =
          queue match {
            case (_kase, i) :: tail =>
              type _T <: A
              val kase: Case[_T] = _kase.asInstanceOf[Case[_T]]
              import kase.given

              '{
                lazy val inst: TC[_T] = ${ i.asExprOf[TC[_T]] }
                ${ loop(tail, acc :+ 'inst.asInstanceOf[Expr[TC[Any]]]) }
              }
            case Nil =>
              useTypeClassInstances(new LazyTypeClasses[TC](acc))
          }

        loop(cases.zip(summonTypeClass[TC](None)).toList, IArray.empty)
      }

      /**
        * This is useful when you have an instance of `A`, and want to do things differently depending on which sub-type of `A` you have.
        * A common example would be the canonical scala JsonEncoder.
        * We are able to auto-derive all the child case JsonEncoders for `myEnum: MyEnum`, but how do we know which one to call?
        * This would then allow you to wrap a child `JsonAST` in `{ "MySubType": ... }`.
        * This function gives you an exhaustive match on `myEnum: MyEnum`, the ability to use a child `TC[_]` instance, and then modify that [[Expr]].
        *
        * Hint: You are almost certainly going to want to call [[SumGeneric.typeClassInstance]] from within [[useCase]].
        *
        * If you need to match on more than 1 thing, you probably want [[MatchBuilder.instance]] and/or [[MatchBuilder.value]].
        */
      def matchOnInstance[B: Type](instance: Expr[A])(
          useCase: [i <: A] => (Case[i], Expr[i]) => Expr[B],
      ): Expr[B] =
        MatchBuilder
          .instance(instance)
          .buildNoDefault[B] {
            [i <: A] => (_: Case[i]) => EmptyTuple
          } {
            [i <: A] => (kase: Case[i], expr: Tuple1[Expr[i]]) => useCase(kase, expr._1)
          }

      /**
        * This is useful when you have some generic thing, and can create a representation of it from a Case.
        * A common example would be the canonical scala JsonDecoder.
        * Example json: `{ "MySubType": ... }`
        * This function would allow you to match on a `String`,
        * and do `Expr(kase.name)` in order to then use the child case decoder for `MySubType`.
        *
        * If you need to match on more than 1 thing, you probably want [[MatchBuilder.instance]] and/or [[MatchBuilder.value]].
        *
        * Hint: You are almost certainly going to want to call [[SumGeneric.typeClassInstance]] from within [[useCase]].
        *
        * Warning:
        * This is currently only tested to work where `useCase`.Expr[B] returns a constant.
        * Ex: (??? : String) match { case "a" => ...; case "b" => ... }
        */
      def matchOnInput[B: Type, C: Type](
          input: Expr[B],
      )(
          useCase: [i <: A] => Case[i] => (Expr[B], Expr[C]),
      )(
          default: Expr[C],
      ): Expr[C] =
        MatchBuilder
          .value(input)
          .build[C] {
            [i <: A] => (kase: Case[i]) => Tuple1(useCase(kase)._1)
          } {
            [i <: A] => (kase: Case[i], _: EmptyTuple) => useCase(kase)._2
          }(default)

    }

  }
  object SumGeneric {

    def of[A](using Type[A]): SumGeneric[A] =
      SumGeneric.attemptOf[A].getOrAbort(s"Unable to derive SumGeneric[${TypeRepr.of[A].show}]:\n  ")

    def attemptOf[A](using Type[A]): Either[String, SumGeneric[A]] =
      for {
        _ <- ().asRight

        _typeRepr: TypeRepr = TypeRepr.of[A]
        _typeSymbol: Symbol = _typeRepr.typeSymbol

        sealedTypeType: Symbol.TypeType.Sealed <-
          _typeSymbol.typeTypeSealed.toRight(s"not a `sealed trait`, `sealed abstract class`, or `enum`\n  [${_typeSymbol.flags.show}]")

        parentTParams = _typeRepr match {
          case applied: TypeRepr.AppliedType => applied.args.toNonEmpty
          case _                             => None
        }

        childTypeSymbols: NonEmptyList[Symbol] <- {
          def rec(typeSym: Symbol): Either[String, NonEmptyList[Symbol]] =
            typeSym.typeType match {
              case Some(_: Symbol.TypeType.Sealed) =>
                NonEmptyList
                  .fromList(typeSym.children)
                  .toRight(s"no children: $typeSym")
                  .flatMap(_.traverse(rec))
                  .map(_.flatten)
              case Some(_: Symbol.TypeType.Case) =>
                NonEmptyList.one(typeSym).asRight
              case None =>
                s"child is not a `case class`, `case object`, `sealed trait`, `sealed abstract class`, or `enum`: $typeSym\n  [${typeSym.flags.show}]".asLeft
            }

          rec(_typeSymbol).map(_.distinct)
        }

        _childGenerics: NonEmptyList[ProductGeneric[? <: A]] <- childTypeSymbols.traverse { sym =>
          val childTypeType: Symbol.TypeType.Case = sym.typeTypeCase.get
          def extensionArgs: Either[String, (List[String], List[TypeRepr])] =
            sym.tree match {
              case classDef: Tree.Statement.Definition.ClassDef =>
                classDef.constructor.symbol.paramSymss match {
                  case tParams :: _ :: Nil =>
                    // TODO (KR) : support extension chains
                    //           : sealed trait Outer[+A]
                    //           : sealed trait Inner[+A] extends Outer[A]
                    //           : final case class Innest[+A](a: A) extends Inner[A]

                    classDef.parents
                      .flatMap {
                        case tt: Tree.TypeTree =>
                          tt.tpe match {
                            case at: TypeRepr.AppliedType if at.tycon.typeSymbol == _typeSymbol => at.args.some
                            case _                                                              => None
                          }
                        case _: Tree.Statement.Term =>
                          None
                      }
                      .headOption
                      .map((tParams.map(_.name), _))
                      .toRight(s"unable to find extension relationship for $sym -> ${_typeSymbol}")
                  case _ =>
                    s"unable to get type params for child $sym".asLeft
                }
              case _ =>
                s"not a class def? $sym".asLeft
            }

          type _T <: A

          val tType: Either[String, Type[_T]] =
            (parentTParams, childTypeType.isObject) match {
              case (Some(parentTParams), false) =>
                val nothingRepr = TypeRepr.of[Nothing]
                for {
                  (symNames, extArgs) <- extensionArgs
                  tupList <- parentTParams.toList.zip(extArgs).traverse {
                    case (_, extArg) if extArg.show == nothingRepr.show => None.asRight
                    case (parentTParam, extArg: TypeRepr.NamedType.TypeRef) =>
                      extArg.qualifier match {
                        case _: TypeRepr.ThisType =>
                          (extArg.name, parentTParam).some.asRight
                        case _ =>
                          report.errorAndAbort(extArg.raw.toString)
                      }
                    case (parentTParam, extArg) =>
                      s"Non-decipherable arg extension relationship: `${sym.fullName}` ${extArg.show} -> ${parentTParam.show}".asLeft
                  }
                  tupMap <-
                    tupList.flatten
                      .groupMap(_._1)(_._2)
                      .toList
                      .traverse {
                        case (k, v :: Nil) => (k, v).asRight
                        // TODO (KR) : might be possible to use an intersection type here?
                        case (k, vs) => s"param ${sym.fullName}.$k maps to multiple parents: ${vs.map(_.show).mkString(", ")}".asLeft
                      }
                      .map(_.toMap)

                  orderedArgs <-
                    symNames.traverse { n =>
                      tupMap.get(n).toRight(s"No such arg? ${sym.fullName}.$n")
                    }

                } yield sym.typeRef.appliedTo(orderedArgs).asTyped[_T]
              case (None, false) =>
                sym.typeRef.asTyped[_T].asRight
              case (_, true) =>
                if (childTypeType.isScala2) sym.companionModule.termRef.asTyped[_T].asRight
                else sym.termRef.asTyped[_T].asRight
            }

          tType.flatMap { tTpe =>
            @scala.annotation.unused
            given Type[_T] = tTpe

            ProductGeneric.attemptOf[_T].leftMap(e => s"error deriving product generic for child `$sym`: $e")
          }
        }
      } yield new SumGeneric[A] { self =>
        override val label: String = _typeSymbol.name

        override val typeRepr: TypeRepr = _typeRepr

        override val symRepr: Symbol = _typeSymbol

        override val typeType: Symbol.TypeType.Sealed = sealedTypeType

        override val cases: IArray[Case[? <: A]] =
          IArray.from {
            _childGenerics.toList.zipWithIndex.map { case (_pg, idx) =>
              type _T <: A
              val pg: ProductGeneric[_T] = _pg.asInstanceOf[ProductGeneric[_T]]

              self.Case[_T](
                idx = idx,
                productGeneric = pg,
              )
            }
          }

      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      UnionGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  @scala.annotation.nowarn("msg=unused import")
  trait UnionGeneric[A] {

    final case class Case[I <: A](
        idx: Int,
        typeRepr: TypeRepr,
    ) {

      given tpe: Type[I] = typeRepr.asTyped

      def typeClassInstance[TC[_]](instances: LazyTypeClasses[TC]): Expr[TC[I]] =
        instances.instances(idx).asInstanceOf[Expr[TC[I]]]

    }

    val typeRepr: TypeRepr

    val cases: IArray[Case[? <: A]]

    final given tpe: Type[A] = typeRepr.asTyped

    private def summonTypeClass[TC[_]: Type]: IArray[Expr[TC[Any]]] =
      cases.map { _case =>
        type _T <: A
        val kase: Case[_T] = _case.asInstanceOf[Case[_T]]
        import kase.given

        Expr
          .summon[TC[_T]]
          .getOrElse(report.errorAndAbort(s"Unable to find instance `${TypeRepr.of[TC[_T]].show}` for union case `${kase.typeRepr.show}` in type ${typeRepr.show}"))
          .asInstanceOf[Expr[TC[Any]]]
      }

    object builders {

      def withLazyTypeClasses[TC[_]: Type, O: Type](
          useTypeClassInstances: LazyTypeClasses[TC] => Expr[O],
      ): Expr[O] = {
        def loop(
            queue: List[(Case[?], Expr[TC[Any]])],
            acc: IArray[Expr[TC[Any]]],
        ): Expr[O] =
          queue match {
            case (_case, i) :: tail =>
              type _T <: A
              val kase: Case[_T] = _case.asInstanceOf[Case[_T]]
              import kase.given

              '{
                lazy val inst: TC[_T] = ${ i.asExprOf[TC[_T]] }
                ${ loop(tail, acc :+ 'inst.asInstanceOf[Expr[TC[Any]]]) }
              }
            case Nil =>
              useTypeClassInstances(new LazyTypeClasses[TC](acc))
          }

        loop(cases.zip(summonTypeClass[TC]).toList, IArray.empty)
      }

      def instanceFromLazyTypeClasses[TC[_]: Type](
          useTypeClassInstances: LazyTypeClasses[TC] => Expr[TC[A]],
      ): Expr[TC[A]] =
        withLazyTypeClasses[TC, TC[A]](useTypeClassInstances)

    }

  }
  object UnionGeneric {

    def of[A](using Type[A]): UnionGeneric[A] =
      UnionGeneric.attemptOf[A].getOrAbort(s"Unable to derive UnionGeneric[${TypeRepr.of[A].show}]:\n  ")

    def attemptOf[A](using Type[A]): Either[String, UnionGeneric[A]] = {
      def expandTypes(repr: TypeRepr): NonEmptyList[TypeRepr] = repr.dealias match
        case TypeRepr.AndOrType.OrType(a, b) => expandTypes(a) ::: expandTypes(b)
        case _                               => NonEmptyList.one(repr)

      val _typeRepr: TypeRepr = TypeRepr.of[A]
      val expanded: NonEmptyList[TypeRepr] = expandTypes(_typeRepr)

      Either.cond(expanded.length >= 2, (), s"Type ${_typeRepr.show} is not an union type").map { _ =>
        new UnionGeneric[A] {
          override val typeRepr: TypeRepr = _typeRepr
          override val cases: IArray[Case[? <: A]] =
            IArray.from {
              expanded.toList.zipWithIndex.map { case (repr, i) =>
                type _T <: A
                Case[_T](i, repr)
              }
            }
        }
      }
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      IntersectionGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  @scala.annotation.nowarn("msg=unused import")
  trait IntersectionGeneric[A] {

    final case class Case[I >: A](
        idx: Int,
        typeRepr: TypeRepr,
    ) {

      given tpe: Type[I] = typeRepr.asTyped

      def typeClassInstance[TC[_]](instances: LazyTypeClasses[TC]): Expr[TC[I]] =
        instances.instances(idx).asInstanceOf[Expr[TC[I]]]

    }

    val typeRepr: TypeRepr

    val cases: IArray[Case[? >: A]]

    final given tpe: Type[A] = typeRepr.asTyped

    private def summonTypeClass[TC[_]: Type]: IArray[Expr[TC[Any]]] =
      cases.map { _case =>
        type _T >: A
        val kase: Case[_T] = _case.asInstanceOf[Case[_T]]
        import kase.given

        Expr
          .summon[TC[_T]]
          .getOrElse(report.errorAndAbort(s"Unable to find instance `${TypeRepr.of[TC[_T]].show}` for intersection case `${kase.typeRepr.show}` in type ${typeRepr.show}"))
          .asInstanceOf[Expr[TC[Any]]]
      }

    object builders {

      def withLazyTypeClasses[TC[_]: Type, O: Type](
          useTypeClassInstances: LazyTypeClasses[TC] => Expr[O],
      ): Expr[O] = {
        def loop(
            queue: List[(Case[?], Expr[TC[Any]])],
            acc: IArray[Expr[TC[Any]]],
        ): Expr[O] =
          queue match {
            case (_case, i) :: tail =>
              type _T >: A
              val kase: Case[_T] = _case.asInstanceOf[Case[_T]]
              import kase.given

              '{
                lazy val inst: TC[_T] = ${ i.asExprOf[TC[_T]] }
                ${ loop(tail, acc :+ 'inst.asInstanceOf[Expr[TC[Any]]]) }
              }
            case Nil =>
              useTypeClassInstances(new LazyTypeClasses[TC](acc))
          }

        loop(cases.zip(summonTypeClass[TC]).toList, IArray.empty)
      }

      def instanceFromLazyTypeClasses[TC[_]: Type](
          useTypeClassInstances: LazyTypeClasses[TC] => Expr[TC[A]],
      ): Expr[TC[A]] =
        withLazyTypeClasses[TC, TC[A]](useTypeClassInstances)

    }

  }
  object IntersectionGeneric {

    def of[A](using Type[A]): IntersectionGeneric[A] =
      IntersectionGeneric.attemptOf[A].getOrAbort(s"Unable to derive IntersectionGeneric[${TypeRepr.of[A].show}]:\n  ")

    def attemptOf[A](using Type[A]): Either[String, IntersectionGeneric[A]] = {
      def expandTypes(repr: TypeRepr): NonEmptyList[TypeRepr] = repr.dealias match
        case TypeRepr.AndOrType.AndType(a, b) => expandTypes(a) ::: expandTypes(b)
        case _                                => NonEmptyList.one(repr)

      val _typeRepr: TypeRepr = TypeRepr.of[A]
      val expanded: NonEmptyList[TypeRepr] = expandTypes(_typeRepr)

      Either.cond(expanded.length >= 2, (), s"Type ${_typeRepr.show} is not an intersection type").map { _ =>
        new IntersectionGeneric[A] {
          override val typeRepr: TypeRepr = _typeRepr
          override val cases: IArray[Case[? >: A]] =
            IArray.from {
              expanded.toList.zipWithIndex.map { case (repr, i) =>
                type _T >: A
                Case[_T](i, repr)
              }
            }
        }
      }
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension [A](self: Either[String, A])
    private def getOrAbort(prefix: String): A = self match
      case Right(value) => value
      case Left(error)  => report.errorAndAbort(s"$prefix$error")

}
object K0 {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Derivable
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Derivable[T[_]] {

    protected def internalDeriveProduct[Q <: Quotes, A](k0: K0[Q])(g: k0.ProductGeneric[A])(using quotes: Q, tpe: Type[A]): Expr[T[A]]
    protected def internalDeriveSum[Q <: Quotes, A](k0: K0[Q])(g: k0.SumGeneric[A])(using quotes: Q, tpe: Type[A]): Expr[T[A]]

    protected final def derivedImpl[A](using quotes: Quotes, aTpe: Type[A]): Expr[T[A]] = {
      val meta: Meta[quotes.type] = Meta(quotes)
      val k0: K0[quotes.type] = K0(meta)

      val g: k0.Generic[A] = k0.Generic.of[A]
      val derivedExpr = g match
        case g: k0.ProductGeneric[A] => internalDeriveProduct[quotes.type, A](k0)(g)(using quotes, aTpe)
        case g: k0.SumGeneric[A]     => internalDeriveSum[quotes.type, A](k0)(g)(using quotes, aTpe)

      if (g.optionalAnnotation[showDerivation].nonEmpty)
        meta.report.info(derivedExpr.show)

      derivedExpr
    }

    /*
    inline def derived[A]: T[A] = ${ derivedImpl[A] }
     */

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      DerivableUnion
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait DerivableUnion[T[_]] {

    protected def internalDeriveUnion[Q <: Quotes, A](k0: K0[Q])(g: k0.UnionGeneric[A])(using quotes: Q, aTpe: Type[A], tTpe: Type[T]): Expr[T[A]]

    protected final def derivedUnionImpl[A](using quotes: Quotes, aTpe: Type[A], tTpe: Type[T]): Expr[T[A]] = {
      val meta: Meta[quotes.type] = Meta(quotes)
      val k0: K0[quotes.type] = K0(meta)

      val g: k0.UnionGeneric[A] = k0.UnionGeneric.of[A]

      internalDeriveUnion[quotes.type, A](k0)(g)(using quotes, aTpe, tTpe)
    }

    /*
    inline def derivedUnion[A]: T[A] = ${ derivedUnionImpl[A] }
     */

  }
  object DerivableUnion {

    trait Fold[T[_]] extends DerivableUnion[T] {

      override protected final def internalDeriveUnion[Q <: Quotes, A](k0: K0[Q])(g: k0.UnionGeneric[A])(using quotes: Q, tpe: Type[A], tTpe: Type[T]): Expr[T[A]] = {
        val expr: Expr[T[A]] =
          g.builders.instanceFromLazyTypeClasses[T] { tcs =>
            val head = g.cases.head
            val tail = g.cases.tail

            tail
              .foldLeft((head.typeRepr, head.typeClassInstance(tcs).asInstanceOf[Expr[T[Any]]])) { case ((_accT, _acc), _kase) =>
                type A1 <: A
                type A2 <: A
                val acc: Expr[T[A1]] = _acc.asInstanceOf[Expr[T[A1]]]
                val kase: g.Case[A2] = _kase.asInstanceOf[g.Case[A2]]
                val tpe1: Type[A1] = _accT.asTyped
                val tpe2: Type[A2] = kase.tpe

                (
                  k0.meta.TypeRepr.AndOrType.OrType(_accT, kase.typeRepr),
                  foldUnion[Q, A1, A2](acc, kase.typeClassInstance(tcs))(using quotes, tpe1, tpe2, tTpe).asInstanceOf[Expr[T[Any]]],
                )
              }
              ._2
              .asInstanceOf[Expr[T[A]]]
          }

        // k0.meta.report.info(expr.show)

        expr
      }

      protected def foldUnion[Q <: Quotes, A1, A2](a1: Expr[T[A1]], a2: Expr[T[A2]])(using quotes: Q, tpe1: Type[A1], tpe2: Type[A2], tTpe: Type[T]): Expr[T[A1 | A2]]

      /*
      inline def derivedUnion[A]: T[A] = ${ derivedUnionImpl[A] }
       */

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      DerivableIntersection
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait DerivableIntersection[T[_]] {

    protected def internalDeriveIntersection[Q <: Quotes, A](k0: K0[Q])(g: k0.IntersectionGeneric[A])(using quotes: Q, aTpe: Type[A], tTpe: Type[T]): Expr[T[A]]

    protected final def derivedIntersectionImpl[A](using quotes: Quotes, aTpe: Type[A], tTpe: Type[T]): Expr[T[A]] = {
      val meta: Meta[quotes.type] = Meta(quotes)
      val k0: K0[quotes.type] = K0(meta)

      val g: k0.IntersectionGeneric[A] = k0.IntersectionGeneric.of[A]

      internalDeriveIntersection[quotes.type, A](k0)(g)(using quotes, aTpe, tTpe)
    }

    /*
    inline def derivedIntersection[A]: T[A] = ${ derivedIntersectionImpl[A] }
     */

  }
  object DerivableIntersection {

    trait Fold[T[_]] extends DerivableIntersection[T] {

      override protected final def internalDeriveIntersection[Q <: Quotes, A](k0: K0[Q])(g: k0.IntersectionGeneric[A])(using quotes: Q, tpe: Type[A], tTpe: Type[T]): Expr[T[A]] = {
        val expr: Expr[T[A]] =
          g.builders.instanceFromLazyTypeClasses[T] { tcs =>
            val head = g.cases.head
            val tail = g.cases.tail

            tail
              .foldLeft((head.typeRepr, head.typeClassInstance(tcs).asInstanceOf[Expr[T[Any]]])) { case ((_accT, _acc), _kase) =>
                type A1 >: A
                type A2 >: A
                val acc: Expr[T[A1]] = _acc.asInstanceOf[Expr[T[A1]]]
                val kase: g.Case[A2] = _kase.asInstanceOf[g.Case[A2]]
                val tpe1: Type[A1] = _accT.asTyped
                val tpe2: Type[A2] = kase.tpe

                (
                  k0.meta.TypeRepr.AndOrType.AndType(_accT, kase.typeRepr),
                  foldIntersection[Q, A1, A2](acc, kase.typeClassInstance(tcs))(using quotes, tpe1, tpe2, tTpe).asInstanceOf[Expr[T[Any]]],
                )
              }
              ._2
              .asInstanceOf[Expr[T[A]]]
          }

        // k0.meta.report.info(expr.show)

        expr
      }

      protected def foldIntersection[Q <: Quotes, A1, A2](a1: Expr[T[A1]], a2: Expr[T[A2]])(using quotes: Q, tpe1: Type[A1], tpe2: Type[A2], tTpe: Type[T]): Expr[T[A1 & A2]]

      /*
      inline def derivedIntersection[A]: T[A] = ${ derivedIntersectionImpl[A] }
       */

    }

  }

}
