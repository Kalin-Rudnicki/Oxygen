package oxygen.meta2

import oxygen.core.RightProjection
import oxygen.core.syntax.extra.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.annotation.tailrec
import scala.quoted.*

object K0 {

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Generic
//////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Generic[A] {

    val label: String
    val sym: Symbol
    val typeRepr: TypeRepr
    val typeType: TypeType

    final given tpe: Type[A] = typeRepr.asTypeOf
    // final given quotes: Quotes = sym.quotes

    final def annotations(using Quotes): AnnotationsTyped[A] = AnnotationsTyped(typeRepr.annotations.all, typeRepr.show)

  }
  object Generic {

    def of[A: Type](using Quotes): Generic[A] =
      TypeRepr.of[A].typeType match {
        case Some(_: TypeType.Case)   => ProductGeneric.of[A]
        case Some(_: TypeType.Sealed) => SumGeneric.of[A]
        case None                     => report.errorAndAbort(s"Type ${TypeRepr.of[A].show} is not a product or sum type")
      }

  }

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      ProductGeneric
//////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait ProductGeneric[A] private extends Generic[A] { generic =>

    override val typeType: TypeType.Case

    def fields: Contiguous[Field[?]]

    def fieldsToInstance[S[_]: SeqOps](exprs: S[Expr[?]])(using Quotes): Expr[A]

    // FIX-PRE-MERGE (KR) :
    final class ValDefinitions[F[_]](fTpe: Type[F], make: Quotes ?=> Contiguous[(ValDef, Type[?])]) {

      private given Type[F] = fTpe

      def defineAndUse[B: Type](f: Quotes ?=> Expressions[F] => Expr[B])(using quotes: Quotes): Expr[B] = {
        val vals: Contiguous[(ValDef, Type[?])] = make(using quotes)
        val newQuotes: Quotes = vals.lastOption.fold(quotes)(_._1.symbol.asQuotes) // this might be unnecessary
        val expressions: Expressions[F] =
          Expressions[F](
            fTpe,
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

    final class Expressions[F[_]](
        fTpe: Type[F],
        expressions: Contiguous[Expressions.Elem[F, ?]],
    ) {

      private given Type[F] = fTpe

      def at[B: Type](idx: Int)(using Quotes): Expr[F[B]] =
        expressions.at(idx).expr.asExprOf[F[B]]

      def mapK[G[_]: Type as gTpe](f: [b] => Type[b] ?=> Expr[F[b]] => Expr[G[b]]): Expressions[G] =
        Expressions[G](
          gTpe,
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

    final case class Field[B](
        idx: Int,
        typeRepr: TypeRepr,
        constructorValDef: ValDef,
        fieldValDef: ValDef,
    ) {

      def name: String = fieldValDef.name

      given tpe: Type[B] = typeRepr.asTypeOf

      def summonTypeClass[TC[_]: Type](using quotes: Quotes): Expr[TC[B]] =
        Implicits.search(TypeRepr.of[TC[B]]) match
          case ImplicitSearchSuccess(tree)        => tree.asExprOf[TC[B]] // FIX-PRE-MERGE (KR) :
          case ImplicitSearchFailure(explanation) => report.errorAndAbort(s"[${Expr.summon[TC[B]].nonEmpty}] Error summoning ${TypeRepr.of[TC[B]].show}\n\n$explanation", constructorValDef.pos)

      def annotations(using Quotes): AnnotationsTyped[B] = AnnotationsTyped(constructorValDef.symbol.annotations.all, constructorValDef.show)

      def get(parent: Expr[A])(using quotes: Quotes): Expr[B] =
        parent.toTerm.select(fieldValDef.symbol).asExprOf[B]

      def getExpr[F[_]](expressions: Expressions[F])(using Quotes): Expr[F[B]] =
        expressions.at[B](idx)

    }

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

      def flatMapJoin[S[_]: SeqOps, Out](
          sep: Out,
      )(f: [b] => Type[b] ?=> Field[b] => S[Out]): Growable[Out] =
        Growable
          .many(fields)
          .map { _field =>
            type _b
            val field: Field[_b] = _field.asInstanceOf[Field[_b]]

            Growable.many(f[_b](using field.tpe)(field))
          }
          .intersperse(Growable.single(sep))
          .flatten

      def flatMapJoin[S[_]: SeqOps, Out](
          start: Out,
          sep: Out,
          end: Out,
      )(f: [b] => Type[b] ?=> Field[b] => S[Out]): Growable[Out] =
        Growable
          .many(fields)
          .map { _field =>
            type _b
            val field: Field[_b] = _field.asInstanceOf[Field[_b]]

            Growable.many(f[_b](using field.tpe)(field))
          }
          .surround(Growable.single(start), Growable.single(sep), Growable.single(end))
          .flatten

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

      def cacheVals[F[_]: Type as fTpe](
          valName: String => String = n => s"${n}_value",
          valType: ValType = ValType.Val,
      )(
          f: [b] => (Quotes, Type[b]) ?=> Field[b] => Expr[F[b]],
      )(using Quotes): ValDefinitions[F] = {
        val flags: Flags = valType match
          case ValType.Val     => Flags.EmptyFlags
          case ValType.LazyVal => Flags.Lazy
          case ValType.Var     => Flags.Mutable

        @tailrec
        def rec(queue: List[Field[?]], acc: Growable[(ValDef, Type[?])]): Contiguous[(ValDef, Type[?])] =
          queue match {
            case head :: tail =>
              type B
              val field: Field[B] = head.asInstanceOf[Field[B]]
              given bTpe: Type[B] = field.tpe
              val newSym: Symbol = Symbol.companion.newVal(Symbol.spliceOwner, valName(field.name), TypeRepr.of[F[B]], flags, Symbol.noSymbol)
              val newDef: ValDef = ValDef.companion.apply(newSym, f[B](using quotes, field.tpe)(field).toTerm(using quotes).some)
              rec(tail, acc :+ (newDef, field.tpe))
            case Nil =>
              acc.to[Contiguous]
          }

        new ValDefinitions[F](fTpe, (quotes: Quotes) ?=> rec(fields.toList, Growable.empty))
      }

      def cacheTypeClassInstances[F[_]: Type](
          valName: String => String = n => s"${n}_instance",
          valType: ValType = ValType.LazyVal,
      )(using quotes: Quotes): ValDefinitions[F] =
        cacheVals[F](valName = valName, valType = valType) { [b] => (q, _) ?=> (field: Field[b]) => field.summonTypeClass[F](using quotes = quotes) }

      def showTypeClassInstances[F[_]: Type](using Quotes): Unit =
        fields.foreach { _field =>
          type B
          val field: Field[B] = _field.asInstanceOf[Field[B]]
          import field.tpe

          def msg(label: String, str: String): String =
            s"""field: ${field.name}
               |type: ${field.typeRepr.show}
               |$label: $str""".stripMargin

          Implicits.search(TypeRepr.of[F[B]]) match
            case success: ImplicitSearchSuccess => report.info(msg("instance", success.tree.show), field.constructorValDef.pos)
            case failure: ImplicitSearchFailure => report.warning(msg("explanation", failure.explanation), field.constructorValDef.pos)
        }

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
              val typeArgsSymbols: List[Symbol] = constructorTypes.params.map(p => _typeSym.typeMember(p.name))

              if (typeArgsSymbols.size != typeArgs.size)
                report.errorAndAbort("Type param symbols and reprs have different size?")

              // FIX-PRE-MERGE (KR) : remove
              /*
              report.errorAndAbort(
                "types:" + "\n\n" +
                  typeArgsSymbols.map(_.fullName).mkString("\n") + "\n\n" +
                  tpeArgs.map { a => s"${a.show} => ${a.dealias.show}" }.mkString("\n"),
              )
               */

              _.substituteTypes(typeArgsSymbols, typeArgs)
            case None =>
              identity
          }

        val fieldTuple: Contiguous[(Int, TypeRepr, ValDef, ValDef)] =
          Contiguous.from(constructorVals.zip(fieldVals)).zipWithIndex.map { case ((constructorVal, fieldVal), idx) =>
            if (constructorVal.name != fieldVal.name)
              report.errorAndAbort("vals are not in same order?")

            val typeRepr: TypeRepr = alterRepr(constructorVal.tpt.tpe)

            // FIX-PRE-MERGE (KR) : remove
            // report.info(typeRepr.show, constructorVal.pos)

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

  trait SumGeneric[A] private extends Generic[A] { generic =>

    override val typeType: TypeType.Sealed

    val cases: Contiguous[Case[? <: A]]

    final case class Case[B <: A](
        idx: Int,
        productGeneric: ProductGeneric[B],
    ) {

      given tpe: Type[B] = productGeneric.tpe
      // given quotes: Quotes = productGeneric.quotes

      def label: String = productGeneric.label

      def annotations(using Quotes): AnnotationsTyped[B] = productGeneric.annotations

    }

    // FIX-PRE-MERGE (KR) :
    final class ValDefinitions[F[_]] {

      def defineAndUse[B: Type](f: Quotes ?=> Expressions[F] => Expr[B])(using quotes: Quotes): Expr[B] =
        ??? // FIX-PRE-MERGE (KR) :

    }

    // FIX-PRE-MERGE (KR) :
    final class Expressions[F[_]]

    object util {

      def map[Out](f: [b <: A] => Type[b] ?=> Case[b] => Out): Growable[Out] =
        Growable.many(cases).map { _case =>
          type _b <: A
          val `case`: Case[_b] = _case.asInstanceOf[Case[_b]]

          f[_b](using `case`.tpe)(`case`)
        }

      def mapExpr[Out](f: [b <: A] => Type[b] ?=> Case[b] => Expr[Out]): Growable[Expr[Out]] =
        map[Expr[Out]](f)

      // FIX-PRE-MERGE (KR) :

      def cacheVals[F[_]: Type as fTpe](
          valName: String => String = n => s"${n}_value",
          valType: ValType = ValType.Val,
      )(
          f: [b <: A] => (Quotes, Type[b]) ?=> Case[b] => Expr[F[b]],
      ): ValDefinitions[F] =
        ??? // FIX-PRE-MERGE (KR) :

      def cacheTypeClassInstances[F[_]: Type](
          valName: String => String = n => s"${n}_instance",
          valType: ValType = ValType.LazyVal,
      ): ValDefinitions[F] =
        ??? // FIX-PRE-MERGE (KR) :

    }

  }
  object SumGeneric {

    def of[A: Type](using Quotes): SumGeneric[A] =
      ??? // TODO (KR) :

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Derivable
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Derivable[F[_]] {

    protected def deriveProductImpl[A](g: ProductGeneric[A])(using Quotes, Type[F], Type[A]): Expr[F[A]]

    protected def deriveSumImpl[A](g: SumGeneric[A])(using Quotes, Type[F], Type[A]): Expr[F[A]]

    protected def deriveAnyValImpl[A, B](g: ProductGeneric.AnyValGeneric[A, B])(using Quotes, Type[F], Type[A], Type[B]): Expr[F[A]] =
      deriveProductImpl(g)

    protected def deriveCaseObjectImpl[A](g: ProductGeneric.CaseObjectGeneric[A])(using Quotes, Type[F], Type[A]): Expr[F[A]] =
      deriveProductImpl(g)

    protected final def derivedImpl[A](using Quotes, Type[F], Type[A]): Expr[F[A]] = {
      val g: Generic[A] = Generic.of[A]
      val res: Expr[F[A]] = g match {
        case g: ProductGeneric.AnyValGeneric[A, ?] =>
          type B
          val g2: ProductGeneric.AnyValGeneric[A, B] = g.asInstanceOf[ProductGeneric.AnyValGeneric[A, B]]
          given Type[B] = g2.bTpe
          deriveAnyValImpl(g2)
        case g: ProductGeneric.CaseObjectGeneric[A] => deriveCaseObjectImpl(g)
        case g: ProductGeneric.CaseClassGeneric[A]  => deriveProductImpl(g)
        case g: SumGeneric[A]                       => deriveSumImpl(g)
      }

      // FIX-PRE-MERGE (KR) : annotation
      // report.info(res.show)

      res
    }

  }
  object Derivable {

    trait WithInstances[F[_]] extends Derivable[F] {

      override protected final def deriveProductImpl[A](g: ProductGeneric[A])(using Quotes, Type[F], Type[A]): Expr[F[A]] = {
        g.util.showTypeClassInstances[F]
        report.errorAndAbort("die")
        ??? // g.util.cacheTypeClassInstances[F]().defineAndUse { exprs => deriveProductImplInst(g)(exprs) }
      }

      override protected final def deriveSumImpl[A](g: SumGeneric[A])(using Quotes, Type[F], Type[A]): Expr[F[A]] =
        g.util.cacheTypeClassInstances[F]().defineAndUse { exprs => deriveSumImplInst(g)(exprs) }

      protected def deriveProductImplInst[A](g: ProductGeneric[A])(i: g.Expressions[F])(using Quotes, Type[F], Type[A]): Expr[F[A]]

      protected def deriveSumImplInst[A](g: SumGeneric[A])(i: g.Expressions[F])(using Quotes, Type[F], Type[A]): Expr[F[A]]

    }

    trait WrapAnyVal[F[_]] { self: Derivable[F] =>

      protected def wrapAnyValInstance[A, B](a: Expr[F[A]], wrap: Expr[A] => Expr[B], unwrap: Expr[B] => Expr[A])(using Quotes, Type[A], Type[B]): Expr[F[B]]

      override protected final def deriveAnyValImpl[A, B](g: ProductGeneric.AnyValGeneric[A, B])(using Quotes, Type[F], Type[A], Type[B]): Expr[F[A]] =
        g.util.cacheTypeClassInstances[F]().defineAndUse { exprs =>
          wrapAnyValInstance(g.field.getExpr(exprs), g.util.wrap(_), g.util.unwrap(_))
        }

    }

  }

}
