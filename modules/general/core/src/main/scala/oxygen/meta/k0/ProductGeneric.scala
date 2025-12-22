package oxygen.meta.k0

import oxygen.core.*
import oxygen.core.collection.*
import oxygen.core.syntax.common.*
import oxygen.core.typeclass.*
import oxygen.meta.*
import oxygen.meta.k0 as PKG
import oxygen.quoted.*
import scala.collection.immutable.ArraySeq
import scala.quoted.*

sealed trait ProductGeneric[A] extends Generic.ProductGeneric[A] { productGeneric =>

  override final type ChildBound = Any
  override final type Child[B] = Field[B]

  override type SelfType[A2] <: ProductGeneric[A2]

  override val typeType: TypeType.Case
  protected val subTypeName: String

  def fields: ArraySeq[Field[?]]

  override final def children: ArraySeq[AnyChild] = fields.asInstanceOf[ArraySeq[AnyChild]]

  def fieldsToInstance[S[_]: SeqRead](exprs: S[Expr[?]])(using Quotes): Expr[A]

  override final def toIndentedString: IndentedString =
    IndentedString.section(s"ProductGeneric.$subTypeName[${typeRepr.showCode}]")(fields.map(_.toIndentedString))

  def filtered[SubsetT](f: ChildFunction0[PKG.Const[Boolean]])(using Quotes): ProductGenericSubset[A, SubsetT] = {
    val fieldSubset: ArraySeq[Field[?]] = fields.filter { field0 =>
      type T
      val field: Field[T] = field0.typedAs[T]
      given Type[T] = field.tpe
      f(field)
    }

    fieldSubset match {
      case ArraySeq() =>
        val _unitTypeRepr: TypeRepr = TypeRepr.of[Unit]
        val empty: ProductGenericSubset.Empty[A] =
          new ProductGenericSubset.Empty[A] {
            override val unitTypeRepr: TypeRepr = _unitTypeRepr
            override val aGeneric: ProductGeneric[A] = productGeneric
          }
        empty.typedAs[SubsetT]
      case ArraySeq(aField0) =>
        val _aField: Field[SubsetT] = aField0.typedAs[SubsetT]
        new ProductGenericSubset.Single[A, SubsetT] {
          override val aGeneric: productGeneric.type = productGeneric
          override val aField: aGeneric.Field[SubsetT] = _aField
        }
      case _aFields =>
        val tupleTypeRepr: TypeRepr = TypeRepr.tuplePreferTupleN(_aFields.map(_.typeRepr).toList)
        new ProductGenericSubset.Many[A, SubsetT] {
          override val aGeneric: productGeneric.type = productGeneric
          override val aFields: ArraySeq[productGeneric.Field[?]] = _aFields
          override val bGeneric: ProductGeneric.CaseClassGeneric[SubsetT] =
            ProductGeneric.CaseClassGeneric.unsafeOf[SubsetT](tupleTypeRepr, tupleTypeRepr.typeSymbol, productGeneric.derivedFromConfig)
        }
    }
  }

  val instantiate: Instantiate = new Instantiate

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Inner Classes
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /////// Field ///////////////////////////////////////////////////////////////

  final case class Field[B](
      idx: Int,
      typeRepr: TypeRepr,
      constructorValDef: ValDef,
      fieldSym: Symbol,
  ) extends Entity.Child[Any, B, A] {

    override type SelfType[A2] = Field[A2]

    override val childType: String = "field"

    override val label: String = constructorValDef.name

    override val sym: Symbol = constructorValDef.symbol

    def fieldDef: ValOrDefDef = fieldSym.tree.narrow[ValOrDefDef]

    override def parentGeneric: ProductGeneric[A] = productGeneric

    override def pos: Position = constructorValDef.pos

    override def annotations(using Quotes): AnnotationsTyped[B] = AnnotationsTyped(constructorValDef.symbol.annotations.all, constructorValDef.show)

    override def toIndentedString: IndentedString = s"$name: ${typeRepr.showCode}"

    def fromParentTerm(parent: Term): Term = parent.select(fieldSym)

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

    def fieldsToInstance[S[_]: SeqRead](exprs: S[Expr[?]])(using Quotes): Expr[A] =
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
              case Nil => monad.mapE(value) { a => productGeneric.fieldsToInstance((acc :+ a).toArraySeq) }
              case _   => monad.flatMapE(value) { a => rec(tail, acc :+ a) }
            }
          case Nil =>
            monad.pure(productGeneric.fieldsToInstance(acc.toArraySeq))
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

}
object ProductGeneric {

  def apply[A: ProductGeneric as g]: ProductGeneric[A] = g

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Summon Instance
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private[k0] def unsafeOf[A](
      repr: TypeRepr,
      sym: Symbol,
      config: Derivable.Config,
  )(using Quotes): ProductGeneric[A] =
    sym.typeType.product.required match {
      case _: TypeType.Case.Class =>
        CaseClassGeneric.unsafeOf[A](repr, sym, config)
      case _: TypeType.Case.Object =>
        CaseObjectGeneric.unsafeOf[A](repr, sym, config)
    }

  def of[A](using Type[A], Quotes): ProductGeneric[A] = ProductGeneric.of[A](Derivable.Config())
  def of[A](config: Derivable.Config)(using Type[A], Quotes): ProductGeneric[A] =
    Generic.of[A](config) match
      case g: ProductGeneric[A] => g
      case _                    => report.errorAndAbort(s"Not a product type: ${TypeRepr.of[A].show}", TypeRepr.of[A].typeOrTermSymbol.pos)

  def ofTuple[A](tupleTypes: List[TypeRepr], config: Derivable.Config = Derivable.Config())(using Quotes): ProductGeneric[A] = {
    if tupleTypes.length < 2 then report.errorAndAbort("`ProductGeneric.ofTuple` only works with tuple size >= 2")

    val typeRepr: TypeRepr = TypeRepr.tuplePreferTupleN(tupleTypes)
    val typeSym = typeRepr.typeSymbol
    ProductGeneric.unsafeOf[A](typeRepr, typeSym, config)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      deriveTransform
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Derives a transformation between [[SourceT]] and [[TargetT]].
    *
    * [[TargetT]] must be a type representable by a [[ProductGeneric]].
    *
    * Depending on the number of fields [[TargetT]] has, [[SourceT]] should be:
    *   0  : Unit
    *   1  : The type of single field withing [[TargetT]]
    *   2+ : Ordered tuple of fields within [[TargetT]]
    */
  inline def deriveTransform[SourceT, TargetT]: (SourceT => TargetT, TargetT => SourceT) = ${ deriveTransformImpl[SourceT, TargetT] }

  def deriveTransformExprsImpl[SourceT: Type, TargetT: Type](using Quotes): (Expr[SourceT => TargetT], Expr[TargetT => SourceT]) = {
    val productGeneric: ProductGeneric[TargetT] = ProductGeneric.of[TargetT]
    val sourceTypeRepr: TypeRepr = TypeRepr.of[SourceT].widen

    def fail(explanation: String, exp: TypeRepr): Nothing =
      report.errorAndAbort(
        s"""Error deriving auto-transformation between:
             |  target            : ${productGeneric.typeRepr.showAnsiCode}
             |  source   (actual) : ${sourceTypeRepr.showAnsiCode}
             |  source (expected) : ${exp.showAnsiCode}
             |
             |  explanation: $explanation""".stripMargin,
      )

    productGeneric match {
      case productGeneric: ProductGeneric.NoFieldsGeneric[TargetT] =>
        val unitRepr: TypeRepr = TypeRepr.of[Unit]
        if !(sourceTypeRepr =:= unitRepr) then fail("Target type has 0 fields, therefore source type needs to be Unit", unitRepr)

        val typedUnitExpr: Expr[SourceT] = '{ () }.asExprOf[SourceT]

        val abExpr: Expr[SourceT => TargetT] = '{ { (_: SourceT) => ${ productGeneric.instantiate.instance } } }
        val baExpr: Expr[TargetT => SourceT] = '{ { (_: TargetT) => $typedUnitExpr } }

        (abExpr, baExpr)
      case productGeneric: ProductGeneric.SingleFieldCaseClassGeneric[TargetT, ?] =>
        productGeneric.optionalSingleFieldTypedAs[SourceT] match {
          case Some(productGeneric: ProductGeneric.SingleFieldCaseClassGeneric[TargetT, SourceT]) =>
            (productGeneric.singleField.wrapExpr, productGeneric.singleField.unwrapExpr)
          case None =>
            fail(
              s"Target type has 1 field (${productGeneric.field.name}: ${productGeneric.field.typeRepr.showAnsiCode}), therefore source type must match that single field",
              productGeneric.field.typeRepr,
            )
        }
      case productGeneric: ProductGeneric.CaseClassGeneric[TargetT] =>
        type TupleT
        val tupleGeneric: ProductGeneric[TupleT] = ProductGeneric.ofTuple[TupleT](productGeneric.fields.map(_.typeRepr).toList)
        if !(tupleGeneric.typeRepr =:= sourceTypeRepr) then fail(s"Target type has 2+ fields, therefore source type must be a tuple of those fields", tupleGeneric.typeRepr)

        val reTypedTupleGeneric: ProductGeneric[SourceT] = tupleGeneric.typedAs[SourceT]

        def transformGeneric[A, B](a: ProductGeneric[A], b: ProductGeneric[B]): Expr[A => B] = {
          import a.tpe as aTpe
          import b.tpe as bTpe
          '{ { (in: A) => ${ b.instantiate.fieldsToInstance(a.fields.map(_.fromParent('in))) } } }
        }

        val abExpr: Expr[SourceT => TargetT] = transformGeneric(reTypedTupleGeneric, productGeneric)
        val baExpr: Expr[TargetT => SourceT] = transformGeneric(productGeneric, reTypedTupleGeneric)

        (abExpr, baExpr)
    }
  }

  def deriveTransformImpl[SourceT: Type, TargetT: Type](using Quotes): Expr[(SourceT => TargetT, TargetT => SourceT)] = {
    val (abExpr, baExpr) = deriveTransformExprsImpl[SourceT, TargetT]
    '{ ($abExpr, $baExpr) }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      NoFieldsGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait NoFieldsGeneric[A] private[ProductGeneric] () extends ProductGeneric[A] { generic =>

    override final val fields: ArraySeq[Field[?]] = ArraySeq.empty

    def fieldsToInstance0(using Quotes): Expr[A]
    override final def fieldsToInstance[S[_]: SeqRead](exprs: S[Expr[?]])(using Quotes): Expr[A] =
      exprs.into[List] match {
        case Nil   => fieldsToInstance0
        case exprs => report.errorAndAbort(s"attempted to instantiate case object with non-empty fields (${exprs.size})")
      }

    override val instantiate: NoFieldsInstantiate = new NoFieldsInstantiate

    /////// Instantiate ///////////////////////////////////////////////////////////////

    class NoFieldsInstantiate extends Instantiate {

      final def instance(using Quotes): Expr[A] = generic.fieldsToInstance(Nil)

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      CaseObjectGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait CaseObjectGeneric[A] private[ProductGeneric] () extends NoFieldsGeneric[A] { generic =>

    override final type SelfType[A2] = CaseObjectGeneric[A2]

    override val typeType: TypeType.Case.Object
    override protected val subTypeName: String = "CaseObjectGeneric"

  }
  object CaseObjectGeneric {

    def of[A](using Type[A], Quotes): ProductGeneric.CaseObjectGeneric[A] =
      ProductGeneric.of[A] match
        case gen: ProductGeneric.CaseObjectGeneric[A] => gen
        case gen                                      => report.errorAndAbort(s"internal defect : CaseObjectGeneric.of did not return a CaseObjectGeneric:\n$gen")

    private[ProductGeneric] def unsafeOf[A](
        _typeRepr: TypeRepr,
        _termSym: Symbol,
        config: Derivable.Config,
    ): CaseObjectGeneric[A] =
      new CaseObjectGeneric[A] {

        override val label: String = _termSym.name
        override val sym: Symbol = _termSym
        override val typeRepr: TypeRepr = _typeRepr

        override val typeType: TypeType.Case.Object = _typeRepr.typeType.caseObject.required(using typeRepr.quotes)
        override val derivedFromConfig: Derivable.Config = config

        override def fieldsToInstance0(using Quotes): Expr[A] =
          _termSym.toTerm.asExprOf[A]

      }

    def unit(unitTypeRepr: TypeRepr, fromConfig: Derivable.Config = Derivable.Config()): CaseObjectGeneric[Unit] =
      new ProductGeneric.CaseObjectGeneric[Unit] {
        override val label: String = "Unit"
        override val typeRepr: TypeRepr = unitTypeRepr
        override val sym: Symbol = typeRepr.termSymbol
        override val typeType: TypeType.Case.Object = TypeType.CaseObject
        override val derivedFromConfig: Derivable.Config = fromConfig
        override def fieldsToInstance0(using Quotes): Expr[Unit] = '{ () }
      }

    def unit(using Quotes): CaseObjectGeneric[Unit] = CaseObjectGeneric.unit(TypeRepr.of[Unit])

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      CaseClassGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait CaseClassGeneric[A] private[ProductGeneric] () extends ProductGeneric[A] {
    override val typeType: TypeType.Case.Class
    override protected val subTypeName: String = "CaseClassGeneric"
  }
  object CaseClassGeneric {

    def of[A](using Type[A], Quotes): ProductGeneric.CaseClassGeneric[A] =
      ProductGeneric.of[A] match
        case gen: ProductGeneric.CaseClassGeneric[A] => gen
        case gen                                     => report.errorAndAbort(s"internal defect : CaseClassGeneric.of did not return a CaseClassGeneric:\n$gen")

    private[ProductGeneric] def unsafeOf[A](
        _typeRepr: TypeRepr,
        _typeSym: Symbol,
        config: Derivable.Config,
    )(using Quotes): CaseClassGeneric[A] = {
      val isAnyVal: Boolean =
        _typeRepr.typeSymbol.tree match
          case cdef: ClassDef if cdef.parents.headOption.flatMap(_.narrowOpt[TypeTree]).exists(_.tpe =:= TypeRepr.of[AnyVal]) => true
          case _ if _typeRepr <:< TypeRepr.of[AnyVal]                                                                         => true
          case _                                                                                                              => false

      val _primaryConstructorSym: Symbol = _typeSym.primaryConstructor
      val _primaryConstructor: DefDef = _primaryConstructorSym.tree.narrow[DefDef]

      val (constructorTypes, constructorTerms): (Option[TypeParamClause], TermParamClause) =
        _primaryConstructor.paramss match
          case List(types: TypeParamClause, terms: TermParamClause) => (types.some, terms)
          case List(terms: TermParamClause)                         => (None, terms)
          case _                                                    => report.errorAndAbort("Invalid case class structure. Expected single param group.")

      val constructorVals: List[ValDef] = constructorTerms.params
      val fieldSyms: List[Symbol] = _typeSym.caseFields

      if constructorVals.size != fieldSyms.size then report.errorAndAbort("Primary constructor size differs from case fields size?")

      val typeArgs: List[TypeRepr] = _typeRepr.dealias match
        case appTpe: AppliedType => appTpe.args
        case _                   => Nil

      // TODO (KR) : consider making this type replacement a shared utility
      val alterRepr: TypeRepr => TypeRepr =
        constructorTypes match {
          case Some(constructorTypes) =>
            val typeArgsSymbols: List[Symbol] = constructorTypes.params.map { s => _typeSym.typeMember(s.name) }

            if typeArgsSymbols.size != typeArgs.size then report.errorAndAbort("Type param symbols and reprs have different size?")

            _.substituteTypes(typeArgsSymbols, typeArgs)
          case None =>
            identity
        }

      val allDecls = _typeSym.declaredMethods

      val fieldTuple: ArraySeq[(Int, TypeRepr, ValDef, Symbol)] =
        ArraySeq.from(constructorVals.zip(fieldSyms)).zipWithIndex.map { case ((constructorVal, fieldSym), idx) =>
          if constructorVal.name != fieldSym.name then report.errorAndAbort("vals are not in same order?")

          val optFieldDef: Option[ValOrDefDef] =
            allDecls.filter(_.name == constructorVal.name).map(_.tree).collect { case d: ValOrDefDef => d } match {
              case single :: Nil => single.some
              case _             => None
            }

          // TODO (KR) : this is a very hacky way to get around -Yretain-trees bug + difficult type referencing in `alterRepr`
          val baseTypeRepr: TypeRepr =
            try { fieldSym.tree.narrow[ValOrDefDef].tpt.tpe }
            catch { case _: Throwable => optFieldDef.fold(constructorVal.tpt.tpe)(_.tpt.tpe) }

          val typeRepr: TypeRepr = alterRepr(baseTypeRepr)

          (idx, typeRepr, constructorVal, fieldSym)
        }

      val constructorAwaitingArgs: Term = {
        val pc = New.companion.apply(TypeTree.ref(_typeSym)).select(_primaryConstructorSym)
        constructorTypes match {
          case Some(_) => pc.appliedToTypes(typeArgs)
          case None    => pc
        }
      }

      fieldTuple match {
        case ArraySeq() =>
          new NoFieldsCaseClassGeneric[A] {

            override val label: String = _typeSym.name
            override val sym: Symbol = _typeSym
            override val typeRepr: TypeRepr = _typeRepr
            override val derivedFromConfig: Derivable.Config = config

            override val typeType: TypeType.Case.Class = _typeSym.typeType.caseClass.required(using typeRepr.quotes)

            override def fieldsToInstance0(using Quotes): Expr[A] =
              constructorAwaitingArgs
                .appliedToArgs(Nil)
                .asExprOf[A]

          }
        case ArraySeq(fieldTuple) =>
          type B
          if isAnyVal then
            new AnyValGeneric[A, B] {

              override val label: String = _typeSym.name
              override val sym: Symbol = _typeSym
              override val typeRepr: TypeRepr = _typeRepr
              override val derivedFromConfig: Derivable.Config = config

              override val typeType: TypeType.Case.Class = _typeSym.typeType.caseClass.required(using typeRepr.quotes)
              override val field: Field[B] =
                Field(
                  idx = fieldTuple._1,
                  typeRepr = fieldTuple._2,
                  constructorValDef = fieldTuple._3,
                  fieldSym = fieldTuple._4,
                )

              override def fieldsToInstance1(expr: Expr[B])(using Quotes): Expr[A] =
                constructorAwaitingArgs
                  .appliedToArgs(expr.toTerm :: Nil)
                  .asExprOf[A]

            }
          else
            new SingleFieldCaseClassGeneric[A, B] {

              override val label: String = _typeSym.name
              override val sym: Symbol = _typeSym
              override val typeRepr: TypeRepr = _typeRepr
              override val derivedFromConfig: Derivable.Config = config

              override val typeType: TypeType.Case.Class = _typeSym.typeType.caseClass.required(using typeRepr.quotes)
              override val field: Field[B] =
                Field(
                  idx = fieldTuple._1,
                  typeRepr = fieldTuple._2,
                  constructorValDef = fieldTuple._3,
                  fieldSym = fieldTuple._4,
                )

              override def fieldsToInstance1(expr: Expr[B])(using Quotes): Expr[A] =
                constructorAwaitingArgs
                  .appliedToArgs(expr.toTerm :: Nil)
                  .asExprOf[A]

            }
        case _ =>
          new CaseClassGeneric[A] {

            override val label: String = _typeSym.name
            override val sym: Symbol = _typeSym
            override val typeRepr: TypeRepr = _typeRepr
            override val typeType: TypeType.Case.Class = _typeSym.typeType.caseClass.required(using typeRepr.quotes)
            override val derivedFromConfig: Derivable.Config = config

            override val fields: ArraySeq[Field[?]] =
              fieldTuple.map { case (idx, typeRepr, constructorVal, fieldSym) =>
                Field(
                  idx = idx,
                  typeRepr = typeRepr,
                  constructorValDef = constructorVal,
                  fieldSym = fieldSym,
                )
              }

            override def fieldsToInstance[S[_]: SeqRead](exprs: S[Expr[?]])(using quotes: Quotes): Expr[A] = {
              val exprSize = exprs.size
              if exprSize != fields.length then report.errorAndAbort(s"Provided exprs ($exprSize) != num fields (${fields.length})")

              constructorAwaitingArgs
                .appliedToArgs(exprs.into[List].map(_.toTerm))
                .asExprOf[A]
            }

          }
      }
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      NoFieldsCaseClassGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait NoFieldsCaseClassGeneric[A] private[ProductGeneric] () extends CaseClassGeneric[A], NoFieldsGeneric[A] { generic =>

    override protected val subTypeName: String = "NoFieldsCaseClassGeneric"

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      SingleFieldCaseClassGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait SingleFieldCaseClassGeneric[A, B] private[ProductGeneric] () extends CaseClassGeneric[A] {

    def singleFieldTypedAs[TypeName]: SingleFieldCaseClassGeneric[A, TypeName] = this.asInstanceOf[SingleFieldCaseClassGeneric[A, TypeName]]

    def optionalSingleFieldTypedAs[TypeName: Type as newType](using Quotes): Option[SingleFieldCaseClassGeneric[A, TypeName]] =
      Option.when(newType.toTypeRepr =:= field.typeRepr) { this.singleFieldTypedAs[TypeName] }

    val field: Field[B]

    given bTpe: Type[B] = field.tpe

    override protected val subTypeName: String = "SingleFieldCaseClassGeneric"
    override final lazy val fields: ArraySeq[Field[?]] = ArraySeq(field)

    def fieldsToInstance1(expr: Expr[B])(using Quotes): Expr[A]
    override final def fieldsToInstance[S[_]: SeqRead](exprs: S[Expr[?]])(using Quotes): Expr[A] =
      exprs.into[List] match
        case expr :: Nil => fieldsToInstance1(expr.asExprOf[B])
        case exprs       => report.errorAndAbort(s"attempted to instantiate SingleFieldCaseClassGeneric with non-single fields (${exprs.size})")

    val singleField: SingleField = new SingleField

    /////// SingleField ///////////////////////////////////////////////////////////////

    class SingleField {

      final def wrap(value: Expr[B])(using Quotes): Expr[A] = fieldsToInstance1(value)

      final def unwrap(value: Expr[A])(using Quotes): Expr[B] = field.fromParent(value)

      final def wrapExpr(using Quotes): Expr[B => A] =
        '{ (expr: B) => ${ wrap('expr) } }

      final def unwrapExpr(using Quotes): Expr[A => B] =
        '{ (expr: A) => ${ unwrap('expr) } }

    }

  }
  object SingleFieldCaseClassGeneric {

    def of[A](using Type[A], Quotes): ProductGeneric.SingleFieldCaseClassGeneric[A, ?] =
      ProductGeneric.of[A] match
        case gen: ProductGeneric.SingleFieldCaseClassGeneric[A, ?] => gen
        case gen                                                   => report.errorAndAbort(s"internal defect : SingleFieldCaseClassGeneric.of did not return a SingleFieldCaseClassGeneric:\n$gen")

    def ofTypeField[A, B](using Type[A], Quotes): ProductGeneric.SingleFieldCaseClassGeneric[A, B] =
      ProductGeneric.SingleFieldCaseClassGeneric.of[A].singleFieldTypedAs[B]

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      AnyValGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait AnyValGeneric[A, B] private[ProductGeneric] () extends SingleFieldCaseClassGeneric[A, B] {

    override def singleFieldTypedAs[TypeName]: AnyValGeneric[A, TypeName] = this.asInstanceOf[AnyValGeneric[A, TypeName]]

    override def optionalSingleFieldTypedAs[TypeName: Type as newType](using Quotes): Option[AnyValGeneric[A, TypeName]] =
      Option.when(newType.toTypeRepr =:= field.typeRepr) { this.singleFieldTypedAs[TypeName] }

    override protected val subTypeName: String = "AnyValGeneric"

  }
  object AnyValGeneric {

    def of[A](using Type[A], Quotes): ProductGeneric.AnyValGeneric[A, ?] =
      ProductGeneric.of[A] match
        case gen: ProductGeneric.AnyValGeneric[A, ?] => gen
        case gen                                     => report.errorAndAbort(s"internal defect : AnyValGeneric.of did not return a AnyValGeneric:\n$gen")

    def ofTypeField[A, B](using Type[A], Quotes): ProductGeneric.AnyValGeneric[A, B] =
      ProductGeneric.AnyValGeneric.of[A].singleFieldTypedAs[B]

  }

}
