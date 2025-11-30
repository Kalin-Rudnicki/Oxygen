package oxygen.sql.generic.model

import oxygen.meta.k0.*
import oxygen.predef.color.given
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.parsing.*
import scala.quoted.*

/**
  * Represents an Expr within a query.
  * This could be a direct reference to a [[VariableReference]],
  * or any supported combination of `row.field`, `field.get`, `a && b`, etc.
  */
private[generic] sealed trait QueryExpr {

  /**
    * Represents the full term of this [[RawQueryExpr]].
    * Whether that is a single identifier, or a combination of Exprs.
    */
  val fullTerm: Term
  def queryRefs: Growable[VariableReference]

  def show(using Quotes): String

  final def isAndOr: Boolean = this match
    case _: QueryExpr.BinaryAndOr => true
    case _                        => false

  final def showWrapAndOr(using Quotes): String = this match
    case _: QueryExpr.BinaryAndOr => s"($show)"
    case _                        => show

  final def getRowRepr(using Quotes, ParseContext): ParseResult[TypeclassExpr.RowRepr] =
    this match {
      case unary: QueryExpr.QueryVariableReferenceLike => ParseResult.success(unary.rowRepr)
      case _: QueryExpr.Binary                         => ParseResult.success(TypeclassExpr.RowRepr.boolean)
      case _                                           => ParseResult.error(fullTerm, "Unable to extract RowRepr")
    }

}
private[generic] object QueryExpr extends Parser[RawQueryExpr, QueryExpr] {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Const
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class ConstValue(fullTerm: Term, constTerm: Term) extends QueryExpr {

    override def queryRefs: Growable[VariableReference] = Growable.empty

    override def show(using Quotes): String =
      s"{ ${constTerm.showCode} }".cyanFg.toString

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      VariableReferenceLike
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait VariableReferenceLike extends QueryExpr {
    val rootIdent: Ident
    val queryRef: VariableReference
    override final def queryRefs: Growable[VariableReference] = Growable.single(queryRef)
    final lazy val internalParam: Function.NamedParam = queryRef.internalParam
  }
  object VariableReferenceLike extends Parser[RawQueryExpr.VariableReferenceLike, QueryExpr.VariableReferenceLike] {

    override def parse(expr: RawQueryExpr.VariableReferenceLike)(using ParseContext, Quotes): ParseResult[QueryExpr.VariableReferenceLike] =
      expr match {
        case RawQueryExpr.ReferencedVariable(term, queryRef: VariableReference.QueryLike) =>
          ParseResult.Success(QueryExpr.QueryVariableReferenceLike.ReferencedVariable(term, queryRef))
        case RawQueryExpr.ReferencedVariable(term, queryRef: VariableReference.InputLike) =>
          ParseResult.Success(QueryExpr.InputVariableReferenceLike.ReferencedVariable(term, queryRef))
        case RawQueryExpr.SelectProductField(select, inner) =>
          parse(inner).flatMap {
            case inner: QueryExpr.QueryVariableReferenceLike.CanSelect    => ParseResult.Success(QueryExpr.QueryVariableReferenceLike.ProductFieldSelect(select, inner))
            case inner: QueryExpr.InputVariableReferenceLike.CanSelect    => ParseResult.Success(QueryExpr.InputVariableReferenceLike.ProductFieldSelect(select, inner))
            case inner: QueryExpr.QueryVariableReferenceLike.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
            case inner: QueryExpr.InputVariableReferenceLike.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
          }
        case RawQueryExpr.OptionGet(select, inner) =>
          parse(inner).flatMap {
            case inner: QueryExpr.QueryVariableReferenceLike.CanSelect    => ParseResult.Success(QueryExpr.QueryVariableReferenceLike.OptionGet(select, inner))
            case inner: QueryExpr.QueryVariableReferenceLike.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
            case _: QueryExpr.InputVariableReferenceLike                  => ParseResult.error(select, "not supported: `Option.get` on input")
          }
        case RawQueryExpr.SelectPrimaryKey(apply, inner, givenTableRepr) =>
          parse(inner).flatMap {
            case inner: QueryExpr.QueryVariableReferenceLike.CanSelect    => ParseResult.Success(QueryExpr.QueryVariableReferenceLike.SelectPrimaryKey(apply, inner, givenTableRepr))
            case inner: QueryExpr.InputVariableReferenceLike.CanSelect    => ParseResult.Success(QueryExpr.InputVariableReferenceLike.SelectPrimaryKey(apply, inner, givenTableRepr))
            case inner: QueryExpr.QueryVariableReferenceLike.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
            case inner: QueryExpr.InputVariableReferenceLike.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
          }
        case RawQueryExpr.SelectNonPrimaryKey(apply, inner, givenTableRepr) =>
          parse(inner).flatMap {
            case inner: QueryExpr.QueryVariableReferenceLike.CanSelect    => ParseResult.Success(QueryExpr.QueryVariableReferenceLike.SelectNonPrimaryKey(apply, inner, givenTableRepr))
            case inner: QueryExpr.InputVariableReferenceLike.CanSelect    => ParseResult.Success(QueryExpr.InputVariableReferenceLike.SelectNonPrimaryKey(apply, inner, givenTableRepr))
            case inner: QueryExpr.QueryVariableReferenceLike.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
            case inner: QueryExpr.InputVariableReferenceLike.CanNotSelect => ParseResult.error(inner.fullTerm, "select not allowed")
          }
      }

  }

  /////// InputVariableReferenceLike ///////////////////////////////////////////////////////////////

  sealed trait InputVariableReferenceLike extends VariableReferenceLike, TermTransformer {

    def inTpe: TypeRepr
    def outTpe: TypeRepr
    override val queryRef: VariableReference.InputLike

    final def isOptional: Boolean = queryRef match
      case _: VariableReference.OptionalInputParam => true
      case _                                       => false

    override final def show(using Quotes): String = this match
      case QueryExpr.InputVariableReferenceLike.ReferencedVariable(_, queryRef)   => queryRef.show
      case QueryExpr.InputVariableReferenceLike.ProductFieldSelect(select, inner) => s"${inner.show}.${select.name.cyanFg}"
      case QueryExpr.InputVariableReferenceLike.SelectPrimaryKey(_, inner, _)     => s"${inner.show}.${"tablePK".hexFg("#35A7FF")}"
      case QueryExpr.InputVariableReferenceLike.SelectNonPrimaryKey(_, inner, _)  => s"${inner.show}.${"tableNPK".hexFg("#35A7FF")}"

  }
  object InputVariableReferenceLike extends Parser[RawQueryExpr.VariableReferenceLike, QueryExpr.InputVariableReferenceLike] {

    sealed trait CanSelect extends InputVariableReferenceLike
    sealed trait CanNotSelect extends InputVariableReferenceLike

    final case class ReferencedVariable(ident: Ident, queryRef: VariableReference.InputLike) extends CanSelect, TermTransformer.Defer {
      override val fullTerm: Term = ident
      override val rootIdent: Ident = ident
      override val inTpe: TypeRepr = queryRef.param.tpe.widen
      override val outTpe: TypeRepr = queryRef.param.tpe.widen
      override protected def defer: TermTransformer = queryRef
    }

    final case class ProductFieldSelect(select: Select, inner: CanSelect) extends CanSelect {
      override val fullTerm: Term = select
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: VariableReference.InputLike = inner.queryRef
      override val inTpe: TypeRepr = inner.inTpe
      override val outTpe: TypeRepr = select.tpe.widen
      override def simplified: TermTransformer.Simplified = inner >>> TermTransformer.FromSelect(select)
    }

    final case class SelectPrimaryKey(apply: Apply, inner: CanSelect, givenTableRepr: TypeclassExpr.TableRepr) extends CanNotSelect {
      override val fullTerm: Term = apply
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: VariableReference.InputLike = inner.queryRef

      private object doApply extends TermTransformer.Transform {
        override val inTpe: TypeRepr = inner.inTpe
        override val outTpe: TypeRepr = apply.tpe.widen
        override protected def convertTermInternal(term: Term)(using Quotes): Term = givenTableRepr.getPK(term)
      }

      override val inTpe: TypeRepr = doApply.inTpe
      override val outTpe: TypeRepr = doApply.outTpe

      override def simplified: TermTransformer.Simplified = inner >>> doApply
    }

    final case class SelectNonPrimaryKey(apply: Apply, inner: CanSelect, givenTableRepr: TypeclassExpr.TableRepr) extends CanNotSelect {
      override val fullTerm: Term = apply
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: VariableReference.InputLike = inner.queryRef

      private object doApply extends TermTransformer.Transform {
        override val inTpe: TypeRepr = inner.inTpe
        override val outTpe: TypeRepr = apply.tpe.widen
        override protected def convertTermInternal(term: Term)(using Quotes): Term = givenTableRepr.getNPK(term)
      }

      override val inTpe: TypeRepr = doApply.inTpe
      override val outTpe: TypeRepr = doApply.outTpe

      override def simplified: TermTransformer.Simplified = inner >>> doApply
    }

    override def parse(input: RawQueryExpr.VariableReferenceLike)(using ParseContext, Quotes): ParseResult[InputVariableReferenceLike] =
      VariableReferenceLike.parse(input).flatMap {
        case ref: InputVariableReferenceLike => ParseResult.success(ref)
        case ref: QueryVariableReferenceLike => ParseResult.error(ref.fullTerm, "Expected input reference at root, got query element")
      }

  }

  /////// QueryVariableReferenceLike ///////////////////////////////////////////////////////////////

  sealed trait QueryVariableReferenceLike extends VariableReferenceLike {

    def rowRepr(using Quotes): TypeclassExpr.RowRepr
    override val queryRef: VariableReference.QueryLike
    final lazy val isRoot: Boolean = queryRef.isRoot

    override final def show(using Quotes): String = this match
      case QueryExpr.QueryVariableReferenceLike.ReferencedVariable(_, varRef)          => varRef.show
      case QueryExpr.QueryVariableReferenceLike.ProductFieldSelect(select, inner)      => s"${inner.show}.${select.name.cyanFg}"
      case QueryExpr.QueryVariableReferenceLike.OptionGet(_, inner)                    => s"${inner.show}.${"get".blueFg}"
      case sel @ QueryExpr.QueryVariableReferenceLike.SelectPrimaryKey(_, inner, _)    => s"${inner.show}.${"tablePK".hexFg("#35A7FF")}(using ${sel.rowRepr.show})"
      case sel @ QueryExpr.QueryVariableReferenceLike.SelectNonPrimaryKey(_, inner, _) => s"${inner.show}.${"tableNPK".hexFg("#35A7FF")}(using ${sel.rowRepr.show})"

  }
  object QueryVariableReferenceLike extends Parser[RawQueryExpr.VariableReferenceLike, QueryExpr.QueryVariableReferenceLike] {

    sealed trait CanSelect extends QueryVariableReferenceLike
    sealed trait CanNotSelect extends QueryVariableReferenceLike

    final case class ReferencedVariable(ident: Ident, queryRef: VariableReference.QueryLike) extends CanSelect {
      override val fullTerm: Term = ident
      override val rootIdent: Ident = ident
      override def rowRepr(using Quotes): TypeclassExpr.RowRepr = queryRef.rowRepr
    }

    final case class ProductFieldSelect(select: Select, inner: CanSelect) extends CanSelect {
      override val fullTerm: Term = select
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: VariableReference.QueryLike = inner.queryRef
      override def rowRepr(using Quotes): TypeclassExpr.RowRepr = inner.rowRepr.productSchemaField(select, select.symbol.name)
    }

    final case class OptionGet(select: Select, inner: CanSelect) extends CanSelect {
      override val fullTerm: Term = select
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: VariableReference.QueryLike = inner.queryRef
      override def rowRepr(using Quotes): TypeclassExpr.RowRepr = inner.rowRepr.optionSchemaGet(select)
    }

    final case class SelectPrimaryKey(apply: Apply, inner: CanSelect, givenTableRepr: TypeclassExpr.TableRepr) extends CanNotSelect {
      override val fullTerm: Term = apply
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: VariableReference.QueryLike = inner.queryRef
      override def rowRepr(using Quotes): TypeclassExpr.RowRepr = givenTableRepr.pkRowRepr(fullTerm)
    }

    final case class SelectNonPrimaryKey(apply: Apply, inner: CanSelect, givenTableRepr: TypeclassExpr.TableRepr) extends CanNotSelect {
      override val fullTerm: Term = apply
      override val rootIdent: Ident = inner.rootIdent
      override val queryRef: VariableReference.QueryLike = inner.queryRef
      override def rowRepr(using Quotes): TypeclassExpr.RowRepr = givenTableRepr.npkRowRepr(fullTerm)
    }

    override def parse(input: RawQueryExpr.VariableReferenceLike)(using ParseContext, Quotes): ParseResult[QueryVariableReferenceLike] =
      VariableReferenceLike.parse(input).flatMap {
        case ref: QueryVariableReferenceLike => ParseResult.success(ref)
        case ref: InputVariableReferenceLike => ParseResult.error(ref.fullTerm, "Expected query element reference at root, got input")
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Binary
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Binary extends QueryExpr {

    val lhs: QueryExpr
    val rhs: QueryExpr
    override final def queryRefs: Growable[VariableReference] = lhs.queryRefs ++ rhs.queryRefs

    override final def show(using Quotes): String = this match
      case QueryExpr.BinaryAndOr(_, lhs, op, rhs) => s"${lhs.showWrapAndOr} ${op.show} ${rhs.showWrapAndOr}"
      case comp: QueryExpr.BinaryComp             => s"${comp.lhs.show} ${comp.op.show} ${comp.rhs.show}"

  }
  object Binary extends Parser[RawQueryExpr.Binary, QueryExpr.Binary] {

    override def parse(expr: RawQueryExpr.Binary)(using ParseContext, Quotes): ParseResult[QueryExpr.Binary] =
      expr match {
        case RawQueryExpr.Binary(term, lhs, op: BinOp.Comp, rhs) =>
          for {
            lhs <- QueryExpr.parse(lhs)
            rhs <- QueryExpr.parse(rhs)
          } yield QueryExpr.BinaryComp(term, lhs, op, rhs)
        case RawQueryExpr.Binary(term, lhs, op: BinOp.AndOr, rhs) =>
          for {
            lhs <- QueryExpr.parse(lhs)
            rhs <- QueryExpr.parse(rhs)
          } yield QueryExpr.BinaryAndOr(term, lhs, op, rhs)
      }

  }

  final case class BinaryAndOr(
      fullTerm: Term,
      lhs: QueryExpr,
      op: BinOp.AndOr,
      rhs: QueryExpr,
  ) extends Binary

  final case class BinaryComp(
      fullTerm: Term,
      lhs: QueryExpr,
      op: BinOp.Comp,
      rhs: QueryExpr,
  ) extends Binary

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      BuiltIn
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait BuiltIn extends QueryExpr {

    override final def show(using Quotes): String = this match
      case QueryExpr.CountWithArg(_, inner) => s"${"COUNT".cyanFg}( ${inner.show} )"
      case QueryExpr.Static(_, out, _)      => out.cyanFg.toString

  }

  final case class CountWithArg(fullTerm: Term, inner: QueryVariableReferenceLike) extends BuiltIn {
    override def queryRefs: Growable[VariableReference] = inner.queryRefs
  }

  final case class Static(fullTerm: Term, out: String, rowRepr: TypeclassExpr.RowRepr) extends BuiltIn {
    override def queryRefs: Growable[VariableReference] = Growable.empty
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Composite
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Composite extends QueryExpr {

    override final def show(using Quotes): String = this match
      case QueryExpr.InstantiateTable(_, gen, _, args)         => args.map(_.show).mkString(s"${gen.typeRepr.showCode}.apply(", ", ", ")")
      case QueryExpr.StringConcat(_, args)                     => args.map(_.show).mkString("CONCAT(", ", ", ")")
      case QueryExpr.OptionApply(_, inner)                     => s"Option(${inner.show})"
      case QueryExpr.OptionNullability(_, inner, showScala, _) => s"${inner.show}.${showScala.blueFg}"

  }

  final case class InstantiateTable(
      fullTerm: Term,
      gen: ProductGeneric[?],
      givenTableRepr: TypeclassExpr.TableRepr,
      args: List[QueryExpr],
  ) extends Composite {

    override def queryRefs: Growable[VariableReference] = Growable.many(args).flatMap(_.queryRefs)

    def cleanedArgs: List[QueryExpr] =
      args.flatMap {
        case it: InstantiateTable => it.cleanedArgs
        case a                    => a :: Nil
      }

  }

  final case class StringConcat(fullTerm: Term, args: List[QueryExpr]) extends Composite {
    override def queryRefs: Growable[VariableReference] = Growable.many(args).flatMap(_.queryRefs)
  }

  // TODO (KR) : split out cases for var refs
  final case class OptionApply(fullTerm: Term, inner: QueryExpr) extends Composite {
    override def queryRefs: Growable[VariableReference] = inner.queryRefs
  }

  final case class OptionNullability(select: Select, inner: QueryVariableReferenceLike, showScala: String, showSql: String) extends Composite {
    override val fullTerm: Term = select
    override def queryRefs: Growable[VariableReference] = inner.queryRefs
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Parse
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override def parse(expr: RawQueryExpr)(using ParseContext, Quotes): ParseResult[QueryExpr] =
    expr match {
      case expr: RawQueryExpr.VariableReferenceLike                            => VariableReferenceLike.parse(expr)
      case expr: RawQueryExpr.Binary                                           => Binary.parse(expr)
      case RawQueryExpr.InstantiateTable(fullTerm, gen, givenTableRepr, args)  => args.traverse(parse).map(QueryExpr.InstantiateTable(fullTerm, gen, givenTableRepr, _))
      case RawQueryExpr.RandomUUID(fullTerm)                                   => ParseResult.success(QueryExpr.Static(fullTerm, "gen_random_uuid()", TypeclassExpr.RowRepr.uuid))
      case RawQueryExpr.InstantNow(fullTerm)                                   => ParseResult.success(QueryExpr.Static(fullTerm, "NOW()", TypeclassExpr.RowRepr.instant))
      case RawQueryExpr.StringConcat(fullTerm, args)                           => args.traverse(parse(_)).map(StringConcat(fullTerm, _))
      case RawQueryExpr.OptionApply(fullTerm, inner)                           => QueryExpr.parse(inner).map(QueryExpr.OptionApply(fullTerm, _))
      case RawQueryExpr.ConstValue(fullTerm, constTerm)                        => ParseResult.Success(QueryExpr.ConstValue(fullTerm, constTerm))
      case RawQueryExpr.StaticCount(fullTerm, out)                             => ParseResult.Success(QueryExpr.Static(fullTerm, s"COUNT($out)", TypeclassExpr.RowRepr.long))
      case RawQueryExpr.OptionNullability(fullTerm, inner, showScala, showSql) => QueryExpr.QueryVariableReferenceLike.parse(inner).map(QueryExpr.OptionNullability(fullTerm, _, showScala, showSql))
      case RawQueryExpr.CountWithArg(fullTerm, inner)                          =>
        parse(inner).flatMap {
          case inner: QueryExpr.QueryVariableReferenceLike => ParseResult.Success(QueryExpr.CountWithArg(fullTerm, inner))
          case inner                                       => ParseResult.error(inner.fullTerm, "can only count( _ ) a unary query")
        }
    }

}
