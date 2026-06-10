package oxygen.executable.generic

import oxygen.cli.*
import oxygen.executable.*
import oxygen.quoted.*
import oxygen.schema.{JsonSchema, PlainTextSchema}
import scala.quoted.*

private[generic] object ParserCodegen {

  def unwrapOption(typeRepr: TypeRepr): Option[TypeRepr] = typeRepr.dealias match
    case AppliedType(constructor, arg :: Nil) if constructor.typeSymbol.fullName == "scala.Option" => Some(arg)
    case _                                                                                         => None

  def unwrapList(typeRepr: TypeRepr): Option[TypeRepr] = typeRepr.dealias match
    case AppliedType(constructor, arg :: Nil) if constructor.typeSymbol.fullName == "scala.collection.immutable.List" => Some(arg)
    case _                                                                                                            => None

  def unwrapNel(typeRepr: TypeRepr): Option[TypeRepr] = typeRepr.dealias match
    case AppliedType(constructor, arg :: Nil) if constructor.typeSymbol.fullName.endsWith(".NonEmptyList") => Some(arg)
    case _                                                                                               => None

  def unwrapTuple(typeRepr: TypeRepr): Option[List[TypeRepr]] = typeRepr.dealias match
    case AppliedType(constructor, elems) if constructor.typeSymbol.fullName.startsWith("scala.Tuple") && elems.nonEmpty => Some(elems)
    case _                                                                                                             => None

  def singleValueParser(
      name: String,
      elem: TypeRepr,
      param: ParamRepr,
  )(using Quotes): Expr[PositionalArgsParser[?]] =
    elem.asTypeOf match
      case '[t] =>
        val schema: Expr[PlainTextSchema[t]] =
          summonPlainTextSchema[t](report.errorAndAbort(s"Missing PlainTextSchema or JsonSchema for ${elem.showAnsiCode}", param.raw.valPosition))
        '{ PositionalArgsParser.single(${ Expr(name) }, ${ subHelp(param) })(using $schema) }
      case _ => report.errorAndAbort(s"Unable to build value parser for ${elem.showAnsiCode}", param.raw.valPosition)

  def combineTyped(
      left: Expr[PositionalArgsParser[?]],
      leftType: TypeRepr,
      right: Expr[PositionalArgsParser[?]],
      rightType: TypeRepr,
  )(using Quotes): (Expr[PositionalArgsParser[?]], TypeRepr) =
    (leftType.asTypeOf, rightType.asTypeOf) match
      case ('[lt], '[rt]) =>
        val l: Expr[PositionalArgsParser[lt]] = left.asExprOf[PositionalArgsParser[lt]]
        val r: Expr[PositionalArgsParser[rt]] = right.asExprOf[PositionalArgsParser[rt]]
        val combined: Expr[PositionalArgsParser[(lt, rt)]] = '{ $l ^>> $r }
        (combined, TypeRepr.of[(lt, rt)])
      case _ => report.errorAndAbort("Unable to combine positional parsers")

  def tupleFlattenMapper(size: Int)(using Quotes): Expr[Any => Any] =
    size match
      case 3 => '{ (v: Any) => val ((v0, v1), v2) = v.asInstanceOf[((Any, Any), Any)]; (v0, v1, v2) }
      case 4 => '{ (v: Any) => val (((v0, v1), v2), v3) = v.asInstanceOf[(((Any, Any), Any), Any)]; (v0, v1, v2, v3) }
      case n => '{ (v: Any) => oxygen.executable.generic.DeriveCliAppRuntime.flattenTuple(v, ${ Expr(n) }).asInstanceOf[Any] }

  def buildTupleParser(
      longName: String,
      elems: List[TypeRepr],
      param: ParamRepr,
  )(using Quotes): Expr[PositionalArgsParser[?]] =
    elems match
      case Nil => '{ PositionalArgsParser.unit }
      case one :: Nil =>
        val name = if elems.size == 1 then longName else s"$longName-0"
        singleValueParser(name, one, param)
      case _ :: _ =>
        val parsers: List[(Expr[PositionalArgsParser[?]], TypeRepr)] =
          elems.zipWithIndex.map { (elem, idx) =>
            val name = if elems.size == 1 then longName else s"$longName-$idx"
            (singleValueParser(name, elem, param), elem)
          }
        val (nested, _) = parsers.tail.foldLeft(parsers.head) { case ((acc, accType), (next, nextType)) =>
          combineTyped(acc, accType, next, nextType)
        }
        if elems.size <= 2 then nested
        else '{ $nested.map(${ tupleFlattenMapper(elems.size) }).asInstanceOf[PositionalArgsParser[Any]] }

  def buildValueParser(
      longName: String,
      typeRepr: TypeRepr,
      param: ParamRepr,
  )(using Quotes): Expr[PositionalArgsParser[?]] =
    unwrapList(typeRepr) match
      case Some(inner) =>
        inner.asTypeOf match
          case '[t] =>
            val schema: Expr[PlainTextSchema[t]] =
              summonPlainTextSchema[t](report.errorAndAbort(s"Missing PlainTextSchema or JsonSchema for ${inner.showAnsiCode}", param.raw.valPosition))
            val single: Expr[PositionalArgsParser[t]] =
              '{ PositionalArgsParser.single(${ Expr(longName) }, ${ subHelp(param) })(using $schema) }
            '{ $single.repeated }
          case _ => report.errorAndAbort(s"Unable to build list param for ${typeRepr.showAnsiCode}", param.raw.valPosition)
      case None =>
        unwrapNel(typeRepr) match
          case Some(inner) =>
            inner.asTypeOf match
              case '[t] =>
                val schema: Expr[PlainTextSchema[t]] =
                  summonPlainTextSchema[t](report.errorAndAbort(s"Missing PlainTextSchema or JsonSchema for ${inner.showAnsiCode}", param.raw.valPosition))
                val single: Expr[PositionalArgsParser[t]] =
                  '{ PositionalArgsParser.single(${ Expr(longName) }, ${ subHelp(param) })(using $schema) }
                '{ $single.repeatedNel }
              case _ => report.errorAndAbort(s"Unable to build nel param for ${typeRepr.showAnsiCode}", param.raw.valPosition)
          case None =>
            unwrapTuple(typeRepr) match
              case Some(elems) => buildTupleParser(longName, elems, param)
              case None =>
                typeRepr.asTypeOf match
                  case '[t] =>
                    val schema: Expr[PlainTextSchema[t]] =
                      summonPlainTextSchema[t](report.errorAndAbort(s"Missing PlainTextSchema or JsonSchema for ${typeRepr.showAnsiCode}", param.raw.valPosition))
                    '{ PositionalArgsParser.single(${ Expr(longName) }, ${ subHelp(param) })(using $schema) }
                  case _ => report.errorAndAbort(s"Unable to build value parser for ${typeRepr.showAnsiCode}", param.raw.valPosition)

  def summonPlainTextSchema[ParamT: Type](onMissing: => Nothing)(using Quotes): Expr[PlainTextSchema[ParamT]] =
    Implicits.searchOption[PlainTextSchema[ParamT]].getOrElse {
      Implicits.searchOption[JsonSchema[ParamT]] match
        case Some(jsonSchema) => '{ PlainTextSchema.jsonString(using $jsonSchema) }
        case None             => onMissing
    }

  def subHelp(param: ParamRepr)(using Quotes): Expr[SubHelp] =
    param.docs match
      case Nil => '{ SubHelp.Empty }
      case ds  => '{ SubHelp.fromDocs(${ Expr(ds) }) }

  def shortNameExpr(resolved: Defaultable.Opt[Char])(using Quotes): Expr[Option[Char]] = resolved match
    case Defaultable.Explicit(value) => Expr(value)
    case Defaultable.Default         => '{ None }

  def toggleShortNamesExpr(raw: RawParamRepr)(using Quotes): Expr[Option[(Char, Char)]] =
    (raw.annot_shortName_trueName, raw.annot_shortName_falseName) match
      case (Some(t), Some(f)) => '{ Some((${ Expr(t.name) }, ${ Expr(f.name) })) }
      case _                  => '{ None }

  def toggleLongNamesExpr(longNames: ToggleLongNameRepr)(using Quotes): Expr[ToggleLongNameRepr] = longNames match
    case ToggleLongNameRepr.PrefixTrue(truePrefix, base)       => '{ ToggleLongNameRepr.PrefixTrue(${ Expr(truePrefix) }, ${ Expr(base) }) }
    case ToggleLongNameRepr.PrefixFalse(falsePrefix, base)     => '{ ToggleLongNameRepr.PrefixFalse(${ Expr(falsePrefix) }, ${ Expr(base) }) }
    case ToggleLongNameRepr.PrefixBoth(truePrefix, falsePrefix, base) =>
      '{ ToggleLongNameRepr.PrefixBoth(${ Expr(truePrefix) }, ${ Expr(falsePrefix) }, ${ Expr(base) }) }
    case ToggleLongNameRepr.Explicit(trueName, falseName)      => '{ ToggleLongNameRepr.Explicit(${ Expr(trueName) }, ${ Expr(falseName) }) }

  def withDefault[A: Type](
      parser: Expr[NamedArgsParser[A]],
      typeRepr: TypeRepr,
      ownerName: String,
      param: ParamRepr,
      defaultSyms: Map[(String, Int), Term],
  )(using Quotes): Expr[NamedArgsParser[A]] =
    defaultSyms.get((ownerName, param.raw.paramIdx)) match
      case Some(rhs) => '{ $parser.withDefault(${ rhs.asExprOf[A] }) }
      case None =>
        unwrapList(typeRepr) match
          case Some(inner) =>
            inner.asTypeOf match
              case '[elemT] =>
                val empty: Expr[A] = '{ List.empty[elemT] }.asExprOf[A]
                '{ $parser.withDefault($empty) }
              case _ => parser
          case None => parser

  def withDefaultPositional[A: Type](
      parser: Expr[PositionalArgsParser[A]],
      typeRepr: TypeRepr,
      ownerName: String,
      param: ParamRepr,
      defaultSyms: Map[(String, Int), Term],
  )(using Quotes): Expr[PositionalArgsParser[A]] =
    defaultSyms.get((ownerName, param.raw.paramIdx)) match
      case Some(rhs) => '{ $parser.withDefault(${ rhs.asExprOf[A] }) }
      case None =>
        unwrapList(typeRepr) match
          case Some(inner) =>
            inner.asTypeOf match
              case '[elemT] =>
                val empty: Expr[A] = '{ List.empty[elemT] }.asExprOf[A]
                '{ $parser.withDefault($empty) }
              case _ => parser
          case None => parser

  def buildParam(
      param: ParamRepr,
      ownerName: String,
      defaultSyms: Map[(String, Int), Term],
  )(using Quotes): Expr[ArgsParser[?]] = param match
    case p: ParamRepr.Positional =>
      unwrapOption(p.raw.typeRepr) match
        case Some(inner) =>
          val base: Expr[PositionalArgsParser[?]] =
            buildValueParser(p.longName, inner, p)
          '{ $base.optional }
        case None =>
          p.raw.typeRepr.asTypeOf match
            case '[t] =>
              val valueParser: Expr[PositionalArgsParser[?]] =
                buildValueParser(p.longName, p.raw.typeRepr, p)
              val base: Expr[PositionalArgsParser[t]] =
                '{ $valueParser.asInstanceOf[PositionalArgsParser[t]] }
              withDefaultPositional(base, p.raw.typeRepr, ownerName, p, defaultSyms)
            case _ => report.errorAndAbort(s"Unable to build positional param for ${p.raw.typeRepr.showAnsiCode}", p.raw.valPosition)
    case p: ParamRepr.Named =>
      unwrapOption(p.raw.typeRepr) match
        case Some(inner) =>
          val valueParser: Expr[PositionalArgsParser[?]] =
            buildValueParser(p.longName, inner, p)
          '{ NamedArgsParser.named(${ Expr(p.longName) }, $valueParser, ${ shortNameExpr(p.shortName) }, ${ subHelp(p) }).optional }
        case None =>
          p.raw.typeRepr.asTypeOf match
            case '[t] =>
              val valueParser: Expr[PositionalArgsParser[?]] =
                buildValueParser(p.longName, p.raw.typeRepr, p)
              val base: Expr[NamedArgsParser[t]] =
                '{ NamedArgsParser.named(${ Expr(p.longName) }, $valueParser.asInstanceOf[PositionalArgsParser[t]], ${ shortNameExpr(p.shortName) }, ${ subHelp(p) }) }
              withDefault(base, p.raw.typeRepr, ownerName, p, defaultSyms)
            case _ => report.errorAndAbort(s"Unable to build named param for ${p.raw.typeRepr.showAnsiCode}", p.raw.valPosition)
    case p: ParamRepr.Flag =>
      '{ NamedArgsParser.flag(${ Expr(p.longName) }, ${ Expr(p.absentValue) }, help = ${ subHelp(p) }) }
    case p: ParamRepr.Toggle =>
      '{ NamedArgsParser.toggle(${ toggleLongNamesExpr(p.longNames) }, ${ toggleShortNamesExpr(p.raw) }, ${ subHelp(p) }) }
    case p: ParamRepr.Custom =>
      p.make
    case p: ParamRepr.Config =>
      type T = p.raw.T
      '{
        ArgsParser.config[T](
          ${ Expr(p.envVar) },
          (raw: String) => ConfigLoader.loadDecoded[T](raw, ${ p.decoder }),
        )
      }

  def combine(parsers: List[Expr[ArgsParser[?]]])(using Quotes): Expr[ArgsParser[?]] = parsers match
    case Nil         => '{ ArgsParser.unit }
    case head :: Nil => head
    case head :: tail =>
      tail.foldLeft(head) { (acc, next) => '{ $acc ^>> $next } }

  def combineHelp(parsers: List[Expr[ArgsParser[?]]])(using Quotes): Expr[ArgsParser[?]] = parsers match
    case Nil         => '{ ArgsParser.unit }
    case one :: Nil  => '{ ArgsParser.helpOnly($one.help) }
    case many        =>
      val helpExprs: List[Expr[Help]] = many.map(parser => '{ $parser.help })
      val combined: Expr[Help] = helpExprs.reduceLeft((left, right) => '{ Help.And($left, $right) })
      '{ ArgsParser.helpOnly($combined) }

  def buildParams(
      params: List[ParamRepr],
      ownerName: String,
      defaultSyms: Map[(String, Int), Term],
  )(using Quotes): Expr[ArgsParser[?]] =
    combine(params.map(buildParam(_, ownerName, defaultSyms)))

  def flattenTuple(parsed: Expr[?], count: Int)(using Quotes): List[Expr[?]] =
    if count <= 0 then Nil
    else if count == 1 then List(parsed)
    else
      val prev = '{ $parsed.asInstanceOf[scala.Tuple2[?, ?]]._1 }
      val last = '{ $parsed.asInstanceOf[scala.Tuple2[?, ?]]._2 }
      flattenTuple(prev, count - 1) :+ last

  def typedArgs(parsed: Expr[Any], typeReprs: List[TypeRepr])(using Quotes): List[Expr[?]] =
    val rawExprs: List[Expr[?]] = flattenTuple(parsed, typeReprs.size)
    rawExprs.zip(typeReprs).map { (expr, typeRepr) =>
      typeRepr.asTypeOf match
        case '[t] => '{ $expr.asInstanceOf[t] }
        case _    => expr
    }

  def callMethod(instance: Expr[?], method: oxygen.quoted.Symbol, argExprs: List[Expr[?]])(using Quotes): Expr[?] =
    val base = instance.toTerm.select(method)
    if argExprs.isEmpty then
      method.tree.narrowOpt[DefDef] match
        case Some(d) if d.paramss.isEmpty           => base.etaExpand(method.owner).asExpr
        case Some(d) if d.paramss.forall(_.params.isEmpty) => base.appliedToNone.asExpr
        case _                                      => base.appliedToArgs(argExprs.map(_.toTerm)).asExpr
    else base.appliedToArgs(argExprs.map(_.toTerm)).asExpr

}