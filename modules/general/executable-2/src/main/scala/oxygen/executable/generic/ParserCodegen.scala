package oxygen.executable.generic

import oxygen.cli.*
import oxygen.executable.*
import oxygen.quoted.*
import scala.quoted.*

private[generic] object ParserCodegen {

  def subHelp(param: ParamRepr)(using Quotes): Expr[SubHelp] =
    param.docs match
      case Nil => '{ SubHelp.Empty }
      case ds  => '{ SubHelp.fromDocs(${ Expr(ds) }) }

  def docHelpParser(docs: List[String])(using Quotes): Expr[ArgsParser[?]] =
    '{ ArgsParser.helpOnly(Help.Extra(Help.Raw(${ Expr(docs.mkString("\n")) }))) }

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

  def withDefaultNamed[A: Type](
      parser: Expr[NamedArgsParser[A]],
      ownerName: String,
      param: ParamRepr,
      defaultSyms: Map[(String, Int), Term],
  )(using Quotes): Expr[NamedArgsParser[A]] =
    defaultSyms.get((ownerName, param.raw.paramIdx)) match
      case None      => parser
      case Some(rhs) => '{ $parser.withDefault(${ rhs.asExprOf[A] }) }

  def withDefaultPositional[A: Type](
      parser: Expr[PositionalArgsParser[A]],
      ownerName: String,
      param: ParamRepr,
      defaultSyms: Map[(String, Int), Term],
  )(using Quotes): Expr[PositionalArgsParser[A]] =
    defaultSyms.get((ownerName, param.raw.paramIdx)) match
      case None      => parser
      case Some(rhs) => '{ $parser.withDefault(${ rhs.asExprOf[A] }) }

  def buildParam(
      param: ParamRepr,
      ownerName: String,
      defaultSyms: Map[(String, Int), Term],
  )(using Quotes): Expr[ArgsParser[?]] = param match
    case p: ParamRepr.Positional =>
      p.raw.typeRepr.asTypeOf match
        case '[t] =>
          val builder = Implicits.searchRequired[PositionalArgsParser.Builder[t]](
            s"Missing PositionalArgsParser.Builder for ${p.raw.typeRepr.showAnsiCode}",
            p.raw.valPosition,
          )
          val base: Expr[PositionalArgsParser[t]] =
            '{
              $builder.build(
                ${ Expr(p.longName) },
                ${ subHelp(p) },
              )
            }
          withDefaultPositional(base, ownerName, p, defaultSyms)
        case _ => report.errorAndAbort(s"Unable to build positional param for ${p.raw.typeRepr.showAnsiCode}", p.raw.valPosition)
    case p: ParamRepr.Named =>
      p.raw.typeRepr.asTypeOf match
        case '[t] =>
          val builder = Implicits.searchRequired[NamedArgsParser.Builder[t]](
            s"Missing NamedArgsParser.Builder for ${p.raw.typeRepr.showAnsiCode}",
            p.raw.valPosition,
          )
          val base: Expr[NamedArgsParser[t]] =
            '{
              $builder.build(
                ${ Expr(p.longName) },
                ${ subHelp(p) },
                ${ shortNameExpr(p.shortName) },
              )
            }
          withDefaultNamed(base, ownerName, p, defaultSyms)
        case _ => report.errorAndAbort(s"Unable to build named param for ${p.raw.typeRepr.showAnsiCode}", p.raw.valPosition)
    case p: ParamRepr.Flag =>
      '{ NamedArgsParser.flag(${ Expr(p.longName) }, ${ Expr(p.absentValue) }, help = ${ subHelp(p) }) }
    case p: ParamRepr.Toggle =>
      '{ NamedArgsParser.toggle(${ toggleLongNamesExpr(p.longNames) }, ${ toggleShortNamesExpr(p.raw) }, ${ subHelp(p) }) }
    case p: ParamRepr.Custom =>
      p.make
    case p: ParamRepr.Config =>
      if p.optional then
        p.raw.typeRepr.dealias match
          case AppliedType(_, inner :: Nil) =>
            inner.asTypeOf match
              case '[t] =>
                '{
                  ArgsParser.configOptional[t](
                    ${ Expr(p.envVar) },
                    (raw: String) => ConfigLoader.loadDecoded[t](raw, ${ p.decoder.asExprOf[oxygen.json.JsonDecoder[t]] }),
                    ${ subHelp(p) },
                  )
                }
              case _ => report.errorAndAbort(s"Unable to build optional @config for ${p.raw.typeRepr.showAnsiCode}", p.raw.valPosition)
          case _ => report.errorAndAbort(s"Unable to build optional @config for ${p.raw.typeRepr.showAnsiCode}", p.raw.valPosition)
      else
        type T = p.raw.T
        '{
          ArgsParser.config[T](
            ${ Expr(p.envVar) },
            (raw: String) => ConfigLoader.loadDecoded[T](raw, ${ p.decoder.asExprOf[oxygen.json.JsonDecoder[T]] }),
            ${ subHelp(p) },
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
        case Some(d) if d.paramss.isEmpty                    => base.etaExpand(method.owner).asExpr
        case Some(d) if d.paramss.forall(_.params.isEmpty)   => base.appliedToNone.asExpr
        case _                                               => base.appliedToArgs(argExprs.map(_.toTerm)).asExpr
    else base.appliedToArgs(argExprs.map(_.toTerm)).asExpr

}