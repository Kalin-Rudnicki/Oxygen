package oxygen.core.typeclass

import oxygen.core.syntax.option.*
import scala.quoted.*

trait Lens[A, B] extends PartialLens[A, B] {

  def get(parent: A): B
  def replace(parent: A)(child: B): A
  def modify(parent: A)(child: B => B): A

  override def apply(parent: A): Lens.Applied[A, B] = Lens.Applied.Inst(this, parent)

  override final def getOption(parent: A): Option[B] = get(parent).some
  override final def replaceOption(parent: A)(child: B): Option[A] = replace(parent)(child).some
  override final def modifyOption(parent: A)(child: B => B): Option[A] = modify(parent)(child).some
  override final def flatModifyOption(parent: A)(child: B => Option[B]): Option[A] = child(get(parent)).map(replace(parent))

  final def >>>[C](that: Lens[B, C]): Lens[A, C] = Lens.AndThen(this, that)

}
object Lens {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def unsafe[A, B](underlying: PartialLens[A, B]): Lens[A, B] = UnsafePartial(underlying)

  def derived[A]: DeriveBuilder[A] = new DeriveBuilder[A]
  inline def deriveLens[A, B](inline f: A => B): Lens[A, B] = ${ derivedImpl('f) }

  final class DeriveBuilder[A]
  object DeriveBuilder {

    extension [A](@scala.annotation.unused inline self: DeriveBuilder[A])
      inline def apply[B](inline f: A => B): Lens[A, B] =
        ${ derivedImpl('f) }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Applied[A, B] extends PartialLens.Applied[A, B] {

    override protected val lens: Lens[A, B]

    final def get: B = lens.get(parent)
    final def replace(child: B): A = lens.replace(parent)(child)
    final def modify(child: B => B): A = lens.modify(parent)(child)

  }
  object Applied {
    final case class Inst[A, B](lens: Lens[A, B], parent: A) extends Applied[A, B]
  }

  final case class AndThen[A, B, C](a: Lens[A, B], b: Lens[B, C]) extends Lens[A, C] {
    override def get(parent: A): C = b.get(a.get(parent))
    override def replace(parent: A)(child: C): A = a.modify(parent)(b.replace(_)(child))
    override def modify(parent: A)(child: C => C): A = a.modify(parent)(b.modify(_)(child))
  }

  final case class UnsafePartial[A, B](underlying: PartialLens[A, B]) extends Lens[A, B] {
    override def get(parent: A): B =
      underlying.getOption(parent).getOrElse(throw new RuntimeException(s"Lens.UnsafePartial.get returned None for: $parent"))
    override def replace(parent: A)(child: B): A =
      underlying.replaceOption(parent)(child).getOrElse(throw new RuntimeException(s"Lens.UnsafePartial.replace returned None for: $parent"))
    override def modify(parent: A)(child: B => B): A =
      underlying.modifyOption(parent)(child).getOrElse(throw new RuntimeException(s"Lens.UnsafePartial.modify returned None for: $parent"))

  }

  final case class Impl[A, B](
      _get: A => B,
      _replace: (A, B) => A,
      _modify: (A, B => B) => A,
  ) extends Lens[A, B] {
    override def get(parent: A): B = _get(parent)
    override def replace(parent: A)(child: B): A = _replace(parent, child)
    override def modify(parent: A)(child: B => B): A = _modify(parent, child)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Macros
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def derivedImpl[A: Type, B: Type](fExpr: Expr[A => B])(using quotes: Quotes): Expr[Lens[A, B]] = {
    import quotes.reflect.*

    def invalidStructure(term: Tree): Nothing =
      report.errorAndAbort(s"invalid Lens derivation macro structure: ${term.show}")

    extension (self: Term)
      def removeInlines: Term = self match
        case Inlined(_, Nil, term) => term.removeInlines
        case _                     => self

    val defDef: DefDef =
      fExpr.asTerm.removeInlines match
        case Block((defDef: DefDef) :: Nil, Closure(Ident(n), _)) if defDef.name == n => defDef
        case _                                                                        => invalidStructure(fExpr.asTerm)

    val valDef: ValDef = defDef.paramss match
      case TermParamClause(valDef :: Nil) :: Nil => valDef
      case _                                     => invalidStructure(defDef)

    val rhs: Term = defDef.rhs.getOrElse(invalidStructure(defDef))

    @scala.annotation.nowarn // FIX-PRE-MERGE (KR) : remove
    def rec[_B: Type](term: Term): Option[Expr[Lens[A, _B]]] =
      term match {
        case select: Select =>
          type _A

          val inner: Term = select.qualifier
          val sym: Symbol = select.symbol
          val name: String = select.name
          val innerTpe: TypeRepr = inner.tpe.widen
          given Type[_A] = innerTpe.asType.asInstanceOf[Type[_A]]
          val innerClassSym: Symbol = innerTpe.classSymbol.getOrElse(invalidStructure(term))
          val copySym: Symbol = innerClassSym.declaredMethod("copy") match
            case copySym :: Nil => copySym
            case _              => invalidStructure(term)

          val lens: Expr[Lens[_A, _B]] =
            '{
              Lens.Impl[_A, _B](
                _get = a => ${ ('a).asTerm.select(sym).asExprOf[_B] },
                _replace = (a, b) => ${ ('a).asTerm.select(copySym).appliedTo(NamedArg(name, ('b).asTerm)).asExprOf[_A] },
                _modify = ???,
              )
            }

          rec[_A](inner) match {
            case Some(_innerLens) =>
              val innerLens: Expr[Lens[A, _A]] = _innerLens.asExprOf[Lens[A, _A]]
              Some('{ $innerLens >>> $lens })
            case None =>
              Some(lens.asExprOf[Lens[A, _B]])
          }
        case Ident(n) if n == valDef.name =>
          None
        case _ =>
          invalidStructure(term)
      }

    val res: Expr[Lens[A, B]] = rec[B](rhs).getOrElse(invalidStructure(defDef))

    report.errorAndAbort(res.show)

    res
  }

}
