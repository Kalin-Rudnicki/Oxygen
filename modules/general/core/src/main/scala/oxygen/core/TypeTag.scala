package oxygen.core

import oxygen.core.TypeTag.TypeRef
import oxygen.core.collection.NonEmptyList
import oxygen.core.syntax.either.*
import oxygen.core.syntax.option.*
import scala.annotation.tailrec
import scala.quoted.*
import scala.reflect.ClassTag

// TODO (KR) : Support more types of types
trait TypeTag[A <: AnyKind] extends TypeTag.Showable {

  type RefT <: TypeTag.TypeRef

  val tag: RefT
  val closestClass: Class[?]

  def modifyTag[RefT2 <: TypeTag.TypeRef](f: RefT => RefT2): TypeTag.Aux[A, RefT2] = TypeTag(f(tag), closestClass)
  def withClosestClass(closestClass: Class[?]): TypeTag.Aux[A, RefT] = TypeTag(tag, closestClass)

  override def prefixAll: String = tag.polyShow(_.prefixAll)
  override def prefixAll(genericsString: TypeRef.Single => String): String = tag.polyShow(_.prefixAll(genericsString))
  override def prefixAllNoGenerics: String = tag.polyShow(_.prefixAllNoGenerics)

  override def prefixObject: String = tag.polyShow(_.prefixObject)
  override def prefixObject(genericsString: TypeRef.Single => String): String = tag.polyShow(_.prefixObject(genericsString))
  override def prefixObjectNoGenerics: String = tag.polyShow(_.prefixObjectNoGenerics)

  override def prefixNone: String = tag.polyShow(_.prefixNone)
  override def prefixNone(genericsString: TypeRef.Single => String): String = tag.polyShow(_.prefixNone(genericsString))
  override def prefixNoneNoGenerics: String = tag.polyShow(_.prefixNoneNoGenerics)

  override def canonicalName: String = tag.polyShow(_.canonicalName)
  override def canonicalName(genericsString: TypeRef.Single => String): String = tag.polyShow(_.canonicalName(genericsString))
  override def canonicalNameNoGenerics: String = tag.polyShow(_.canonicalNameNoGenerics)

  override def hashCode: Int = tag.hashCode
  override def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: TypeTag[?] => this.tag == that.tag
    case _                => false
  override def toString: String = tag.polyShow(_.prefixObject)

  final def applyHKTArgs(args: List[TypeTag.TypeRef]): TypeTag.TypeRef = this.tag match
    case hkt: TypeTag.TypeRef.HKT => hkt.make(args)
    case tag                      => tag

}
object TypeTag {

  private final case class Inst[A <: AnyKind, _RefT <: TypeRef](_tag: _RefT, _closestClass: Class[?]) extends TypeTag[A] {
    override type RefT = _RefT
    override val tag: _RefT = _tag
    override val closestClass: Class[?] = _closestClass
  }

  inline def apply[A <: AnyKind](using ev: TypeTag[A]): ev.type = ev
  inline def apply[A <: AnyKind, _RefT <: TypeRef](_tag: _RefT, _closestClass: Class[?]): TypeTag.Aux[A, _RefT] = Inst[A, _RefT](_tag, _closestClass)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Types
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  type Aux[A <: AnyKind, _RefT <: TypeRef] = TypeTag[A] { type RefT = _RefT }
  type Single[A <: AnyKind] = TypeTag.Aux[A, TypeRef.Single]

  enum PackagePrefix { case All, Object, None }

  sealed trait TypeRef {

    final def polyShow(f: TypeRef.Single => String): String = {
      def rec(self: TypeRef, depth: Int): String = (self, depth) match
        case (self: TypeRef.Single, _)       => f(self)
        case (self: TypeRef.Union, 0)        => self.cases.map(rec(_, depth + 1)).mkString(" | ")
        case (self: TypeRef.Intersection, 0) => self.cases.map(rec(_, depth + 1)).mkString(" & ")
        case (self: TypeRef.Union, _)        => self.cases.map(rec(_, depth + 1)).mkString("(", " | ", ")")
        case (self: TypeRef.Intersection, _) => self.cases.map(rec(_, depth + 1)).mkString("(", " & ", ")")
        case (self: TypeRef.HKT, _)          => s"{ [${self.paramNames.mkString(", ")}] =>> ${self.generic.polyShow(f)} }"
        case (TypeRef.Wildcard, _)           => "?"

      rec(this, 0)
    }

    final def containsTypeArg(that: TypeRef): Boolean = this match
      case TypeRef.Single(_, typeArgs, _) => typeArgs.contains(that) || typeArgs.exists(_.containsTypeArg(that))
      case _                              => false

    final def toSingle: Option[TypeRef.Single] = this match
      case self: TypeRef.Single => self.some
      case _                    => None

    final def toUnion: Option[TypeRef.Union] = this match
      case self: TypeRef.Union => self.some
      case _                   => None

    final def toIntersection: Option[TypeRef.Intersection] = this match
      case self: TypeRef.Intersection => self.some
      case _                          => None

  }
  object TypeRef {

    final case class Single(
        typeName: String,
        typeArgs: List[TypeRef],
        prefix: Either[TypeRef.Single, List[String]], // Either[ another type , package name ]
    ) extends TypeRef,
          Showable {

      lazy val reversePath: NonEmptyList[String] = prefix.fold(r => typeName :: r.reversePath, _ => NonEmptyList.one(typeName))
      lazy val packagePrefixes: List[String] = prefix.fold(_.packagePrefixes, identity)
      lazy val objectPrefixes: List[String] = reversePath.tail.reverse

      // =====| Show |=====

      private def genericsWith(f: TypeRef.Single => String): String =
        if typeArgs.isEmpty then ""
        else typeArgs.map(_.polyShow(f)).mkString("[", ", ", "]")

      override def prefixAll: String = prefixAll(_.prefixAll)
      override def prefixAll(genericsString: TypeRef.Single => String): String = prefix.fold(_.prefixAll(genericsString) + ".", _.map(s => s"$s.").mkString) + typeName + genericsWith(genericsString)
      override def prefixAllNoGenerics: String = prefix.fold(_.prefixAllNoGenerics + ".", _.map(s => s"$s.").mkString) + typeName

      override def prefixObject: String = prefixObject(_.prefixObject)
      override def prefixObject(genericsString: TypeRef.Single => String): String = prefix.fold(_.prefixObject(genericsString) + ".", _ => "") + typeName + genericsWith(genericsString)
      override def prefixObjectNoGenerics: String = prefix.fold(_.prefixObjectNoGenerics + ".", _ => "") + typeName

      override def prefixNone: String = prefixNone(_.prefixNone)
      override def prefixNone(genericsString: TypeRef.Single => String): String = typeName + genericsWith(genericsString)
      override def prefixNoneNoGenerics: String = typeName

      private def makePrefixString(parts: List[String]): Option[String] = Option.when(parts.nonEmpty)(parts.mkString("."))
      def optPackagePrefixString: Option[String] = makePrefixString(packagePrefixes)
      def optObjectPrefixString: Option[String] = makePrefixString(objectPrefixes)
      def optPrefixString: Option[String] = makePrefixString(packagePrefixes ::: objectPrefixes)

      def packagePrefixString: String = optPackagePrefixString.getOrElse("")
      def objectPrefixString: String = optObjectPrefixString.getOrElse("")
      def prefixString: String = optPrefixString.getOrElse("")

      override def canonicalName: String = canonicalNameNoGenerics + genericsWith(_.canonicalName)
      override def canonicalName(genericsString: TypeRef.Single => String): String = canonicalNameNoGenerics + genericsWith(genericsString)
      override def canonicalNameNoGenerics: String = s"${(packagePrefixes ::: objectPrefixes.map(s => s"$s$$")).map(s => s"$s.").mkString}$typeName"

      // =====| Convert |=====

      def withTypeArgs(typeArgs: List[TypeRef]): TypeRef.Single = copy(typeArgs = typeArgs)
      def withTypeArgs(typeArgs: TypeRef*): TypeRef.Single = copy(typeArgs = typeArgs.toList)

    }
    object Single {

      def make(packages: String*)(objects: String*)(typeName: String): TypeRef.Single = {
        @tailrec
        def loop(queue: NonEmptyList[String], prefix: Either[Single, List[String]]): Single = {
          val tmp = Single(queue.head, Nil, prefix)
          NonEmptyList.fromList(queue.tail) match {
            case Some(queueTail) => loop(queueTail, tmp.asLeft)
            case None            => tmp
          }
        }

        loop(NonEmptyList(typeName, objects.reverse.toList).reverse, packages.toList.asRight)
      }

      private val packagePrefixRegex = "^([^$.]+)\\.(.+)$".r
      private val objectPrefixRegex = "^([^$.]+)\\$\\.?(.+)$".r
      private val objectRegex = "^([^$.]+)\\$$".r
      def parse(name: String): TypeRef.Single = {
        @tailrec
        def loop(
            str: String,
            rPackages: List[String],
            rObjects: List[String],
        ): TypeRef.Single = str match
          case packagePrefixRegex(pkg, rest) => loop(rest, pkg :: rPackages, rObjects)
          case objectPrefixRegex(obj, rest)  => loop(rest, rPackages, obj :: rObjects)
          case objectRegex(typeName)         => TypeRef.Single.make(rPackages.reverse*)(rObjects.reverse*)(typeName)
          case typeName                      => TypeRef.Single.make(rPackages.reverse*)(rObjects.reverse*)(typeName)

        loop(name, Nil, Nil)
      }

    }

    final case class Union(cases: Set[TypeRef]) extends TypeRef

    final case class Intersection(cases: Set[TypeRef]) extends TypeRef

    final case class HKT(paramNames: List[String], make: List[TypeRef] => TypeRef) extends TypeRef, Showable {

      lazy val genericTypeParams: List[TypeRef.Single] = paramNames.map { p => TypeRef.Single(s"<$p>", Nil, Nil.asRight) }
      lazy val generic: TypeRef = make(genericTypeParams)

      override def prefixAll: String = generic.polyShow(_.prefixAll)
      override def prefixAll(genericsString: Single => String): String = generic.polyShow(_.prefixAll(genericsString))
      override def prefixAllNoGenerics: String = generic.polyShow(_.prefixAllNoGenerics)

      override def prefixObject: String = generic.polyShow(_.prefixObject)
      override def prefixObject(genericsString: Single => String): String = generic.polyShow(_.prefixObject(genericsString))
      override def prefixObjectNoGenerics: String = generic.polyShow(_.prefixObjectNoGenerics)

      override def prefixNone: String = generic.polyShow(_.prefixNone)
      override def prefixNone(genericsString: Single => String): String = generic.polyShow(_.prefixNone(genericsString))
      override def prefixNoneNoGenerics: String = generic.polyShow(_.prefixNoneNoGenerics)

      override def canonicalName: String = generic.polyShow(_.canonicalName)
      override def canonicalName(genericsString: Single => String): String = generic.polyShow(_.canonicalName(genericsString))
      override def canonicalNameNoGenerics: String = generic.polyShow(_.canonicalNameNoGenerics)

    }

    case object Wildcard extends TypeRef

  }

  trait Showable {

    final def prefixWithGenerics(p: TypeTag.PackagePrefix): String = p match
      case TypeTag.PackagePrefix.All    => prefixAll
      case TypeTag.PackagePrefix.Object => prefixObject
      case TypeTag.PackagePrefix.None   => prefixNone

    final def prefixNoGenerics(p: TypeTag.PackagePrefix): String = p match
      case TypeTag.PackagePrefix.All    => prefixAllNoGenerics
      case TypeTag.PackagePrefix.Object => prefixObjectNoGenerics
      case TypeTag.PackagePrefix.None   => prefixNoneNoGenerics

    final def prefix(generics: Boolean, p: TypeTag.PackagePrefix): String =
      if generics then prefixWithGenerics(p)
      else prefixNoGenerics(p)

    def prefixAll: String
    def prefixAll(genericsString: TypeRef.Single => String): String
    def prefixAllNoGenerics: String

    def prefixObject: String
    def prefixObject(genericsString: TypeRef.Single => String): String
    def prefixObjectNoGenerics: String

    def prefixNone: String
    def prefixNone(genericsString: TypeRef.Single => String): String
    def prefixNoneNoGenerics: String

    def canonicalName: String
    def canonicalName(genericsString: TypeRef.Single => String): String
    def canonicalNameNoGenerics: String

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def make[A](packagePrefix: String*)(objectPrefix: String*)(typeName: String): TypeTag.Single[A] =
    TypeTag(TypeRef.Single.make(packagePrefix*)(objectPrefix*)(typeName), classOf[Any])

  def fromName[A](name: String): TypeTag.Single[A] = TypeTag(TypeRef.Single.parse(name), classOf[Any])

  def fromClass[A](klass: Class[?]): TypeTag.Single[A] =
    TypeTag(
      TypeRef.Single.parse(klass.getName).withTypeArgs(klass.getTypeParameters.toList.map { a => TypeRef.Single(a.getName, Nil, Nil.asRight) }),
      klass,
    )
  def usingClassTag[A](using ct: ClassTag[A]): TypeTag.Single[A] =
    fromClass(ct.runtimeClass)

  private def derive2impl[A <: AnyKind: Type](using quotes: Quotes): Expr[TypeTag[A]] =
    oxygen.core.generic.DeriveTypeTag(quotes).summonOrDerive[A]

  inline given derived: [A <: AnyKind] => TypeTag[A] = ${ derive2impl[A] }

}
