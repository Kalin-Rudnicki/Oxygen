package oxygen.schema.compiled

import oxygen.json.*
import oxygen.predef.core.*
import scala.annotation.tailrec

@jsonDiscriminator("type")
sealed trait SchemaType derives JsonCodec {

  lazy val toTypeRef: TypeTag.TypeRef

  final lazy val showPrefixNone: String = toTypeRef.polyShow(_.prefixNone)
  final lazy val showPrefixObject: String = toTypeRef.polyShow(_.prefixObject)
  final lazy val showPrefixAll: String = toTypeRef.polyShow(_.prefixAll)

  final lazy val showPrefixNoneNoGenerics: String = toTypeRef.polyShow(_.prefixNoneNoGenerics)
  final lazy val showPrefixObjectNoGenerics: String = toTypeRef.polyShow(_.prefixObjectNoGenerics)
  final lazy val showPrefixAllNoGenerics: String = toTypeRef.polyShow(_.prefixAllNoGenerics)

  final def baseTypeName: String = showPrefixObjectNoGenerics
  final def simpleTypeName: String = showPrefixObject
  final def fullTypeName: String = showPrefixAll

  override final def toString: String = showPrefixAll

}
object SchemaType {

  final case class Concrete(
      typeName: String,
      typeArgs: Option[NonEmptyList[SchemaType]],
      parent: ConcreteParent,
  ) extends SchemaType derives JsonCodec {

    lazy val typeArgsList: List[SchemaType] = typeArgs.fold(Nil)(_.toList)

    @tailrec
    private def parentTypeRef(acc: Either[TypeTag.TypeRef.Single, List[String]], rObjects: List[String]): Either[TypeTag.TypeRef.Single, List[String]] =
      rObjects match
        case oHead :: oTail => parentTypeRef(TypeTag.TypeRef.Single(oHead, Nil, acc).asLeft, oTail)
        case Nil            => acc

    override lazy val toTypeRef: TypeTag.TypeRef.Single =
      TypeTag.TypeRef.Single(
        typeName,
        typeArgsList.map(_.toTypeRef),
        parent match {
          case SchemaType.ConcreteParent.Package(parentPackages, parentObjects) => parentTypeRef(parentPackages.asRight, parentObjects.fold(Nil)(_.toList))
          case SchemaType.ConcreteParent.Type(tpe)                              => tpe.toTypeRef.asLeft
        },
      )

    def toSimplifiedParent: ConcreteParent = parent.simplify match
      case _: ConcreteParent.Type                                      => ConcreteParent.Type(this)
      case ConcreteParent.Package(parentPackages, None)                => ConcreteParent.Package(parentPackages, NonEmptyList.one(typeName).some)
      case ConcreteParent.Package(parentPackages, Some(parentObjects)) => ConcreteParent.Package(parentPackages, (parentObjects :+ typeName).some)

  }

  @jsonDiscriminator("parentType")
  enum ConcreteParent derives JsonCodec {

    case Package(
        parentPackages: List[String],
        parentObjects: Option[NonEmptyList[String]],
    )

    case Type(`type`: Concrete)

    final def simplify: ConcreteParent = this match
      case pkg: ConcreteParent.Package => pkg
      case ConcreteParent.Type(tpe)    => tpe.toSimplifiedParent

  }

  final case class Union(cases: List[SchemaType]) extends SchemaType {
    override lazy val toTypeRef: TypeTag.TypeRef.Union = TypeTag.TypeRef.Union(cases.map(_.toTypeRef).toSet)
  }

  final case class Intersection(cases: List[SchemaType]) extends SchemaType {
    override lazy val toTypeRef: TypeTag.TypeRef.Intersection = TypeTag.TypeRef.Intersection(cases.map(_.toTypeRef).toSet)
  }

  case object Wildcard extends SchemaType {
    override lazy val toTypeRef: TypeTag.TypeRef.Wildcard.type = TypeTag.TypeRef.Wildcard
  }

  def fromTypeRef(ref: TypeTag.TypeRef.Single): SchemaType.Concrete =
    SchemaType.Concrete(
      ref.typeName,
      NonEmptyList.fromList(ref.typeArgs.map(fromTypeRef)),
      ref.prefix match
        case Left(prefix)  => fromTypeRef(prefix).toSimplifiedParent
        case Right(prefix) => ConcreteParent.Package(prefix, None),
    )

  def fromTypeRef(ref: TypeTag.TypeRef): SchemaType = ref match
    case ref: TypeTag.TypeRef.Single         => fromTypeRef(ref)
    case TypeTag.TypeRef.Union(cases)        => SchemaType.Union(cases.toList.map(fromTypeRef).sorted)
    case TypeTag.TypeRef.Intersection(cases) => SchemaType.Intersection(cases.toList.map(fromTypeRef).sorted)
    case hkt: TypeTag.TypeRef.HKT            => fromTypeRef(hkt.generic)
    case TypeTag.TypeRef.Wildcard            => SchemaType.Wildcard

  given Ordering[SchemaType] =
    Ordering
      .by[SchemaType, Int] {
        case _: Concrete     => 1
        case _: Union        => 2
        case _: Intersection => 3
        case Wildcard        => 4
      }
      .orElseBy(_.baseTypeName)
      .orElseBy(_.simpleTypeName)
      .orElseBy(_.fullTypeName)

}
