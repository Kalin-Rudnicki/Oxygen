package oxygen.schema.compiled

import oxygen.json.JsonCodec
import oxygen.predef.core.*

sealed trait TypeIdentifier {

  val byName: TypeIdentifier.ByName

  override final lazy val hashCode: Int = byName.fullTypeName.hashCode()

  override final def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: TypeIdentifier => this.byName.fullTypeName == that.byName.fullTypeName
    case _                    => false

}
object TypeIdentifier {

  final case class Full private[schema] (schemaType: SchemaType, index: Option[Int]) extends TypeIdentifier derives JsonCodec {

    override val byName: TypeIdentifier.ByName = index match
      case Some(index) => ByName(s"${schemaType.fullTypeName}##$index")
      case None        => ByName(schemaType.fullTypeName)

    override def toString: String = byName.toString

  }
  object Full {
    given Ordering[Full] = Ordering.by[Full, SchemaType](_.schemaType).orElseBy(_.index)
  }

  // [[fullTypeName]] includes the index, if there are conflicts
  final case class ByName private[schema] (fullTypeName: String) extends TypeIdentifier {

    override val byName: ByName = this

    override def toString: String = fullTypeName

  }
  object ByName {
    given JsonCodec[ByName] = JsonCodec.deriveWrapped
  }

  final case class MapFunction(
      full: TypeIdentifier.Full => TypeIdentifier.Full,
      byName: TypeIdentifier.ByName => TypeIdentifier.ByName,
  ) {
    def apply(fullId: TypeIdentifier.Full): TypeIdentifier.Full = full(fullId)
    def apply(byNameId: TypeIdentifier.ByName): TypeIdentifier.ByName = byName(byNameId)
  }
  object MapFunction {

    val id: MapFunction = MapFunction(identity, identity)

    def deDuplicate(allRefs: Seq[TypeIdentifier.Full]): MapFunction = {
      val groupedBySchemaType: Seq[NonEmptyList[TypeIdentifier.Full]] = allRefs.toList.groupByNE(_.schemaType).toSeq.map { _._2.sorted }

      val fullMap: Map[TypeIdentifier.Full, TypeIdentifier.Full] =
        groupedBySchemaType.flatMap {
          case NonEmptyList(head, Nil) => (head, TypeIdentifier.Full(head.schemaType, None)) :: Nil
          case idents                  => idents.toList.zipWithIndex.map { case (ident, idx) => (ident, TypeIdentifier.Full(ident.schemaType, (idx + 1).some)) }
        }.toMap
      val byNameMap: Map[TypeIdentifier.ByName, TypeIdentifier.ByName] =
        fullMap.map { case (fullIdent, mappedFullIdent) => (fullIdent.byName, mappedFullIdent.byName) }

      MapFunction(
        full = ident => fullMap.getOrElse(ident, ident),
        byName = ident => byNameMap.getOrElse(ident, ident),
      )
    }

  }

}
