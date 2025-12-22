package oxygen.meta.k0

import oxygen.core.*
import oxygen.quoted.*
import scala.collection.immutable.ArraySeq
import scala.quoted.*

sealed trait IdentityGeneric[A] extends Generic.IdentityGeneric[A] { identityGeneric =>
  override final type ChildBound = A
  override type Child[B <: ChildBound] = IdentityChild[B]
  override final type SelfType[A2] = IdentityGeneric[A2]

  final class IdentityChild[B <: A] extends Entity.Child[A, B, A] {
    override type SelfType[A2 <: A] = IdentityChild[A2]
    override val idx: Int = 0
    override val childType: String = "identity"
    override val label: String = identityGeneric.label
    override val sym: Symbol = identityGeneric.sym
    override val typeRepr: TypeRepr = identityGeneric.typeRepr
    override def parentGeneric: IdentityGeneric[A] = identityGeneric
    override def pos: Position = identityGeneric.pos
    override def annotations(using Quotes): AnnotationsTyped[B] = AnnotationsTyped(identityGeneric.annotations.all, typeRepr.show)
    override def toIndentedString: IndentedString = s"IdentityGeneric[${typeRepr.showCode}]"
  }

  final val child: IdentityChild[A] = new IdentityChild[A]

  override final val children: ArraySeq[AnyChild] = ArraySeq(child)

  override def toIndentedString: IndentedString = child.toIndentedString

}
object IdentityGeneric {

  final case class Instance[A] private[k0] (
      label: String,
      sym: Symbol,
      typeRepr: TypeRepr,
  ) extends IdentityGeneric[A]

}
