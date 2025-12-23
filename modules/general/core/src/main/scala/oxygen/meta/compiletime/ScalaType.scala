package oxygen.meta.compiletime

import scala.quoted.*

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      ScalaType
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait ScalaType
object ScalaType {

  def of[T <: AnyKind: Type](using Quotes): ScalaType = DeriveScalaType.of[T]

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      ClassType
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait ClassType extends ScalaType

sealed trait ProductType

sealed trait SumType

/////// Object ///////////////////////////////////////////////////////////////

sealed trait Object extends ClassType

trait StandardObject private[compiletime] () extends Object

trait CaseObject private[compiletime] () extends Object, ProductType
trait Scala2CaseObject private[compiletime] () extends CaseObject
trait EnumCaseObject private[compiletime] () extends CaseObject

/////// Class ///////////////////////////////////////////////////////////////

sealed trait Class extends ClassType

trait StandardClass private[compiletime] () extends Class
trait Scala2StandardClass private[compiletime] () extends StandardClass

trait SealedAbstractClass private[compiletime] () extends Class, SumType
trait Scala2SealedAbstractClass private[compiletime] () extends SealedAbstractClass
trait SealedEnum private[compiletime] () extends SealedAbstractClass

trait CaseClass private[compiletime] () extends Class, ProductType
trait Scala2CaseClass private[compiletime] () extends CaseClass
trait EnumCaseClass private[compiletime] () extends CaseClass

/////// Trait ///////////////////////////////////////////////////////////////

sealed trait Trait extends ClassType

trait StandardTrait private[compiletime] () extends Trait
trait Scala2StandardTrait private[compiletime] () extends StandardTrait

trait SealedTrait private[compiletime] () extends Trait, SumType
trait Scala2SealedTrait private[compiletime] () extends SealedTrait

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      AndOrType
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait AndOrType extends ScalaType

/////// UnionType ///////////////////////////////////////////////////////////////

trait UnionType private[compiletime] () extends AndOrType, SumType

/////// AndType ///////////////////////////////////////////////////////////////

trait IntersectionType private[compiletime] () extends AndOrType
