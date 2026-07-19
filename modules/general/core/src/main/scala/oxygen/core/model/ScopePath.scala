package oxygen.core.model

import oxygen.core.{str, Text, TypeTag}
import oxygen.core.typeclass.Showable

sealed trait ScopePath extends Showable
object ScopePath {

  final case class Field(name: String) extends ScopePath {
    override def show: Text = str".$name"
  }

  final case class Index(index: Int) extends ScopePath {
    override def show: Text = str"($index)"
  }

  final case class SubType(typeName: String) extends ScopePath {
    override def show: Text = str".subType[$typeName]"
  }

  def subType(tt: TypeTag[?]): ScopePath.SubType = ScopePath.SubType(tt.prefixObject)

}
