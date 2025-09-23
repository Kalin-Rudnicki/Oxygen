package oxygen.quoted

import scala.quoted.*

trait HasTypeType {

  protected def showSelf: String
  protected def typeTypeInternal: Option[TypeType]

  object typeType {

    extension [A](self: Option[A])
      private def require(expType: String)(using Quotes): A =
        self.getOrElse { report.errorAndAbort(s"$showSelf is not a $expType") }

    def option: Option[TypeType] = typeTypeInternal
    def required(using Quotes): TypeType = option.require("product type / sum type")

    object product {
      def option: Option[TypeType.Case] = typeTypeInternal.flatMap(_.toCase)
      def required(using Quotes): TypeType.Case = option.require("product type")
    }

    object sum {
      def option: Option[TypeType.Sealed] = typeTypeInternal.flatMap(_.toSealed)
      def required(using Quotes): TypeType.Sealed = option.require("sum type")
    }

    object caseClass {
      def option: Option[TypeType.Case.Class] = typeTypeInternal.flatMap(_.toCaseClass)
      def required(using Quotes): TypeType.Case.Class = option.require("case class")
    }

    object caseObject {
      def option: Option[TypeType.Case.Object] = typeTypeInternal.flatMap(_.toCaseObject)
      def required(using Quotes): TypeType.Case.Object = option.require("case object")
    }

  }

}
