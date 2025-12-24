package oxygen.quoted.node

import oxygen.quoted.companion.*
import scala.quoted.*

trait Printer[T] {
  def show(x: T): String
}
object Printer {

  def companion(using quotes: Quotes): PrinterCompanion = PrinterCompanion(using quotes)
  given Quotes => Conversion[Printer.type, PrinterCompanion] = _.companion

  /** Default pinter for `Tree` used when calling `tree.show` */
  given TreePrinter: Quotes => Printer[Tree] = Printer.TreeCode

  /** Default pinter for `TypeRepr` used when calling `tpe.show` */
  given TypeReprPrinter: Quotes => Printer[TypeRepr] = Printer.TypeReprCode

  /** Default pinter for `Constant` used when calling `const.show` */
  given ConstantPrinter: Quotes => Printer[Constant] = Printer.ConstantCode

}
