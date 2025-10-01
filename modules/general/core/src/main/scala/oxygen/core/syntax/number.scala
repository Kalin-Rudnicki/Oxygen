package oxygen.core.syntax

object number {

  extension (self: Long)
    def toStringCommas: String =
      self.toString.reverse.grouped(3).toSeq.reverse.map(_.reverse).mkString(",")

}
