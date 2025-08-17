package oxygen.core

import scala.quoted.*

final case class SourcePosition(
    fileName: String,
    filePath: String,
    start: Int,
    end: Int,
    startLine: Int,
    endLine: Int,
    startColumn: Int,
    endColumn: Int,
) {

  def estimatedScalaPath: String = filePath.split("/scala/").last
  def estimatedScalaFile: String = estimatedScalaPath.replace('/', '.')

  def simple: SourcePosition.Simple = SourcePosition.Simple(estimatedScalaFile, startLine, startColumn)

  override def toString: String = simple.toString

}
object SourcePosition {

  final case class Simple(file: String, line: Int, column: Int) {

    override def toString: String = s"$file@$line:$column"

  }

  private def derivedImpl(using quotes: Quotes): Expr[SourcePosition] = {
    import quotes.reflect.*

    val pos = Position.ofMacroExpansion
    val file = pos.sourceFile

    '{
      SourcePosition(
        fileName = ${ Expr(file.name) },
        filePath = ${ Expr(file.path) },
        start = ${ Expr(pos.start) },
        end = ${ Expr(pos.end) },
        startLine = ${ Expr(pos.startLine) },
        endLine = ${ Expr(pos.endLine) },
        startColumn = ${ Expr(pos.startColumn) },
        endColumn = ${ Expr(pos.endColumn) },
      )
    }
  }

  inline given derived: SourcePosition = ${ derivedImpl }

}
