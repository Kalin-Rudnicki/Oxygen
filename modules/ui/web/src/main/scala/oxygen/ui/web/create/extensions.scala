package oxygen.ui.web.create

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      CSS
//////////////////////////////////////////////////////////////////////////////////////////////////////

extension (self: Int)
  def px: String = s"${self}px"
  def ch: String = s"${self}ch"
  def vw: String = s"${self}vw"
  def vh: String = s"${self}vh"

extension (self: Double)
  def pct: String = s"$self%"
  def em: String = s"${self}em"
  def rem: String = s"${self}rem"

extension (self: String) def __(that: String): String = s"$self $that"

def css(values: String*): String = values.mkString(" ")
