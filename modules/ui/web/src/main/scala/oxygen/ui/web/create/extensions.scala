package oxygen.ui.web.create

import oxygen.predef.core.*

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

extension [S[_], I](self: S[I])
  def foreachWidget[Env, Action, StateGet, StateSet <: StateGet](
      f: I => Widget.Polymorphic[Env, Action, StateGet, StateSet],
  )(using SeqRead[S]): Widget.Polymorphic[Env, Action, StateGet, StateSet] =
    Widget.foreach(self)(f)

extension [S[_], Env, Action, StateGet, StateSet <: StateGet](self: S[Widget.Polymorphic[Env, Action, StateGet, StateSet]])
  def toFragment(using SeqRead[S]): Widget.Polymorphic[Env, Action, StateGet, StateSet] =
    Widget.fragment(self)
