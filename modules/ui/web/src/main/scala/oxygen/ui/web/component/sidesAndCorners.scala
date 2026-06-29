package oxygen.ui.web.component

///////  ///////////////////////////////////////////////////////////////

object SideType {
  sealed trait TopBottom { val topBottomValue: String }
  sealed trait LeftRight { val leftRightValue: String }

  sealed trait Top extends TopBottom { override final val topBottomValue: String = "top" }
  sealed trait Bottom extends TopBottom { override final val topBottomValue: String = "bottom" }
  sealed trait Left extends LeftRight { override final val leftRightValue: String = "left" }
  sealed trait Right extends LeftRight { override final val leftRightValue: String = "right" }
}

/**
  * def exampleTop(s: SideType.Top): Any = ...
  * def exampleLeft(s: SideType.Left): Any = ...
  *
  * exampleTop(Corner.TopLeft)
  * exampleTop(Corner.TopRight)
  * exampleLeft(Corner.TopLeft)
  * exampleLeft(Corner.BottomLeft)
  */
sealed trait Corner { self: SideType.TopBottom & SideType.LeftRight =>
  val topBottomValue: String
  val leftRightValue: String
  final lazy val cornerValue: String = s"$topBottomValue-$leftRightValue"
}
object Corner {
  case object TopLeft extends Corner, SideType.Top, SideType.Left
  case object TopRight extends Corner, SideType.Top, SideType.Right
  case object BottomLeft extends Corner, SideType.Bottom, SideType.Left
  case object BottomRight extends Corner, SideType.Bottom, SideType.Right
}

///////  ///////////////////////////////////////////////////////////////

sealed trait CornerType {
  val sideValue: String
}
object CornerType {
  sealed trait TopLeft extends CornerType
  sealed trait TopRight extends CornerType
  sealed trait BottomLeft extends CornerType
  sealed trait BottomRight extends CornerType
}

/**
  * def exampleTopLeft(s: CornerType.TopLeft): Any = ...
  * def exampleBottomLeft(s: CornerType.BottomLeft): Any = ...
  *
  * exampleTopLeft(Side.Top)
  * exampleTopLeft(Side.Left)
  * exampleBottomLeft(Side.Bottom)
  * exampleBottomLeft(Side.Left)
  */
sealed trait Side
object Side {
  case object Top extends Side, CornerType.TopLeft, CornerType.TopRight { override final val sideValue: String = "top" }
  case object Bottom extends Side, CornerType.BottomLeft, CornerType.BottomRight { override final val sideValue: String = "bottom" }
  case object Left extends Side, CornerType.TopLeft, CornerType.BottomLeft { override final val sideValue: String = "left" }
  case object Right extends Side, CornerType.TopRight, CornerType.BottomRight { override final val sideValue: String = "right" }
}
