package oxygen.ui.web.create

object ZIndices {

  val _1: Int = 10
  val _2: Int = 20
  val _3: Int = 30
  val _4: Int = 40
  val _5: Int = 50

  // OXY-152: dropdown / popup menus. Below modals so a modal always covers an open menu,
  // above normal page content. Panel sits one above its own outside-click scrim.
  val dropdownMenuScrim: Int = 8_000
  val dropdownMenuPanel: Int = 8_001

  val modalBehindPageMessages: Int = 9_000
  val pageMessages: Int = 10_000
  val modalInFrontOfPageMessages: Int = 11_000

  val max: Int = 100_000

}
