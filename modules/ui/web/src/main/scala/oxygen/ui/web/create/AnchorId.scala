package oxygen.ui.web.create

/**
  * W7-T04: stable ids for in-page anchors (`#fragment` / `Window.scroll.toId`).
  *
  * Prefer explicit ids on sections:
  * {{{
  *   Section.level1.withId("billing")(children*)
  *   // or slug from a title:
  *   Section.level1.withId(AnchorId.slug("Billing details"))(...)
  * }}}
  * Raw HTML: `id := "my-section"` on any node.
  */
object AnchorId {

  /**
    * Lowercase slug suitable for `id` / URL fragment.
    * Non-alphanumeric runs become `-`; leading/trailing `-` stripped.
    */
  def slug(text: String): String = {
    val lowered = text.toLowerCase
    val buf = new StringBuilder(lowered.length)
    var lastDash = true // treat start as dash so we don't emit leading -
    var i = 0
    while i < lowered.length do {
      val c = lowered.charAt(i)
      if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then {
        buf.append(c)
        lastDash = false
      } else if !lastDash then {
        buf.append('-')
        lastDash = true
      }
      i += 1
    }
    val s = buf.result()
    if s.endsWith("-") then s.dropRight(1) else s
  }

  /** Strip a leading `#` so callers can pass `"#foo"` or `"foo"`. */
  def normalize(idOrHash: String): String = idOrHash.stripPrefix("#").trim

}
