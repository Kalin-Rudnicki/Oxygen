package oxygen.ui.web.create

/**
  * W7-T05 / W7-T06: motion tokens and CSS presets (not a physics engine).
  */
object Motion {

  object Duration {
    val instant: String = "0ms"
    val fast: String = "120ms"
    val normal: String = "200ms"
    val slow: String = "320ms"
  }

  object Easing {
    val standard: String = "cubic-bezier(0.2, 0, 0, 1)"
    val enter: String = "cubic-bezier(0, 0, 0.2, 1)"
    val exit: String = "cubic-bezier(0.4, 0, 1, 1)"
  }

  /** CSS custom properties + reduced-motion + fade/slide utility classes. */
  val sheet: StyleSheet =
    StyleSheet.makeConst("oxygen-motion")(
      s"""
         |:root {
         |  --oxy-motion-duration-fast: ${Duration.fast};
         |  --oxy-motion-duration-normal: ${Duration.normal};
         |  --oxy-motion-duration-slow: ${Duration.slow};
         |  --oxy-motion-easing-standard: ${Easing.standard};
         |  --oxy-motion-easing-enter: ${Easing.enter};
         |  --oxy-motion-easing-exit: ${Easing.exit};
         |}
         |
         |.oxy-fade-in {
         |  animation: oxy-fade-in var(--oxy-motion-duration-normal) var(--oxy-motion-easing-enter) both;
         |}
         |.oxy-slide-up {
         |  animation: oxy-slide-up var(--oxy-motion-duration-normal) var(--oxy-motion-easing-enter) both;
         |}
         |.oxy-spin {
         |  display: inline-flex;
         |  animation: oxy-spin 0.8s linear infinite;
         |}
         |.oxy-skeleton-pulse {
         |  animation: oxy-skeleton-pulse 1.4s ease-in-out infinite;
         |}
         |
         |@keyframes oxy-fade-in {
         |  from { opacity: 0; }
         |  to { opacity: 1; }
         |}
         |@keyframes oxy-slide-up {
         |  from { opacity: 0; transform: translateY(8px); }
         |  to { opacity: 1; transform: translateY(0); }
         |}
         |@keyframes oxy-spin {
         |  from { transform: rotate(0deg); }
         |  to { transform: rotate(360deg); }
         |}
         |@keyframes oxy-skeleton-pulse {
         |  0%, 100% { opacity: 0.55; }
         |  50% { opacity: 1; }
         |}
         |
         |@media (prefers-reduced-motion: reduce) {
         |  :root {
         |    --oxy-motion-duration-fast: 0ms;
         |    --oxy-motion-duration-normal: 0ms;
         |    --oxy-motion-duration-slow: 0ms;
         |  }
         |  .oxy-fade-in, .oxy-slide-up, .oxy-spin, .oxy-skeleton-pulse {
         |    animation: none !important;
         |  }
         |}
         |""".stripMargin,
    )

}
