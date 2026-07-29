package oxygen.ui.web.style

import oxygen.schema.*
import oxygen.ui.web.style.OxygenColorSystem.Seeds

/**
  * First-party Oxygen theme packs — research-aligned:
  * neutrals first, one pop accent/primary, brand1 = solid shell (matches primary family).
  *
  * Graphite is the recommended default core (zinc canvas + blue pop).
  * Graphite-* siblings keep the same zinc greys and only recolor primary/accent/focus/brand1.
  * Aurora / Ember / Violet / Ocean remain distinct surface personalities.
  *
  * Runtime apply / persist lives in [[oxygen.ui.web.service.Theme]].
  */
object OxygenThemes {

  final case class Pack(
      id: String,
      name: String,
      blurb: String,
      dark: Seeds,
      light: Seeds,
  ) {
    def primarySwatch: String = dark.primary
    def accentSwatch: String = dark.accent
    def bgSwatch: String = dark.background
  }

  val storageKey: String = "oxygen.theme-pack"

  /**
    * Graphite family: fixed zinc neutrals + status; only the pop tokens change.
    * Dark canvas `#09090b` / light paper `#fafafa` — same as [[Seeds.oxygenDark]] / [[Seeds.oxygenLight]].
    */
  private def graphiteFamily(
      id: String,
      name: String,
      blurb: String,
      /** Dark mode: primary, accent, focus, brand1 (shell). */
      darkPop: (String, String, String, String),
      /** Light mode: primary, accent, focus, brand1. */
      lightPop: (String, String, String, String),
  ): Pack = {
    val (dPrimary, dAccent, dFocus, dBrand1) = darkPop
    val (lPrimary, lAccent, lFocus, lBrand1) = lightPop
    Pack(
      id = id,
      name = name,
      blurb = blurb,
      dark = Seeds.oxygenDark.copy(
        primary = dPrimary,
        accent = dAccent,
        focus = dFocus,
        brand1 = dBrand1,
      ),
      light = Seeds.oxygenLight.copy(
        primary = lPrimary,
        accent = lAccent,
        focus = lFocus,
        brand1 = lBrand1,
      ),
    )
  }

  /**
    * Graphite + pop (default). Zinc neutrals; blue primary that reads on dark/light;
    * brand1 = primary family for high-contrast top chrome.
    */
  val graphite: Pack =
    Pack(
      id = "graphite",
      name = "Graphite",
      blurb = "Zinc neutrals with a blue that pops. Recommended default core.",
      dark = Seeds.oxygenDark,
      light = Seeds.oxygenLight,
    )

  /** Graphite greys + emerald action. */
  val graphiteEmerald: Pack =
    graphiteFamily(
      id = "graphite-emerald",
      name = "Graphite Emerald",
      blurb = "Same zinc core; emerald primary for success-forward UIs.",
      darkPop = ("#34d399", "#2dd4bf", "#6ee7b7", "#059669"),
      lightPop = ("#059669", "#0d9488", "#047857", "#047857"),
    )

  /** Graphite greys + rose/coral action. */
  val graphiteRose: Pack =
    graphiteFamily(
      id = "graphite-rose",
      name = "Graphite Rose",
      blurb = "Same zinc core; rose primary for warm CTAs without warm neutrals.",
      darkPop = ("#fb7185", "#f472b6", "#fda4af", "#e11d48"),
      lightPop = ("#e11d48", "#db2777", "#be123c", "#be123c"),
    )

  /** Graphite greys + amber action. */
  val graphiteAmber: Pack =
    graphiteFamily(
      id = "graphite-amber",
      name = "Graphite Amber",
      blurb = "Same zinc core; amber primary that pops on dark and light.",
      darkPop = ("#fbbf24", "#f59e0b", "#fcd34d", "#d97706"),
      lightPop = ("#d97706", "#b45309", "#b45309", "#b45309"),
    )

  /** Graphite greys + indigo action (cooler than default blue). */
  val graphiteIndigo: Pack =
    graphiteFamily(
      id = "graphite-indigo",
      name = "Graphite Indigo",
      blurb = "Same zinc core; indigo primary for product/docs chrome.",
      darkPop = ("#818cf8", "#a78bfa", "#a5b4fc", "#4f46e5"),
      lightPop = ("#4f46e5", "#6366f1", "#4338ca", "#3730a3"),
    )

  /** Graphite greys + fuchsia action. */
  val graphiteFuchsia: Pack =
    graphiteFamily(
      id = "graphite-fuchsia",
      name = "Graphite Fuchsia",
      blurb = "Same zinc core; fuchsia primary for high-energy accents.",
      darkPop = ("#e879f9", "#c084fc", "#f0abfc", "#c026d3"),
      lightPop = ("#c026d3", "#a21caf", "#a21caf", "#86198f"),
    )

  /** Cool slate + cyan action. */
  val aurora: Pack =
    Pack(
      id = "aurora",
      name = "Aurora",
      blurb = "Deep slate canvas, sky-cyan primary for tech dashboards.",
      dark = Seeds(
        background = "#0a0f1a",
        foreground = "#f1f5f9",
        primary = "#22d3ee",
        danger = "#fb7185",
        success = "#4ade80",
        warning = "#fbbf24",
        focus = "#67e8f9",
        accent = "#38bdf8",
        brand1 = "#0891b2",
        brand2 = "#94a3b8",
      ),
      light = Seeds(
        background = "#f8fafc",
        foreground = "#0f172a",
        primary = "#0891b2",
        danger = "#e11d48",
        success = "#16a34a",
        warning = "#d97706",
        focus = "#0e7490",
        accent = "#0284c7",
        brand1 = "#0e7490",
        brand2 = "#64748b",
      ),
    )

  /** Warm paper + vivid coral CTA. */
  val ember: Pack =
    Pack(
      id = "ember",
      name = "Ember",
      blurb = "Warm charcoal/paper neutrals with coral CTAs.",
      dark = Seeds(
        background = "#12100e",
        foreground = "#faf7f5",
        primary = "#fb923c",
        danger = "#f87171",
        success = "#4ade80",
        warning = "#facc15",
        focus = "#fdba74",
        accent = "#f472b6",
        brand1 = "#ea580c",
        brand2 = "#a8a29e",
      ),
      light = Seeds(
        background = "#faf8f6",
        foreground = "#1c1917",
        primary = "#ea580c",
        danger = "#dc2626",
        success = "#16a34a",
        warning = "#ca8a04",
        focus = "#c2410c",
        accent = "#db2777",
        brand1 = "#c2410c",
        brand2 = "#78716c",
      ),
    )

  /** Violet action on near-black / lilac paper. */
  val violet: Pack =
    Pack(
      id = "violet",
      name = "Violet",
      blurb = "High-contrast ink with violet primary that pops.",
      dark = Seeds(
        background = "#0c0a12",
        foreground = "#faf5ff",
        primary = "#a78bfa",
        danger = "#fb7185",
        success = "#34d399",
        warning = "#fbbf24",
        focus = "#c4b5fd",
        accent = "#e879f9",
        brand1 = "#7c3aed",
        brand2 = "#a1a1aa",
      ),
      light = Seeds(
        background = "#faf8ff",
        foreground = "#1e1b2e",
        primary = "#7c3aed",
        danger = "#e11d48",
        success = "#059669",
        warning = "#d97706",
        focus = "#6d28d9",
        accent = "#c026d3",
        brand1 = "#6d28d9",
        brand2 = "#71717a",
      ),
    )

  /** Teal primary on navy / mint paper. */
  val ocean: Pack =
    Pack(
      id = "ocean",
      name = "Ocean",
      blurb = "Navy/mint surfaces with teal actions.",
      dark = Seeds(
        background = "#061016",
        foreground = "#ecfeff",
        primary = "#2dd4bf",
        danger = "#fb7185",
        success = "#4ade80",
        warning = "#fbbf24",
        focus = "#5eead4",
        accent = "#38bdf8",
        brand1 = "#0d9488",
        brand2 = "#94a3b8",
      ),
      light = Seeds(
        background = "#f0fdfa",
        foreground = "#042f2e",
        primary = "#0d9488",
        danger = "#e11d48",
        success = "#15803d",
        warning = "#b45309",
        focus = "#0f766e",
        accent = "#0284c7",
        brand1 = "#0f766e",
        brand2 = "#64748b",
      ),
    )

  /**
    * All packs. Order: Graphite family first (recommended core + colorways), then surface personalities.
    */
  val all: Seq[Pack] = Seq(
    graphite,
    graphiteEmerald,
    graphiteRose,
    graphiteAmber,
    graphiteIndigo,
    graphiteFuchsia,
    aurora,
    ember,
    violet,
    ocean,
  )

  /** Just the zinc-core family (for studio grouping / tests). */
  val graphiteFamilyPacks: Seq[Pack] =
    Seq(graphite, graphiteEmerald, graphiteRose, graphiteAmber, graphiteIndigo, graphiteFuchsia)

  val byId: Map[String, Pack] = all.map(p => p.id -> p).toMap

  val default: Pack = graphite

  def parse(id: String): Option[Pack] = byId.get(id.trim.toLowerCase)

  /** Wire format is pack id (same as localStorage). */
  given PlainTextSchema[Pack] = PlainTextSchema.string.transformOption(parse, _.id)

}
