package oxygen.ui.web.style

import java.time.YearMonth
import oxygen.predef.test.*
import oxygen.ui.web.{LockState, Memo, PageURL}
import oxygen.ui.web.component.{ColorModePicker, ColorPicker, DatePicker, DateTimePicker, Icon, InfiniteScroll, LazySection, Pagination, Progress, SideBar, SortableList, Tabs, TimePicker, TopBar}
import oxygen.ui.web.create.{AnchorId, CSSColor, MediaCSS, Motion}
import oxygen.ui.web.service.{ColorMode, IndexedDB, Intersect}
import oxygen.ui.web.service.HashScroll
import oxygen.ui.web.style.Breakpoints
import oxygen.ui.web.style.OxygenColorSystem.*
import zio.http.{Path, QueryParams}

/** Combined Scala.js suite (single main) for oxygen-ui-web pure logic. */
object OxygenColorSystemSpec extends OxygenSpecDefault {

  override def testSpec: TestSpec =
    suite("oxygen-ui-web")(
      suite("OxygenColorSystem")(
        test("generate dark palette keeps primary seed as standard") {
          // Graphite+pop default
          val p = generate(Seeds.oxygenDark, Mode.Dark)
          assertTrue(p.primary.standard.equalsIgnoreCase("#3b82f6")) &&
          assertTrue(p.bgBase.equalsIgnoreCase("#09090b")) &&
          assertTrue(p.fgDefault.equalsIgnoreCase("#fafafa"))
        },
        test("generate produces distinct hover/active from standard") {
          val p = generate(Seeds.oxygenDark, Mode.Dark)
          assertTrue(p.primary.hover != p.primary.standard) &&
          assertTrue(p.primary.active != p.primary.standard) &&
          assertTrue(p.primary.active != p.primary.hover)
        },
        test("withOverrides replaces a single token") {
          val p = generate(Seeds.oxygenDark, Mode.Dark)
            .withOverrides(Map("color.primary.standard" -> "#ff00aa"))
          assertTrue(p.primary.standard.equalsIgnoreCase("#ff00aa")) &&
          assertTrue(p.bgBase.equalsIgnoreCase("#09090b"))
        },
        test("light mode flips surface ladder direction (default not equal base)") {
          val dark = generate(Seeds.oxygenDark, Mode.Dark)
          val light = generate(Seeds.oxygenLight, Mode.Light)
          assertTrue(dark.bgDefault != dark.bgBase) &&
          assertTrue(light.bgDefault != light.bgBase) &&
          assertTrue(light.bgBase.equalsIgnoreCase("#fafafa"))
        },
        test("toStyleVars maps primary scale") {
          val vars = toStyleVars(generate(Seeds.oxygenDark, Mode.Dark))
          val primaryStd = vars.color.primary.standard
          val primaryHover = vars.color.primary.hover
          val spacingM = vars.spacing.m
          assertTrue(primaryStd.equalsIgnoreCase("#3b82f6")) &&
          assertTrue(primaryHover.nonEmpty) &&
          assertTrue(spacingM == "16px")
        },
        test("OxygenThemes exposes Graphite family + surface packs; Graphite is default") {
          import oxygen.ui.web.style.OxygenThemes
          val ids = OxygenThemes.all.map(_.id).toSet
          val graphiteFamily = Set(
            "graphite",
            "graphite-emerald",
            "graphite-rose",
            "graphite-amber",
            "graphite-indigo",
            "graphite-fuchsia",
          )
          val surfaces = Set("aurora", "ember", "violet", "ocean")
          assertTrue(OxygenThemes.all.size == 10) &&
          assertTrue(OxygenThemes.default.id == "graphite") &&
          assertTrue(ids == graphiteFamily ++ surfaces) &&
          assertTrue(OxygenThemes.graphiteFamilyPacks.map(_.id).toSet == graphiteFamily) &&
          // zinc core shared across Graphite family
          assertTrue(OxygenThemes.graphiteFamilyPacks.forall(_.dark.background.equalsIgnoreCase("#09090b"))) &&
          assertTrue(OxygenThemes.graphiteFamilyPacks.forall(_.light.background.equalsIgnoreCase("#fafafa"))) &&
          // pops differ from default blue
          assertTrue(OxygenThemes.graphiteEmerald.primarySwatch != OxygenThemes.graphite.primarySwatch) &&
          assertTrue(!OxygenThemes.all.exists(_.primarySwatch.equalsIgnoreCase("#00e285")))
        },
        test("contrastInk picks black on light fills and white on dark") {
          assertTrue(OxygenColorSystem.contrastInk(CSSColor.unsafeParse("#fbbf24")) == "#000000") && // amber
          assertTrue(OxygenColorSystem.contrastInk(CSSColor.unsafeParse("#2563eb")) == "#ffffff") && // blue
          assertTrue(OxygenColorSystem.contrastInk(CSSColor.unsafeParse("#ffffff")) == "#000000") &&
          assertTrue(OxygenColorSystem.contrastInk(CSSColor.unsafeParse("#09090b")) == "#ffffff")
        },
        test("roleScale generates on-fill for primary") {
          val p = generate(Seeds.oxygenDark, Mode.Dark)
          assertTrue(p.primary.on == "#ffffff" || p.primary.on == "#000000") &&
          assertTrue(p.primary.on.nonEmpty)
        },
        test("status seeds land on positive/negative/alert") {
          val p = generate(Seeds.oxygenDark, Mode.Dark)
          assertTrue(p.positive.standard.equalsIgnoreCase("#4ade80")) &&
          assertTrue(p.negative.standard.equalsIgnoreCase("#f87171")) &&
          assertTrue(p.alert.standard.equalsIgnoreCase("#fbbf24"))
        },
      ),
      suite("LockState")(
        test("empty is unlocked") {
          val s = LockState.empty
          assertTrue(!s.pageLocked) &&
          assertTrue(!s.regionLocked("form")) &&
          assertTrue(!s.anyLocked)
        },
        test("pageCount tracks page lock") {
          val s = LockState.empty.copy(pageCount = 2)
          assertTrue(s.pageLocked) &&
          assertTrue(s.regionLocked("anything"))
        },
        test("region nest counts") {
          val s = LockState.empty.copy(regions = Map("form" -> 2))
          assertTrue(s.regionLocked("form")) &&
          assertTrue(!s.regionLocked("other")) &&
          assertTrue(!s.pageLocked) &&
          assertTrue(s.anyLocked)
        },
        test("shouldDisable page vs region") {
          val page = LockState.empty.copy(pageCount = 1)
          val region = LockState.empty.copy(regions = Map("form" -> 1))
          assertTrue(page.shouldDisable(None)) &&
          assertTrue(page.shouldDisable(Some("form"))) &&
          assertTrue(!region.shouldDisable(None)) &&
          assertTrue(region.shouldDisable(Some("form"))) &&
          assertTrue(!region.shouldDisable(Some("other")))
        },
      ),
      suite("Breakpoints/MediaCSS")(
        test("breakpoint ordering") {
          assertTrue(Breakpoints.xs < Breakpoints.sm) &&
          assertTrue(Breakpoints.sm < Breakpoints.md) &&
          assertTrue(Breakpoints.md < Breakpoints.lg) &&
          assertTrue(Breakpoints.lg < Breakpoints.xl)
        },
        test("MediaCSS.block emits @media wrapper") {
          val css = MediaCSS.mdUp(".x { display: none; }")
          assertTrue(css.contains("@media (min-width: 768px)")) &&
          assertTrue(css.contains(".x { display: none; }"))
        },
      ),
      suite("Memo")(
        test("same key hits cache") {
          val m = Memo[Int, String]()
          var builds = 0
          def b(k: Int) = { builds += 1; s"v$k" }
          val a = m(1)(b)
          val c = m(1)(b)
          assertTrue(a == "v1") &&
          assertTrue(c == "v1") &&
          assertTrue(builds == 1) &&
          assertTrue(m.hitCount == 1) &&
          assertTrue(m.missCount == 1)
        },
        test("key change rebuilds") {
          val m = Memo[Int, String]()
          var builds = 0
          def b(k: Int) = { builds += 1; s"v$k" }
          m(1)(b)
          val v = m(2)(b)
          assertTrue(v == "v2") &&
          assertTrue(builds == 2)
        },
      ),
      suite("Pagination.State")(
        test("slice and pageCount") {
          val p = Pagination.State(page = 1, pageSize = 10, total = 25)
          assertTrue(p.pageCount == 3) &&
          assertTrue(p.offset == 10) &&
          assertTrue(p.slice((0 until 25).toList) == (10 until 20).toList)
        },
        test("next/prev clamp") {
          val p0 = Pagination.State.initial(10, 25)
          assertTrue(!p0.canPrev) &&
          assertTrue(p0.canNext) &&
          assertTrue(p0.next.page == 1) &&
          assertTrue(p0.next.next.next.page == 2) &&
          assertTrue(!p0.next.next.next.canNext)
        },
      ),
      suite("PageURL fragment")(
        test("formatted round-trip includes hash") {
          val u = PageURL(Path("/page/home"), QueryParams.empty, Some("section-2"))
          assertTrue(u.formatted.endsWith("#section-2")) &&
          assertTrue(u.withFragment("#other").fragment == Some("other")) &&
          assertTrue(u.clearFragment.fragment.isEmpty)
        },
        test("empty fragment omits hash") {
          val u = PageURL(Path("/x"), QueryParams.empty, None)
          assertTrue(!u.formatted.contains("#"))
        },
      ),
      suite("Motion tokens")(
        test("sheet emits duration/easing vars and reduced-motion") {
          val css = Motion.sheet.innerHTML
          assertTrue(css.contains("--oxy-motion-duration-normal")) &&
          assertTrue(css.contains("--oxy-motion-easing-enter")) &&
          assertTrue(css.contains("prefers-reduced-motion")) &&
          assertTrue(css.contains(".oxy-fade-in")) &&
          assertTrue(css.contains(".oxy-slide-up")) &&
          assertTrue(Motion.Duration.fast == "120ms")
        },
      ),
      suite("HashScroll")(
        test("empty fragment is a no-op (false)") {
          HashScroll.toFragment("").map(r => assertTrue(!r))
        },
      ),
      suite("AnchorId")(
        test("slug normalizes titles") {
          assertTrue(AnchorId.slug("Billing details") == "billing-details") &&
          assertTrue(AnchorId.slug("  Hello!!! World  ") == "hello-world") &&
          assertTrue(AnchorId.normalize("#foo") == "foo")
        },
      ),
      suite("InfiniteScroll")(
        test("load append end fail retry") {
          val s0 = InfiniteScroll.State.empty[Int]
          val loading = s0.beginLoad
          val mid = loading.append(Seq(1, 2), hasMore = true)
          val end = mid.beginLoad.append(Seq(3), hasMore = false)
          val failed = mid.beginLoad.fail("network")
          val retried = failed.retry
          assertTrue(s0.canLoadMore) &&
          assertTrue(loading.isLoading) &&
          assertTrue(mid.items == Vector(1, 2) && mid.canLoadMore && mid.nextPage == 1) &&
          assertTrue(end.isEnd && end.items == Vector(1, 2, 3)) &&
          assertTrue(failed.isFailed && failed.failureMessage.contains("network")) &&
          assertTrue(retried.canLoadMore)
        },
      ),
      suite("LazySection")(
        test("toggle open state") {
          val s = LazySection.State()
          assertTrue(!s.open) &&
          assertTrue(s.toggle.open) &&
          assertTrue(s.expand.open) &&
          assertTrue(s.collapse.open == false)
        },
      ),
      suite("Icon catalog")(
        test("substantial unique names and byName") {
          val names = Icon.all.map(_.name)
          assertTrue(Icon.all.size >= 80) &&
          assertTrue(names.distinct.size == names.size) &&
          assertTrue(Icon.byName.get("search").exists(_.name == "search")) &&
          assertTrue(Icon.custom("M0 0").name == "custom") &&
          assertTrue(Icon.home.sm.sizePx == Icon.Size.sm)
        },
      ),
      suite("Tabs/Progress")(
        test("Tabs.State select by id") {
          assertTrue(Tabs.State.initial("gen").select("bill").selected == "bill") &&
          assertTrue(Tabs.State.empty.selected == "")
        },
        test("Progress.clampFraction drives fill bounds") {
          assertTrue(Progress.clampFraction(-1) == 0.0) &&
          assertTrue(Progress.clampFraction(2) == 1.0) &&
          assertTrue(Progress.clampFraction(0.5) == 0.5)
        },
      ),
      suite("TimePicker")(
        test("12h face and AM/PM round-trip") {
          val threePm = TimePicker.State(15, 30)
          assertTrue(threePm.hour12 == 3) &&
          assertTrue(threePm.isPm) &&
          assertTrue(threePm.toggleAmPm.hour == 3) &&
          assertTrue(TimePicker.State(0, 0).hour12 == 12) &&
          assertTrue(!TimePicker.State(0, 0).isPm) &&
          assertTrue(TimePicker.State(12, 0).hour12 == 12) &&
          assertTrue(TimePicker.State(12, 0).isPm) &&
          assertTrue(TimePicker.State.midnight.withHour12(3, pm = true).hour == 15) &&
          assertTrue(TimePicker.State.format(threePm, TimePicker.HourMode.H12).contains("PM"))
        },
      ),
      suite("DatePicker")(
        test("monthCells Sun-first full weeks for known month") {
          // 2024-01-01 was Monday → one leading Sunday pad
          val jan = DatePicker.monthCells(YearMonth.of(2024, 1))
          assertTrue(jan.flatten.size == 31) &&
          assertTrue(jan.size % 7 == 0) &&
          assertTrue(jan.head.isEmpty) &&
          assertTrue(jan(1).contains(java.time.LocalDate.of(2024, 1, 1)))
        },
        test("State select updates cursor") {
          val d = java.time.LocalDate.of(2023, 6, 15)
          val s = DatePicker.State.empty().select(d)
          assertTrue(s.selected.contains(d)) &&
          assertTrue(s.cursor == YearMonth.of(2023, 6))
        },
        test("prevYear/nextYear and year grid page") {
          val s = DatePicker.State.of(java.time.LocalDate.of(2024, 6, 15))
          assertTrue(s.prevYear.cursor.getYear == 2023) &&
          assertTrue(s.nextYear.cursor.getYear == 2025) &&
          assertTrue(s.prevYear.cursor.getMonthValue == 6) &&
          assertTrue(s.showYearGrid.view == DatePicker.View.YearGrid) &&
          assertTrue(s.pickYear(2019).cursor.getYear == 2019) &&
          assertTrue(s.pickYear(2019).view == DatePicker.View.Calendar)
        },
        test("yearPage anchors to 12-year blocks") {
          assertTrue(DatePicker.yearPage(2024) == (2016 to 2027).toVector) &&
          assertTrue(DatePicker.yearPage(2016).head == 2016) &&
          assertTrue(DatePicker.yearPage(2015).last == 2015)
        },
      ),
      suite("TimePicker")(
        test("parse and format round-trip") {
          val s = TimePicker.State.parse("09:30").get
          assertTrue(s.hour == 9 && s.minute == 30) &&
          assertTrue(TimePicker.State.format(s) == "09:30") &&
          assertTrue(TimePicker.State.parse("25:00").isEmpty)
        },
        test("bumpHour/bumpMinute wrap") {
          assertTrue(TimePicker.State(23, 0).bumpHour(1).hour == 0) &&
          assertTrue(TimePicker.State(0, 0).bumpHour(-1).hour == 23) &&
          assertTrue(TimePicker.State(12, 59).bumpMinute(1).minute == 0) &&
          assertTrue(TimePicker.State(12, 0).bumpMinute(-1).minute == 59)
        },
      ),
      suite("ColorPicker")(
        test("normalizeHex expands short form") {
          assertTrue(ColorPicker.normalizeHex("#0f8").contains("#00ff88")) &&
          assertTrue(ColorPicker.normalizeHex("AABBCC").contains("#aabbcc")) &&
          assertTrue(ColorPicker.normalizeHex("nope").isEmpty)
        },
      ),
      suite("DateTimePicker")(
        test("compose toLocalDateTime when date set") {
          val d = java.time.LocalDate.of(2024, 3, 1)
          val s = DateTimePicker.State.empty.withDate(d).withTime(java.time.LocalTime.of(14, 0))
          assertTrue(s.toLocalDateTime.contains(java.time.LocalDateTime.of(2024, 3, 1, 14, 0)))
        },
      ),
      suite("SortableList")(
        test("reorder pure exact order (onto + between)") {
          assertTrue(SortableList.reorder(Vector("a", "b", "c"), 0, 2) == Vector("b", "c", "a")) &&
          assertTrue(SortableList.reorder(Vector(1, 2, 3), 2, 0) == Vector(3, 1, 2)) &&
          assertTrue(SortableList.reorder(Vector(1, 2, 3), 1, 1) == Vector(1, 2, 3)) &&
          // between: move index 0 into slot after last (3) on [a,b,c] → [b,c,a]
          assertTrue(SortableList.reorderToSlot(Vector("a", "b", "c"), 0, 3) == Vector("b", "c", "a")) &&
          // between: already in gap → no-op
          assertTrue(SortableList.reorderToSlot(Vector("a", "b", "c"), 1, 1) == Vector("a", "b", "c")) &&
          assertTrue(SortableList.reorderToSlot(Vector("a", "b", "c"), 1, 2) == Vector("a", "b", "c"))
        },
      ),
      suite("Intersect")(
        test("sentinel id matches InfiniteScroll footer") {
          assertTrue(Intersect.infiniteScrollSentinelId == "oxy-infinite-scroll-sentinel")
        },
      ),
      suite("Idb")(
        test("maxVersion from migrations") {
          val ms = Seq(IndexedDB.Migration(1, Nil), IndexedDB.Migration(3, Seq(IndexedDB.StoreSpec("kv"))))
          assertTrue(IndexedDB.maxVersion(ms) == 3) &&
          assertTrue(IndexedDB.maxVersion(Nil) == 1)
        },
      ),
      suite("Shell chrome (W2-T13)")(
        test("TopBar.Cache and SideBar.Cache use token defaults") {
          val tb = TopBar.Cache.default
          val sb = SideBar.Cache.default
          // Height is optional (None by default so HolyGrail can own row height).
          // CSS var names (string form of S.color.*) must look like var(...) or be non-empty tokens
          assertTrue(tb.bg.nonEmpty && tb.itemHover.nonEmpty && tb.height.isEmpty) &&
          assertTrue(sb.bg.nonEmpty) &&
          assertTrue(!tb.bg.contains("#") || tb.bg.startsWith("var(") || tb.bg.contains("--"))
        },
      ),
      suite("Contrast (W1-T09)")(
        test("black on white meets AA normal") {
          val r = Contrast.ratio("#000000", "#ffffff").get
          assertTrue(r > 20.0) &&
          assertTrue(Contrast.meetsAaNormal("#000000", "#ffffff"))
        },
        test("identical colors are too close and fail AA") {
          assertTrue(Contrast.tooClose("#101010", "#101010")) &&
          assertTrue(!Contrast.meetsAaNormal("#101010", "#101010"))
        },
        test("default dark seeds fg/bg report via seedWarnings") {
          // drives real OxygenColorSystem.Seeds; report is optional (may be empty if ok)
          val w = Contrast.seedWarnings(OxygenColorSystem.Seeds.oxygenDark)
          val fgBg = Contrast.ratio(
            OxygenColorSystem.Seeds.oxygenDark.foreground,
            OxygenColorSystem.Seeds.oxygenDark.background,
          )
          assertTrue(fgBg.exists(_ >= Contrast.aaLarge)) &&
          assertTrue(w.forall(_.ratio > 0))
        },
      ),
      suite("ColorModePicker.Logic")(
        test("options include/exclude System") {
          import ColorMode.Mode.*
          assertTrue(ColorModePicker.Logic.options(true) == List(Light, Dark, System)) &&
          assertTrue(ColorModePicker.Logic.options(false) == List(Light, Dark))
        },
        test("iconFor maps each mode to a distinct icon") {
          import ColorMode.Mode.*
          val names = Set(Light, Dark, System).map(ColorModePicker.Logic.iconFor(_).name)
          assertTrue(names.size == 3) &&
          assertTrue(ColorModePicker.Logic.iconFor(Light).name == "sun") &&
          assertTrue(ColorModePicker.Logic.iconFor(Dark).name == "moon") &&
          assertTrue(ColorModePicker.Logic.iconFor(System).name == "desktop")
        },
        test("effectiveCurrent clamps a stored mode not in options") {
          import ColorMode.Mode.*
          val ld = ColorModePicker.Logic.options(false)
          assertTrue(ColorModePicker.Logic.effectiveCurrent(ld, Dark) == Dark) &&
          assertTrue(ColorModePicker.Logic.effectiveCurrent(ld, System) == Light) &&
          assertTrue(ColorModePicker.Logic.effectiveCurrent(Nil, System) == System)
        },
        test("keyToNav maps arrows/home/end/space/enter, ignores others") {
          import ColorModePicker.Nav.*
          assertTrue(ColorModePicker.Logic.keyToNav("ArrowLeft").contains(Prev)) &&
          assertTrue(ColorModePicker.Logic.keyToNav("Up").contains(Prev)) &&
          assertTrue(ColorModePicker.Logic.keyToNav("ArrowRight").contains(Next)) &&
          assertTrue(ColorModePicker.Logic.keyToNav("Down").contains(Next)) &&
          assertTrue(ColorModePicker.Logic.keyToNav("Home").contains(First)) &&
          assertTrue(ColorModePicker.Logic.keyToNav("End").contains(Last)) &&
          assertTrue(ColorModePicker.Logic.keyToNav(" ").contains(Select)) &&
          assertTrue(ColorModePicker.Logic.keyToNav("Enter").contains(Select)) &&
          assertTrue(ColorModePicker.Logic.keyToNav("a").isEmpty)
        },
        test("resolveNav wraps Prev/Next and honors First/Last/Select") {
          import ColorMode.Mode.*
          import ColorModePicker.Nav.*
          val opts = ColorModePicker.Logic.options(true)
          assertTrue(ColorModePicker.Logic.resolveNav(opts, Light, Prev) == System) &&
          assertTrue(ColorModePicker.Logic.resolveNav(opts, System, Next) == Light) &&
          assertTrue(ColorModePicker.Logic.resolveNav(opts, Dark, Next) == System) &&
          assertTrue(ColorModePicker.Logic.resolveNav(opts, Dark, First) == Light) &&
          assertTrue(ColorModePicker.Logic.resolveNav(opts, Dark, Last) == System) &&
          assertTrue(ColorModePicker.Logic.resolveNav(opts, Dark, Select) == Dark)
        },
        test("cycle advances with wraparound") {
          import ColorMode.Mode.*
          val opts = ColorModePicker.Logic.options(true)
          assertTrue(ColorModePicker.Logic.cycle(opts, Light) == Dark) &&
          assertTrue(ColorModePicker.Logic.cycle(opts, Dark) == System) &&
          assertTrue(ColorModePicker.Logic.cycle(opts, System) == Light)
        },
      ),
    )

}
