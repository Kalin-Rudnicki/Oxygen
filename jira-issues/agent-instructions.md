# Agent Instructions — Oxygen Jira Worker

> **Audience:** A fresh sub-agent spawned to work on a single Jira issue.
> **Context budget:** You have ONLY this file + the issue key/ID you were given (e.g. `OXY-123`). You do NOT have prior conversation history, and you should NOT assume you have the full checklist or any other briefing.

---

## 1. Raw Instructions (verbatim from human)

> "The sub-agent should only have the context to look at this file, and which issue it's responsible for."

That is the isolation constraint. Do not rely on external context, hidden prompts, or assumptions about other issues. Everything you need to operate must be in this file or discoverable from the repo/workspace itself.

Additional per-issue instructions will be appended to this file in §4. The human will explain what to do for each issue — this file will persist both the raw wording and the interpreted/expanded guidance.

---

## 2. Interpreted Guidance — What This Means For You

* **You are single-issue scoped.** You are responsible for exactly one issue (e.g. `OXY-17`). Do not attempt to fix, close, or branch across other issues unless explicitly instructed in §4 for your issue.
* **This file is your briefing.** Treat it as the sole source of project-level process. If something is not in here, infer it from the repository (code, docs, `jira-issues/checklist.md`, configs) — do not hallucinate requirements.
* **You must be self-sufficient.** Clone the repo state, locate relevant code, read existing tests/docs, and make decisions from first principles. If the issue description is vague, use the codebase as ground truth.
* **Persist your interpretation.** When you record work or planning notes, distinguish between *what the human literally said* (raw) vs. *what you inferred/suggest* (interpretation). This file models that discipline.

---

## 3. Repository Orientation

* **Workspace root:** The repo root contains the `oxygen-*` modules (e.g. `oxygen-sql`, `oxygen-http-server`, `oxygen-json`, etc.), `docs/`, and `jira-issues/`.
* **Issue checklist:** `jira-issues/checklist.md` — 79 incomplete issues (7 In Progress epics, 72 To Do). Use it to confirm your issue exists and to see siblings, but do not treat it as a spec.
* **Issue details:** The canonical Jira source is `https://kr-oxygen.atlassian.net/browse/OXY-<N>`. If you have network access, fetch it. If not, infer intent from the checklist title (`Task`/`Bug`/`Epic`/etc.), priority, and the associated module, plus code search.
* **Results:** `jira-issues/results/` exists for per-issue outputs (reserved — do not write there unless §4 tells you to).

### Before You Code

1. Read `jira-issues/checklist.md` to confirm your issue's title, type, and priority.
2. Identify the owning module (often in the title, e.g. "oxygen-sql") and inspect its `src/`, tests, and `README`/`docs/docs/<module>/`.
3. Search for TODOs, `FIXME`, skipped tests, or related open code that hints at the intended design.
4. Check `docs/` for migration notes and conventions that constrain the solution.

---

## 4. Per-Issue Instructions

> **Status:** Briefed 2026-08-07. This section is the SAME 6-step workflow for EVERY issue (OXY-#). No issue has custom per-issue overrides — follow the steps exactly as written. This file persists both the human's RAW wording and the INTERPRETED expansion.

### 4.1 Raw Instructions (verbatim from human — applies to every OXY-#)

> 1. read what already existed on the issue, from the `checklist.md` markdown file
> 2. create a `jira-issues/results/OXY-#.md, and port the existing fields in as `original`
> 3. read the code, and attempt to figure out what the issue means
> 4. update the issue markdown file with what you think the expanded description is, along with a 1-6 confidence rating that you correctly identified it
> 5. ONLY IF confidence >= 3, do another round of deeper analysis to figure out what the required changes would be
> 6. ONLY IF confidence >= 3, update the issue markdown file with an updated description, story point estimate, required changes, etc. this should also include a 1-6 rating of how "autonomously" an agent could do the work, as well as a 1-6 of how much open ambiguity would need to be resolved before an agent starts. any obvious open questions or needed clarifications should also be documented in the markdown file

### 4.2 Interpreted Guidance — Expanded, Actionable Workflow (applies to every OXY-#)

You are given ONE issue key at spawn (e.g. `OXY-17`). Execute these 6 steps in order. Stop early where instructed. Do NOT assume other issues' context.

#### Step 1 — Read existing fields from `checklist.md`

* Open `jira-issues/checklist.md` and locate YOUR key exactly (e.g. `OXY-17`). It appears in one of two sections: `## In Progress (7)` or `## To Do (72)`.
* Extract what is there: the markdown line itself, the linked Jira URL (`https://kr-oxygen.atlassian.net/browse/OXY-#`), the **Type** (Epic/Task/Bug/Documentation/Architecture/Subtask), the **Priority** (Lowest/Lower/Low/Normal/Higher/High), and the **Title** string after `—`. If the title hints at an owning module (e.g. "oxygen-sql"), note it but treat it as a hint, not a spec.
* If the key is missing from the file, record that fact — do not hallucinate fields. If network is available you MAY try fetching the Jira URL, but `checklist.md` is the canonical fallback.

#### Step 2 — Create `jira-issues/results/OXY-#.md` with `original`

* Create the file `jira-issues/results/OXY-#.md` for YOUR issue only. Ensure the parent dir exists (`mkdir -p jira-issues/results`).
* Port the fields from Step 1 into a section called `## Original` (or `original:` frontmatter — pick one and be consistent). Preserve verbatim: the checklist line, type, priority, title, Jira URL, and checklist status (In Progress vs To Do). This is the audit trail — do not edit or infer here.
* Suggested minimal shape (use this structure):
  ```markdown
  # OXY-# — <title>

  ## Original
  - **Key:** OXY-#
  - **Checklist line:** `- [ ] [OXY-#](https://...) — **Type** · Priority — title`
  - **Type:** ...
  - **Priority:** ...
  - **Title (verbatim):** ...
  - **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-#
  - **Checklist section:** In Progress / To Do

  ## Expanded Description
  ...

  ## Confidence
  ...

  ## Required Changes (only if Confidence >= 3)
  ...

  ## Estimates & Autonomy (only if Confidence >= 3)
  ...

  ## Open Questions
  ...
  ```

#### Step 3 — Read the code to infer intent

* Using the title/type/module hint, search the repo for relevant code: `oxygen-*/src/`, tests, `docs/docs/<module>/`, `README.md`, and grep for keywords from the title (e.g. `IN`, `group by`, `on conflict`, `Tuple`, etc.), plus `TODO`/`FIXME`/`skip`/`pending`.
* Read enough to hypothesize what the issue means. For Epics, look at child issues and module scope; for Tasks/Bugs, look at the DSL, schema, or runtime that would need to change.
* Do NOT change code in this step. Take notes for the next step. If the codebase gives no signal, say so — that informs a low confidence rating.

#### Step 4 — Write expanded description + confidence 1–6 (ALWAYS do this)

* Update `jira-issues/results/OXY-#.md` — fill `## Expanded Description` with your best-guess full issue description (what the feature/bug likely is, who it affects, why it matters, rough acceptance criteria inferred from code). Then fill `## Confidence` with a **1–6 rating** for "I correctly identified what this issue means":
  * 1 = wild guess, almost no signal
  * 2 = weak signal, multiple plausible meanings
  * 3 = plausible / more likely than not (threshold)
  * 4 = good evidence, one clear frontrunner
  * 5 = strong evidence (code TODO + docs + sibling issues align)
  * 6 = near-certain (explicit spec / skipped test / design doc found)
* Include 2–4 bullet justification for the rating. If confidence is 1–2, still write the expanded description as your best attempt — do not leave it blank.

#### Step 5 — Gate: deeper analysis ONLY IF confidence >= 3

* If your Step 4 confidence is **1 or 2: STOP**. Leave `## Required Changes` and `## Estimates & Autonomy` as `N/A — confidence < 3, deeper analysis skipped per instructions.` Do not proceed to Step 6.
* If confidence is **3, 4, 5, or 6: continue** to Step 6. Now do a second, deeper pass: trace the exact files/modules that would change, sketch the design, consider schema/migration/API implications, and check cross-cutting concerns (tests, docs, backwards compat).

#### Step 6 — Update file with story points, required changes, autonomy & ambiguity (ONLY IF confidence >= 3)

* Re-update the SAME `jira-issues/results/OXY-#.md` (do not create a second file). Fill:
  * **`## Required Changes`** — concrete, repo-grounded list: files/modules, new vs. modified code, data model changes, tests/docs to add. Use checkboxes or bullets. Call out what you verified vs. inferred.
  * **`## Estimates & Autonomy`** — three ratings/estimates:
    * **Story points** (use Fibonacci-ish scale: 1, 2, 3, 5, 8, 13 — note if Epic vs. Task sized differently)
    * **Autonomy 1–6** — "how autonomously could a sub-agent do the work with only this briefing + the repo?" 1 = needs constant human pairing, 6 = fully autonomous, just run it.
    * **Ambiguity-to-resolve 1–6** — "how much open ambiguity must be resolved BEFORE an agent starts?" 1 = ready to start, 6 = major product/design questions block start.
    * Give 1–2 line justification for each rating.
  * **`## Open Questions`** — any obvious ambiguities, needed clarifications, assumptions you made, or decisions a human should confirm before implementation starts. If none, write `None identified` rather than omitting.
  * Optionally refine `## Expanded Description` if the deeper pass clarified it — keep the confidence rating from Step 4 unchanged (do not retroactively inflate it).
* Keep `## Original` untouched.

#### Deliverable & Verification

* **Deliverable for every issue:** exactly one file: `jira-issues/results/OXY-#.md` (where `#` is YOUR issue number). Do not edit `checklist.md`, do not write to other `OXY-#.md` files, do not commit/push.
* **Verification (self-check before you finish):**
  1. `ls jira-issues/results/OXY-#.md` exists and contains `## Original` with verbatim checklist fields.
  2. `## Expanded Description` and `## Confidence` (1–6 + justification) are present.
  3. If confidence >= 3, `## Required Changes`, `## Estimates & Autonomy` (story points + autonomy 1–6 + ambiguity 1–6), and `## Open Questions` are filled; if < 3, those sections state `N/A — confidence < 3`.
  4. File is Markdown, no secrets or grader material referenced.

---

## 5. General Operating Rules (apply to every issue — §4 overrides where it conflicts)

1. **Minimal, isolated context.** Do not request or assume access to other agents' issues, full chat history, or private grader material. Work only from this file + your issue + the repo.
2. **Scope discipline.** Do exactly the 6 steps in §4 for YOUR issue only. Do not write other issues' `OXY-#.md` files and do not collapse multiple issues into one change.
3. **Research-only, no code changes.** This task is analysis + markdown output. Do NOT edit source code, do NOT implement the issue, do NOT run builds/tests unless you need to read them to infer intent. Your only write is `jira-issues/results/OXY-#.md`.
4. **Verify the markdown.** Before finishing, re-read your `jira-issues/results/OXY-#.md` against the checklist in §4 Deliverable & Verification. Ensure confidence threshold gating was respected.
5. **Leave the workspace reviewable.** Do not commit, push, or rewrite history. Leave `checklist.md` and untracked user files untouched. Clean up only artifacts you created (tmp files).
6. **Record honestly.** If you cannot fetch Jira, cannot find code signal, or cannot verify a claim, say so — do not fabricate. A low confidence (1–2) with honest justification is a correct result.
7. **Ask when blocked.** If the title + code leave multiple plausible meanings, document the ambiguity in `## Open Questions` and pick the most reasonable interpretation with justification rather than stalling.

---

## 6. What to Do Right Now (as the sub-agent)

1. Read this file top-to-bottom.
2. Note your assigned issue key (provided at spawn, e.g. `OXY-17`).
3. Execute §4 Steps 1–6 in order for that key — respecting the confidence >= 3 gate — and leave `jira-issues/results/OXY-#.md` as your sole deliverable.

---

*File created: 2026-08-07. Briefed: 2026-08-07. Ready for sub-agents to run §4 on any OXY-#.*
