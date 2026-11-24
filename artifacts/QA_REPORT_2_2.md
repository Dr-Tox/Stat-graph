# StatLab 2.2.0 verification

## Scope and preservation

This release reorganizes the existing local Shiny application. It does not replace its architecture or scientific data model. The existing graph editor, axis transformations/intervals, annotations, independent-study comparisons, endpoint mapping, CSV/XLSX import, raw/summary validation, editing, project/preset persistence and numerical statistical engine remain available. No runtime dependencies were added or upgraded. Project format stays v2, with existing v1 migration.

## Implemented workflows

- Study selection has direct Compare/Show/Highlight checkboxes. Source labels distinguish synthetic examples, recorded imported files and saved studies without recorded provenance. Checkbox events carry immutable study IDs. Search and row selection survive in-place status updates. Bulk row operations remain available by clicking study-name cells.
- Import Data and Manage Studies are separate tabs. Numbered source/description/review cards provide raw and summary examples, CSV template downloads, explicit column matching, optional factors/metadata/unit conversions and import confirmation. Confirmation resets when relevant settings change. The unsupported summary-wide combination is not offered and is rejected server-side. Existing raw wide tables remain supported.
- Existing study editing, rename/reorder/removal, additional endpoint mapping and harmonization remain accessible. Default tables hide technical identifiers, fixed unspecified factors, redundant metadata and fields irrelevant to the input mode. Show all columns restores those fields. Display formatting does not round stored or exported observations.
- Statistical treatment selection uses native checkboxes with dose and sample-size labels, plus select-all/clear controls. Study/source and selected-stratum context precede inference. Only factors with choices are shown; fixed factors remain in the calculation scope. Compact descriptions/results keep full numerical precision in state and exports; full result details and diagnostics are expandable.
- Removed the obsolete hidden study selector that could compete with direct checkboxes. Comparison filter controls now update in place, deduplicate unchanged options and protect restored filter values while client bindings synchronize.

## Executed verification

The pre-change 2.1.1 baseline passed 48 R test blocks / 463 assertions. The final suite passes **52 blocks / 526 assertions**, 0 failures, 0 errors and 0 warnings. All previous assertions remain included. New coverage checks study counts 1/2/5/14/25/35; direct toggle semantics and unchanged observations; source labeling; raw/summary templates; treatment labels; clearing/reselecting studies; and filter restoration followed by further user edits.

Commands:

```sh
Rscript tests/run_tests.R
# Start the app separately with shiny::runApp(host='127.0.0.1', port=3841)
STATLAB_URL=http://127.0.0.1:3841 node scripts/browser_guided_ux.cjs
STATLAB_URL=http://127.0.0.1:3841 node scripts/browser_smoke.cjs
STATLAB_URL=http://127.0.0.1:3841 node scripts/browser_statistics_project.cjs
STATLAB_URL=http://127.0.0.1:3841 node scripts/browser_editor_21.cjs
```

- New guided workflow: **33 browser checks passed**. Real native checkboxes, clear/reselect, source labels, templates, confirmation reset, summary import, hidden/all columns, treatment select-all/clear, Welch summary inference, JSON restoration, 25-file batch import with 30 studies, searched-study toggling without losing the query, narrow viewport and zero JavaScript errors.
- Preserved main workflow: **25 checks passed**. Exact study IDs, male filtering, three interval panels, PNG/SVG/PDF downloads, saved/restored subgroup filters, explicit inference, pasted data, validated cell edits, invalid numeric edit rejection, added observations, retained-source endpoint mapping, 30 studies and zero external browser requests/JavaScript errors.
- Statistical project workflow: **7 checks passed**. Raw Welch inference at 99% confidence, real CSV/XLSX downloads, saved numerical results and restoration of study/sex/method/two treatments.
- Graph Editor regression: **43 checks passed**. See `browser-editor-2.2.log`. This retained scenario covers hierarchical editing, prepared-data reuse, split axes, manual/automatic annotations, typography, exports and project compatibility.

Total: **108 browser checks passed**. Final logs are `browser-guided-2.2.log`, `browser-main-2.2.log`, `browser-statistics-2.2.log` and `browser-editor-2.2.log`; the R log is `tests-2.2.log`. The before-change run is retained in `baseline-2.1.1-ux.log`. Existing screenshots/exports with historical names may be refreshed by the retained scenarios; prior releases preserve their earlier evidence in Git history.

Legacy browser scripts were adapted to the new visible tabs and native checkbox inputs; assertion coverage was retained and exact study identity checks strengthened. Tests click study-name cells for batch actions because row centers now contain actionable checkboxes. Readiness waits inspect actual rendered study/source/result state rather than assuming a fixed rendering time.

`R/statistical_methods.R` remains byte-identical to the baseline, SHA-256:
`c7613a2b44ded6ec807d23724b1d3d1bf099d67890ddd5b9fce25b3ba07a6e30`.

## Screenshots

- [Study checkboxes](ux-study-checkboxes.png)
- [Guided import](ux-guided-import.png)
- [Manage studies and recorded source](ux-manage-studies.png)
- [Statistical selection and result](ux-statistics.png)
- [Search in a 30-study collection](ux-30-studies.png)
- [Narrow-screen import](ux-responsive.png)

## Limits

This is a guided form workflow, not a new general-purpose spreadsheet or a biological endpoint inference system. Users must still verify endpoint equivalence, units, independent experimental units and group mappings. Arbitrarily heterogeneous wide layouts, new independently named factor columns, paired/repeated/mixed models and nonlinear fitting remain outside the existing release scope. Dense annotation positioning can require manual adjustments. Linux/Chromium is verified; Windows/macOS, original experimental data and scientist usability testing remain unverified. There is no claim of unlimited hardware capacity or certified journal formatting.
