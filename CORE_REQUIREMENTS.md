# StatLab 2.2 acceptance mapping

This file describes the extended 2.0 implementation. See RELEASE_2_1.md for the Graph Editor release scope and artifacts/QA_REPORT_2_1.md for executed verification. See README for supported workflows and limits.

| Requirement | Implementation / verification |
|---|---|
| 1, 2, 5, 14, 25, 35 datasets; arbitrary subsets | `test-comparison.R`; immutable registry; batch-file import; browser 25-file addition |
| Common endpoint | Explicit mapping and endpoint selector; matching unit enforcement |
| Male/female independently; arbitrary mapped subgroup | Independent filters and protected series grouping; missing-subgroup regression |
| Dose ranges spanning orders of magnitude | Four ranges from the specification in deterministic examples; overlay and facets |
| Zero controls | Separate linear control panel, pseudo-log, or explicit exclusion diagnostics; no substitution |
| Linear/log10/pseudo-log X/Y | `test-axes.R`; reversible transformations and source-coordinate assertions |
| One/two/three/more intervals | Aligned ggplot panels; tests for 1–5 intervals, validation, widths, coordinates |
| Exact coordinates | `.x` plotting coordinate is separate from original `dose`; transformations never rewrite stored data |
| Missing subgroups | No synthetic rows or interpolated subgroup observations |
| Edit and refresh | Validated canonical DT edits/add/delete; reactive comparison; browser checks |
| Export preserves axes | Same cached panel objects for Plotly and static export; automated PNG/SVG/PDF and browser downloads |
| Save/reopen | Versioned local JSON, metadata/source/mapping/settings/results; roundtrip and browser restoration |
| Verified statistics | Reference tests against R t.test, aov, TukeyHSD, multcomp, wilcox.test, kruskal.test; raw and sufficient statistics |
| Unsupported methods rejected | Summary rank tests, multi-study/stratum pooling, invalid controls, missing information |
| Malformed input/missing values | Explicit validation; errors shown without committing invalid data |
| Responsiveness/memory | `scripts/profile.R` and `scripts/profile_editor_21.R`, increasing studies/rows; `artifacts/performance*.csv` |

## Delivered features

Modular local Shiny; Compare Datasets home; raw/summary import; CSV/XLSX/paste; manual initial row and editable observations; new groups; dynamic registry; retained source tables and additional endpoint mapping; explicit multiplicative unit conversion; study/sex/generation/exposure/collection-time/arbitrary subgroup filters; overlay and combined facets; all requested graph families when supported; protected independent line grouping; X/Y settings; any number of interval panels; separate zero controls; shared or labeled independent Y; reusable presets; requested independent-group statistics; raw diagnostics; figure/results/data exports; safe project persistence; deterministic synthetic examples; automated test suite; browser and performance evidence.

## Explicit limits / pending work

- No original animal CSV was supplied. Full numerical regression against that file and real-data usability assessment remain pending. The supplied original code is retained with compact formatting.
- Wide raw format supports numeric dose headers; arbitrary heterogeneous wide layouts require individual variable mapping or preprocessing. Wide summary blocks are not inferred.
- Arbitrary biological subgroup content is supported through one mapped `subgroup` column in addition to named factors. Dynamically adding an arbitrary number of separately named factor columns is pending; composite subgroup columns are supported.
- Dose, numeric exposure and numeric collection time support grouped response series. Other retained numerical source columns support in-session scatter/individual plots; they require explicit axis units and are not assumed to be comparable dose series.
- Missing sample sizes allow mean/SD visualization, but prevent sampling-error estimates and inference. Missing raw responses are rejected, not imputed or silently omitted.
- Anchored manual annotations and automatic labels/brackets from validated pairwise results are implemented; complete all-pairs families support compact letters. Dense layouts may require manual offsets.
- Four editable scientific/presentation presets and user presets are implemented. Certified journal templates and per-panel font overrides are pending.
- Axis appearance controls include grid, themes, titles, font sizes, ticks, formats, direction and limits. Axis line/tick color and tick length controls are implemented; arbitrary per-spine styling remains outside the editor.
- Statistical inference is for independent groups in one selected stratum/study. Repeated-measures, paired, litter/clustered, covariate, nested and mixed models, model-based dose-response fitting and cross-study meta-analysis are outside this release.
- Reactive figure caching and server-paginated tables are implemented. Incremental plotting/virtualized legends for very large datasets are future optimizations. No unlimited computational-capacity claim is made.
- Upload/loader threshold: 500 MB per file, configurable locally. No dataset-count cap.
- Format v1 projects migrate to v2. Graph-only undo/redo is implemented; observation undo and autosave are pending. Save a project before destructive data edits.

## Graph Editor acceptance coverage

The original 239 R assertions remain passing. The extended suite covers stable study/group encodings, complete annotation identities, automatic evidence and rejection rules, compact letters, typography persistence, interval edits, safe resets/history, original coordinates and static exports. Chromium workflows exercise hierarchy, title edits without numeric re-preparation, study search/hide/highlight/order, interval-table validation, manual and automatic annotations, v1 loading and v2 round trips. Exact executed totals and screenshot/export links are recorded in the release QA report. No dataset-count maximum is imposed.

## StatLab 2.2 guided workflow update

Direct Compare/Show/Highlight checkboxes replace read-only study booleans. Import and editing are separate guided workspaces with source provenance and raw/summary CSV templates. Statistical treatments use explicit native checkboxes with a selection preview. Existing scientific methods, arbitrary study/interval counts, all graph/axis/annotation functions and versioned projects remain available. See `artifacts/QA_REPORT_2_2.md` for executed regression results.
