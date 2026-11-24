# StatLab 2.1.1

Patch 2.1.1 corrects time-axis distribution grouping, custom-colored grouped-bar errors and automatic Log10 limits for constant positive responses. See [CHANGELOG.md](CHANGELOG.md) and [patch verification](artifacts/QA_REPORT_2_1_1.md).

A local, modular R Shiny workspace for comparing independent studies that share a scientific endpoint, including studies with non-overlapping dose ranges. There is **no software-imposed dataset-count limit**. The default home loads four clearly synthetic studies spanning 0 to 10,000 mg/kg/day.


## New in 2.1: Advanced Scientific Graph Editor

The existing 2.0 modules and statistical engine are retained. Open **Graph editor** in the comparison properties panel. Categories expand into properties and advanced settings; values remain intact when sections close. Valid edits apply after a short pause. The toolbar can hide the properties panel to maximize the figure.

- **Titles & Labels:** independently edit main/subtitle content, visibility, font, size, face, color, alignment and spacing; edit X/Y titles, units and typography. Automatic titles are opt-in; stored manual titles are retained.
- **Axes:** edit scales, limits, tick typography/formats and any number of interval-table rows. Add/remove/move rows and edit boundaries/relative widths. Invalid drafts retain the last valid applied axes. Rendered intervals remain ordered and non-overlapping.
- **Data & Groups:** stable study-ID colors and display labels, keyed subgroup point-symbol and line-style tables, point sizes, error thickness/caps and opacity. Bar/box/violin group offsets are computed separately from original dose coordinates; annotations follow the same groups.
- **Statistical Annotations:** add user-provided stars, letters, labels or brackets using exact study/endpoint/stratum/treatment anchors. Add multiple rows, edit or hide them independently, or paste a manual letter table. No internal analysis is required for manual labels. Advanced controls set original-unit positions/offsets, face, size and bracket geometry.
- **Automatic annotations:** run a valid explicit comparison in Statistics, then use its validated results. Adjusted p-values are retained. Configure strict asterisk cutoffs and scientific/decimal/threshold p-label formats. Complete CLDs require a justified all-pairs family; Dunnett control-only and omnibus results are rejected for this purpose. Stale evidence is suppressed with a diagnostic.
- **Lines & Appearance / Legends & Layout:** fonts, backgrounds, grid/axis/border styles, margins, facets and spacing, grouped scrollable interactive legends and optional static legend omission.
- **Presets & Reset:** four editable built-in styles, individual-property or section reset, and graph-only undo/redo. Save reusable custom graph/axis presets in Export / Projects.

The compact study manager supports search, arbitrary selection, activation, hiding, highlighting and reordering. None of these actions changes experimental observations. Numeric preparation is cached separately from cosmetic edits.

Use `examples/advanced-21.statlab.json` to explore four synthetic studies, male/female facets, three X intervals, error bars, Unicode/manual significance letters, asterisks and a validated comparison bracket. Reproduce its PNG/SVG/PDF with `Rscript scripts/generate_advanced_examples.R`.

See [CHANGELOG.md](CHANGELOG.md), [RELEASE_2_1.md](RELEASE_2_1.md) and [PROJECT_FORMAT.md](PROJECT_FORMAT.md). The supplied original application is retained in `original/`; the separate 2.0 archive has not been modified. All example data are synthetic.

## Install and launch

Use R 4.5 or newer. From the `StatLab` directory:

```r
source('scripts/install.R')
shiny::runApp('.', host = '127.0.0.1', port = 3838)
```

Alternatively restore the recorded dependencies in an isolated project library:

```r
install.packages('renv', repos = 'https://cloud.r-project.org')
renv::restore(lockfile = 'renv.lock', prompt = FALSE)
shiny::runApp('.')
```

`renv.lock` records the packages actually used for verification, including transitive dependencies. For a Conda/Micromamba alternative, use `micromamba create -f environment.yml`, then `micromamba run -n statlab Rscript -e "shiny::runApp()"`. The environment export pins the verified Linux package versions. OS libraries needed by R, Cairo, fonts and svglite must otherwise be installed separately; Linux binary R distributions or your distribution's development libraries simplify installation. This workspace also has a project-local Conda R installation at `/workspace/.r-env/bin/Rscript`; it is not required or bundled with the portable source.

No packages are installed automatically when the app launches. After installation, the application needs no network connection, uploads no data, and has no telemetry. Bind to `127.0.0.1` for local use. Binding to a public interface exposes the app to other users on that network; authentication is not included.

## Compare studies

1. Open **Data Manager**, select one or multiple CSV/XLSX files, and map a representative file's columns. For different schemas, import separate batches.
2. Select raw or summary mode. Name the biological endpoint explicitly and confirm equivalence. A shared label is the user's scientific assertion of equivalence; the app does not infer that ALT and AST or absolute and relative organ weights are equivalent.
3. Map a source study-ID column to split a file into independent datasets. Each import receives fresh internal IDs, so equal study names and subsequent imports cannot overwrite existing datasets.
4. Map dose (or any numerical source X column), response/mean, endpoint/unit columns when present, and relevant strata. Mapped endpoint and unit columns take precedence over fallback text fields; endpoint names from a mapped column remain distinct. To harmonize aliases deliberately, leave the endpoint column unmapped and supply the common biological label. Blank target units retain source units; comparison still rejects inconsistent units for the selected endpoint. Map SD and n when available. For a source matching the original app, choose summary mode, map `study_name`, `dose_mg_kg_day`, `mean_serumconc_ng_ML`, `sd_serumconc`, `days_exposed`, and `collection_time`; use dose unit mg/kg/day and response unit ng/mL. Leave n unmapped if absent. `examples/legacy_original_schema.csv` demonstrates this workflow. Supplied means and SDs can be visualized without n; SEM, confidence intervals and inferential tests require valid n. Retained original source tables allow another response column to be added to the same study without reimporting.
5. Explicitly choose source and target units. Supported compatible conversions include mg/ug/ng per kg per day, mass, concentration, and relative g/100g to %. Unknown units can be retained unchanged; conversion between unknown or incompatible units is rejected. No offset-based conversion is assumed.
6. Return to **Compare Datasets**. Select any subset, select an endpoint, and filter sex, generation, exposure duration, collection time, or an arbitrary mapped subgroup. Empty subgroup filters mean all available levels. Missing subgroups are never fabricated.
7. Use **Graph editor** to overlay, facet by any combination of the supported experimental fields, change encodings, choose graph types and style. Valid changes apply automatically; Apply is also available. Dose, numeric exposure duration and numeric collection time are selectable X variables. Retained numeric source columns are also selectable for paired scatter/individual plots; choose explicit X titles/units. Generic source variables do not silently become dose-response series. Axis labels are separate from unit conversion.

A subgroup may represent genotype, tissue, species, exposure schedule, or another categorical factor. For multiple additional factors beyond the named fields, supply a composite subgroup column with explicit labels; arbitrary dynamically added factor columns are not implemented.

Raw and summary rows are tagged throughout; distribution plots require raw observations for all selected datasets. Scatter plots retain raw observations and show supplied summary means. Dose-response/line plots aggregate raw observations to group means; default bars show SD. No pseudo-individual observations are generated. Lines always retain dataset identity and experimental strata, even when the user chooses a different color mapping. When X is duration/time, dose remains part of the independent series identity.

## Axis settings and broken X axes

X and Y independently support linear, log10, pseudo-log, limits, reverse direction, major/minor ticks, custom labels, numeric formats and text sizes. Pseudo-log uses `scales::pseudo_log_trans(sigma, base=10)` and is explicitly labeled by the chosen setting; sigma controls the near-zero linear region. It supports zero and negative responses without modifying observations.

Use the editable X interval table in Graph editor → Axes. The optional 2.0 text-interchange control also accepts any number of rows:

```text
0,1,1
10,100,1
1000,10000,2
```

Each row is `start,end,relative panel width`. Ordered ranges must have positive gaps and positive widths. Empty panels are rejected. True numeric values are rendered in aligned ggplot panels; gaps are marked `//`, and diagnostics report points outside the requested intervals. Panel ranges and widths are shared by the interactive figure and PNG/SVG/PDF exports. Shared Y limits are the default; independent Y ranges are opt-in and visibly labeled. Limits crop the viewport rather than changing coordinates. Tick intervals are in original units, including on transformed axes.

On log10 X, choose either explicit exclusion with a warning, or a separate linear zero-control panel displaying actual X=0. Negative X values cannot appear on log10. Pseudo-log is another option. No arbitrary positive substitute is used for zero. On log10 Y, nonpositive points are counted and excluded; error bars crossing zero are explicitly reported and omitted in their entirety, without clamping their bounds. The saved observations stay unchanged.

Valid axis edits apply after a short pause; **Apply axis settings** is also available. **Restore automatic scaling** resets both axes and intervals. Export / Projects can store reusable graph-plus-axis presets.

## Data editing and projects

Paste tab-separated Excel data or CSV with headers into Data Manager. For manual entry, enter a header and initial row, import, then edit cells and add rows. Source previews show the first 100 rows; the canonical table is server-paginated. Double-click canonical cells to edit. Dataset ID and input mode are protected. Validation is applied before committing a change; invalid numeric values and missing required observations are rejected explicitly, without implicit imputation or dropping rows. The plot refreshes from reactive state after valid edits. Use a unique treatment label for each inferential group.

Dataset names, notes, registry order, removal, additional endpoint mappings and compatible unit harmonization are editable. Removing a dataset or observation affects in-memory project state; save a project before destructive edits if you need a recovery copy. The original import remains in that dataset's saved source table for provenance and additional variable mapping.

Long raw tables map dose and response columns. Long summary tables map dose, mean, SD and n. Wide raw tables currently support **numeric dose headers**, such as `0`, `0.01`, and `1000`, with optional stratum columns. Select response columns explicitly. Other wide layouts require mapping individual endpoint columns or upstream reshaping; they are not inferred automatically. For Excel, a selected sheet number is applied to the batch; import sheets separately if their mappings differ.

Projects use portable, versioned UTF-8 JSON (format `StatLab`, version `2`, with version `1` migration), containing canonical observations, retained source tables, dataset metadata, endpoint mappings, selected studies, filters, applied graph/axis settings, presets, last statistical settings/results, timestamps and package versions. Reopening parses JSON and validates the schema; it never sources R or evaluates project content. See [PROJECT_FORMAT.md](PROJECT_FORMAT.md).

The configurable upload/loader safety threshold is 500 MB per file, independent of dataset count. It is a memory-protection measure, not a promise of unlimited capacity. Projects are saved through the browser to a user-chosen local location.

## Statistics

Inference is a separate workspace. Explicitly select **one study, one endpoint, one level of each experimental stratum, and treatment groups**. Studies are never pooled automatically. Methods:

| Method | Raw | Summary mean/SD/n | Correction / interval |
|---|---|---|---|
| Descriptive mean, SD, SE, t confidence interval | Yes | Yes; intervals need SD and n | Per-group t interval |
| Student independent t-test | Yes | Yes | Pooled variance; two-sided mean-difference CI |
| Welch t-test | Yes | Yes | Satterthwaite df; two-sided mean-difference CI |
| One-way ANOVA | Yes | Yes | Within/between sufficient statistics; F and both df |
| Tukey | Yes | Yes | Tukey–Kramer family-wise p-values and simultaneous CIs |
| Dunnett against selected control | Yes | Yes | `multcomp`, pooled covariance and residual df; simultaneous single-step p-values/CIs |
| Mann–Whitney U | Yes | No | R Wilcoxon rank-sum, asymptotic with continuity correction; location-shift estimate/CI when estimable |
| Kruskal–Wallis | Yes | No | Tie-corrected rank omnibus test |
| Dunn | Yes | No | Tie-corrected normal rank comparisons; selectable p adjustment (Holm default) |

Results include method, study, endpoint, groups, sample sizes, estimate/CI when available, statistic, df when applicable, full numerical p-value, alpha, correction and assumptions. Methods needing unavailable information fail explicitly. Pairwise t-tests are limited to two user-selected groups; larger comparisons use the relevant multiple-comparison methods. The choice of method is not automatically switched by a normality p-value. Raw-data diagnostics include within-group residual Q–Q, Shapiro when valid, and Fligner variance diagnostics. Repeated measurements, clustered/litter effects, paired designs, dose-response model fitting and multilevel inference are not implemented. Confirm the experimental unit independently.

Manual significance annotations remain user-provided. Automatic annotations require matching validated engine results, use their numerical/adjusted p-values, and retain source evidence. Verify annotation placement and scientific interpretation before publication. Changing observations clears stale inferential results; changing only the display retains the last explicitly executed result, which includes its analyzed groups.

## Tests and evidence

```sh
Rscript scripts/generate_advanced_examples.R
Rscript tests/run_tests.R
Rscript scripts/profile.R
# With the app running and Playwright/Chromium installed:
node scripts/browser_smoke.cjs
node scripts/browser_statistics_project.cjs
node scripts/browser_editor_21.cjs
node scripts/browser_render_21.cjs
# Visual checks also require Python/Pillow and pdftotext:
Rscript scripts/profile_editor_21.R
python scripts/verify_visuals_21.py
```

Synthetic data includes 25 CSV studies, an XLSX workbook, summary data, wide raw data and a portable four-study project. The test suite covers dataset counts 1/2/5/14/25/35, arbitrary subsets, endpoints, missing subgroups, coordinate invariance, zero behavior, transformations, 1–5 intervals, malformed inputs, persistence, exports and statistical reference calculations. Profiling measures elapsed preparation time, object sizes and cumulative allocations at increasing study/observation counts; object sizes are not process peak RSS. Browser rendering/export latency is verified separately by the UI workflow. See [artifacts/QA_REPORT_2_1.md](artifacts/QA_REPORT_2_1.md) and [CORE_REQUIREMENTS.md](CORE_REQUIREMENTS.md).

## Architecture

- `app.R`: dependency check, application composition, per-session state.
- `R/data_validation.R`, `endpoint_mapping.R`, `project.R`: pure canonical model, explicit unit mapping, identities, JSON persistence.
- `R/mod_compare.R`: selection/filter workspace and cached figure reactive.
- `R/mod_axes.R`: independent controls, validation, transformations and interval specification.
- `R/mod_graphs.R`: graph preparation, protected series grouping, ggplot panel construction and Plotly rendering.
- `R/graph_settings.R`, `graph_rendering.R`, `graph_editor_ui.R`: compatible configuration, stable encodings, styling and hierarchical editor.
- `R/mod_annotations.R`, `mod_annotation_editor.R`: anchored manual annotations, validated statistical evidence, compact letters and annotation UI.
- `R/mod_data.R`: batch import, mapping, source retention, editing, dataset registry.
- `R/statistical_methods.R`, `mod_statistics.R`: sufficient-statistic calculations and scoped inferential UI.
- `R/mod_export.R`: exports, presets and project restoration.

The same ggplot panel objects feed static exports and interactive plots, so an export cannot silently fall back to an unsplit plot. Session-local reactive state isolates users. Pure functions are tested independently; UI modules orchestrate state changes. App startup does not read or mutate experimental files automatically. The original user-supplied reference is retained in [original/app2(1).R](original/app2(1).R), with compact formatting, and is not executed by the new app. The original CSV was not supplied, so original-data numerical regression remains unavailable.
