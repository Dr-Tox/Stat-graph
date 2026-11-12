# Verification report

Environment: Linux x86-64, R 4.5.3, dependencies recorded in `renv.lock` and `environment.yml`. The initial workspace had no R or original source files; a project-local R runtime was installed for verification. Original user-supplied application code was retained in `original/`. No real experimental CSV was supplied.

## Automated R verification

Command: `Rscript tests/run_tests.R` from the project root.

**19 test blocks, 239 assertions passed, 0 failures, 0 errors.** See `tests.log` and `test-results.csv`.

Coverage includes 1/2/5/14/25-study registry management, arbitrary subsets, matching endpoints/units, explicit endpoint/unit source columns and per-row conversions, missing male/female strata, original coordinate invariance, linear/log10/pseudo-log and reversal, separate zero controls, explicit log exclusions, 1–5 real interval panels, invalid/empty/overlapping intervals, aligned facet cells across split panels, single-point Plotly series, legend deduplication, Shiny empty-numeric-input application, static PNG/SVG/PDF, raw/summary/wide/CSV/XLSX validation, optional-n legacy summaries, numeric source X variables, project JSON roundtrip including metadata/presets, unsafe identifier-counter rejection, and explicit unsupported-inference rejection.

Reference calculations use independent fitted-model paths: `t.test`, `aov`, `TukeyHSD`, `multcomp::glht(aov(...))`, `wilcox.test` and `kruskal.test`. Raw and mean/SD/n sufficient-statistic implementations agree within the test tolerances. Dunnett integration comparisons allow 0.001 numerical tolerance. Dunn rank-test bounds/corrections and summary rejection are tested; a separate external Dunn implementation was not installed for a numerical cross-check.

## Browser verification

Command: `node scripts/browser_smoke.cjs`, using Playwright and system Chromium against the running local Shiny server. The script uses real selectors, file uploads, editable DT cells, local project files and browser downloads. See `browser-tests.log` for the authoritative final status and screenshots/exports in this directory.

**25 main-workflow browser checks and 7 statistical-project checks passed.** The latter separately verifies CSV/XLSX exports, saved 99% confidence results, and restoration of study, sex, method and both treatment groups. The final main workflow also confirms 30 simultaneous studies after importing 25 additional files. See `browser-statistics-project.log`.

Browser assertions cover default rendering, arbitrary study selection, subgroup filtering, three actual X interval panels, zero-safe pseudo-log configuration, PNG/SVG/PDF downloads, project selection/filter/axis restoration, explicit Welch inference, pasted/manual import, observation editing and addition, invalid-edit rollback, retained-source endpoint mapping, 25-file batch addition alongside five existing studies, and absence of JavaScript errors or external browser requests.

The shared container restricts sockets inside its command sandbox; the local Shiny server and Chromium ran outside that sandbox after automatic approval. Data used for verification is synthetic. No experimental data was sent to an external service.

## Performance measurements

`performance.csv` measures 1/2/5/14/25 studies at increasing raw row counts. On this environment, preparation of a 25-study, 160,000-row comparison took approximately **0.80 seconds**, with a canonical project object around **23.2 MiB** and cumulative profiled allocations around **153.6 MiB**. These are preparation measurements, not browser rendering latency or peak process RSS. First-call dependency initialization is included in the first scenario. Rendering many facets/legends and high-resolution exports requires additional time and memory.

No dataset-count restriction is present. Available RAM, browser rendering resources and the configurable 500 MB per-file upload/loader threshold are practical limits.

## Remaining verification limits

- Original experimental-data regression and domain-expert assessment require the original CSV and experimental-unit definitions.
- Windows/macOS installation and font behavior were not exercised here.
- A fresh restore from CRAN was not attempted; the lockfile records the actual verified binary environment, with a Conda environment export provided as an alternative.
- Very large projects beyond the profiled sizes, extreme numbers of facets, and printer-specific color/font behavior remain unprofiled.
- See `CORE_REQUIREMENTS.md` for implemented features and explicit scope limits.
