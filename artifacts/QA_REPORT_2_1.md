# StatLab 2.1 verification

Verified on Linux x86-64, R 4.5.3, system Chromium and Playwright. Exact versions are in [runtime-21.txt](runtime-21.txt); runtime dependencies remain pinned in `renv.lock` and `environment.yml`. `digest` was already installed/pinned and is now declared directly. No new runtime package versions were introduced. Browser QA additionally uses Node/Playwright; visual comparisons use Python/Pillow and `pdftotext`.

## Baseline and regression

Before implementation, the unmodified StatLab 2.0 application passed **19 R test blocks / 239 assertions**, **25 main browser checks**, and **7 statistical/project browser checks**. Logs, prior browser scripts and the original statistical engine are preserved in [baseline-2.0](baseline-2.0/).

The extended application passed **45 R test blocks / 386 assertions**, with **0 failures and 0 errors**, using `Rscript tests/run_tests.R`. See [tests-2.1.log](tests-2.1.log) and [test-results.csv](test-results.csv). The original test assertions remain included. Coverage includes 1/2/5/14/25/35 studies, arbitrary subsets, raw/summary safeguards, all existing methods and unsupported-method rejection, endpoint/stratum protection, zero controls, log/pseudo-log, split-panel coordinates, stable study colors and subgroup symbols/lines, exact annotation identities, offsets/dodging, omitted-anchor rejection, configurable thresholds, automatic evidence validation, complete CLDs and incomplete-family rejection, typography/settings persistence, v1 migration, unknown settings preservation, history/reset and PNG/SVG/PDF rendering.

`R/statistical_methods.R` remains byte-identical to the baseline, SHA-256:
`c7613a2b44ded6ec807d23724b1d3d1bf099d67890ddd5b9fce25b3ba07a6e30`.

## Browser verification

- [browser-editor-21.log](browser-editor-21.log): **43 checks passed**, including expanding/collapsing categories; live independent typography; unchanged numeric preparation counts for cosmetic edits (complex figure: 3 → 3); study search/hide/unhide/highlight/reorder; interval add/edit/remove and invalid-draft rejection; manual letters without analysis; manual stars with exact study/sex/treatment anchors; automatic numerical p-values; omnibus rejection; valid all-pairs Tukey CLDs; v1 reopening; v2 serialization/restoration; small-screen fit; zero JavaScript errors and zero external browser requests.
- [browser-statistics-project-2.1.log](browser-statistics-project-2.1.log): **7 checks passed**, including actual CSV/XLSX downloads, 99% confidence results and exact study/sex/method/group restoration.
- [browser-render-21.log](browser-render-21.log): **9 checks passed** in a final focused render regression after correcting Plotly subplot axis-title loss verifies shared X/Y labels and typography, annotations, downloads, project reopening and JavaScript errors.
- The original main comparison workflow passed **25 checks**, exercised by `scripts/browser_smoke.cjs`, adapted to the visible study manager and hierarchical controls. See [browser-tests-2.1.log](browser-tests-2.1.log) for its final result.

All browser tests execute actual Shiny interactions, uploads and downloads. Earlier development failures involved readiness/navigation and annotation-table pagination; the final workflows use real visible manager controls and explicit readiness conditions. No pass is inferred from a screenshot alone.

## Complex figures and visual regression

`Rscript scripts/generate_advanced_examples.R` generates a synthetic four-study project with male/female facets, three unequal-width X intervals, SD errors, manual significance letters/Unicode, manual asterisks, a validated Welch comparison bracket, customized typography and independent axis labels. X uses pseudo-log and Y log10. Automatic comparisons retain exact study/stratum and numeric p-values; manual labels are explicitly identified as user-provided.

- [Advanced PNG](advanced-21.png), [SVG](advanced-21.svg), [PDF](advanced-21.pdf), [resolved original anchors](advanced-21-anchors.csv).
- Browser-generated [PNG](editor-browser-21.png), [SVG](editor-browser-21.svg), [PDF](editor-browser-21.pdf) and [saved project](editor-21-project.json).
- `python scripts/verify_visuals_21.py`: **20 checks passed**. The complex static PNG is pixel-identical after project reopening; the before/after browser chart is also pixel-identical (RMS 0, changed pixels 0). SVG and extracted PDF text each preserve a single customized main title, both sex facets, letters, Greek Unicode, asterisks, axis titles and split-range labels. See [visual-regression-21.log](visual-regression-21.log) and [machine-readable metrics](visual-regression-21.json).

Static/interactive output shares annotated ggplot panel objects. These checks establish project-roundtrip reproducibility and vector-content consistency; they do not assert identical pixel layout between PDF/SVG/Plotly renderers or certify arbitrary OS fonts.

## Screenshots

- [Hierarchical Graph Editor](editor-hierarchy-21.png)
- [Independent title controls](editor-title-controls-21.png) and [live edited title](editor-titles-21.png)
- [Manual annotation manager](editor-manual-annotations-21.png)
- [Annotated split-axis figure](editor-annotated-split-21.png)
- [Automatic all-pairs compact letters](editor-automatic-cld-21.png)
- [Responsive workspace](editor-responsive-21.png)
- [30-study comparison](comparison-30-studies.png)

## Performance

[performance-editor-21.csv](performance-editor-21.csv) records preparation, figure construction, Plotly conversion, three cached cosmetic edits, prepared-object size and cumulative R allocations for 1/2/5/14/25/35 studies. At 35 studies / 2,240 raw observations: preparation 0.052 s, figure construction 0.151 s, Plotly conversion 0.592 s, three cached style builds 0.328 s, prepared object 0.142 MiB. Cumulative allocations cover the measured operations (21.53 MiB); they are not peak RSS. First-run initialization and machine load affect timings. This does not establish unlimited computational capacity. No study-count cap was added.

## Remaining verification boundaries

No original animal CSV was provided. Real-data numerical regression and scientist usability review remain pending. Linux is verified; Windows/macOS font and rendering behavior are untested. Dense annotation collision handling is assisted rather than universal; users may need offsets or higher Y limits. Data-edit undo, autosave, drag-to-position annotation placement, arbitrary new factor columns and certified journal templates remain outside this release. See [RELEASE_2_1.md](../RELEASE_2_1.md) for precise supported and pending scope.
