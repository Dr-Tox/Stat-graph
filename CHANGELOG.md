# Changelog

## 2.2.0 — Guided data and comparison workflows

- Replace read-only study booleans with direct Compare/Show/Highlight checkboxes. Retain search, arbitrary study counts, ordering and batch actions.
- Label synthetic examples and recorded imported sources in comparison, data management and statistics.
- Separate Import Data from Manage Studies. Add numbered steps, raw/summary guidance, downloadable templates, synthetic source previews and a confirmation that resets when import settings change.
- Keep optional column mappings, study metadata and explicit unit conversions expandable. Prevent the unsupported summary-wide combination in the UI and validate it server-side.
- Keep source/precision intact while hiding technical/irrelevant columns by default; all columns and existing editing/endpoint-harmonization functions remain accessible.
- Replace statistical treatment chips with labeled checkboxes, preview selection before inference, compact readable tables and expandable details/diagnostics. Keep single-study/stratum safeguards and every existing statistical method.
- Remove the obsolete hidden selection widget that could overwrite checkbox changes. Restore filters explicitly while bindings synchronize, then accept user changes normally.
- Preserve project format v2, presets, all graph/axis/annotation settings and dependencies. Statistical calculation source is unchanged.


## 2.1.1 — Graph correctness fixes

- Keep boxplot and violin distributions separate at each selected X value, including exposure duration and collection time. Validate violin sample counts at that same grouping level.
- Preserve grouped-bar error positions and intervals when assigning a custom error-bar color.
- Expand constant positive responses multiplicatively on Log10 Y axes, avoiding invalid negative automatic limits.
- Add regressions for both time axes, raw-data integrity, custom error colors across linear/log/pseudo-log X scales and facets, and constant responses on normal/reversed Log10 Y axes.

## 2.1 — Advanced Scientific Graph Editor

This release extends the existing StatLab modules; it does not replace the comparison architecture or statistical engine.

### Graph Editor

- Expandable Category → Property → Advanced settings controls in a collapsible, responsive properties panel.
- Live validated styling, independent main/subtitle typography, independent axis-title/tick styling, unit placement, margins, backgrounds, grid/axis/border controls and separate error-bar thickness.
- Stable study-ID colors, editable study labels, keyed subgroup symbol/line tables, and migration of compatible 2.0 positional color/symbol/line assignments to stable keys.
- Clean Scientific, Journal Black & White, Color Publication and Presentation starting presets. Existing saved user presets remain supported.
- Individual-property and section reset, default reset, and up to 100 graph/axis undo and redo snapshots. Scientific observations are never included in graph undo.

### Scientific annotations

- Persistent user-provided asterisks, letters, p-values, group labels and comparison brackets with exact study/endpoint/stratum/treatment anchors, optional dose disambiguation and original-unit offsets.
- Editable annotation table, bulk pasted manual letter assignments, independent deletion/visibility/formatting and automatic placement above data/error bars.
- Automatic brackets and p-values use verified existing pairwise results. Compact letters require a complete justified all-pairs family; omnibus and control-only Dunnett results are rejected for CLDs.
- Configurable strict p-value cutoffs, live reformatting from retained p-values, 1–17 significant digits, decimal/scientific/threshold label formats, and optional threshold notes.
- Analysis provenance fingerprints, retained result snapshots, stale-evidence rejection, explicit manual-source notes and no reconstruction of observations.
- Segmented brackets continue across visible X intervals, with break marks; labels remain facet- and study-specific. Bar/distribution labels follow visual group offsets without changing stored dose coordinates.

### Comparison and axes

- Searchable, paginated study manager replaces visible chip lists. Activate/deactivate, hide/unhide, highlight, search-result selection and display reordering have no dataset-count cap.
- Grouped, scrollable Plotly legends, quick legend collapse, separate subgroup identification and optional static legend omission.
- Editable interval table with add/remove/move/boundary/width controls and validation. Invalid interval drafts leave the last valid figure unchanged.
- Existing linear, log10, pseudo-log, separate-zero-control, faceting, raw/summary and independent-Y capabilities remain available. Faceted split-axis Plotly figures restore one shared, independently styled title per axis.
- Numeric preparation is cached separately from typography, palette, annotation and other cosmetic settings.

### Projects, precision and verification

- Project format v2, with v1 loading and compatible defaults. Unknown settings/root fields survive roundtrips and resets of known properties.
- Observation schema extensions that cannot be interpreted safely are rejected explicitly instead of being silently ignored or pooled.
- v2 JSON uses 17-digit numeric precision. Statistical result columns with null values are preserved.
- Original 2.0 source archive and fresh baseline results are retained. New R, browser, export, screenshot and performance checks are included.
- No statistical methods were added or changed. `R/statistical_methods.R` is byte-identical to the 2.0 baseline.

See `RELEASE_2_1.md` and `artifacts/QA_REPORT_2_1.md` for verified capabilities and limitations.
