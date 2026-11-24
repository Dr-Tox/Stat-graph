# StatLab portable project format v2

StatLab 2.1 and 2.2 save a UTF-8 JSON object with `format: "StatLab"` and `version: 2`. Version 1 projects from StatLab 2.0 are accepted, normalized with compatible defaults and saved as v2 on the next export. No R code or serialized environments are evaluated. Imported project files are not modified.

## Preserved structure

| Key | Contents |
|---|---|
| `next_id` | Monotonic study counter; cannot overwrite existing identities |
| `data` | Canonical raw/summary rows, including supported numeric `xaux_` source variables |
| `datasets` | Stable study IDs, original metadata, retained source tables and endpoint/unit mapping provenance |
| `selected` | Active study IDs; visibility is separate |
| `settings.view` | Endpoint, filters and hidden study IDs |
| `settings.axes` | Existing scales/limits/ticks/interval rows, independent-Y/zero/grid choices, typography and break styling |
| `settings.graph` | Existing graph fields plus typography, appearance, stable dataset styles, highlights, annotation definitions, thresholds, p-value formatting and export dimensions |
| `settings.statistics` | Last explicitly executed study/endpoint/stratum/groups/method/control/alpha/adjustment |
| `analysis_context` | Validated engine settings, exact group-pair metadata, observation/result fingerprints and result snapshot |
| `presets` | Named reusable graph/axis configurations; old presets are normalized on use |
| `results` | Last explicit numerical result table, with null-only columns preserved |
| `reproducibility` | Creation/save information, R/package versions and loaded-format version |

Root fields and settings not recognized by this release are retained opaquely when projects are saved. They are not executed or silently converted into known settings. Resetting known properties preserves unknown settings. Newer project-format versions are rejected. Unrecognized scientific observation columns are also rejected explicitly: silently ignoring an experimental factor could pool biologically different groups.

## Numeric precision and validation

Version 2 writes numeric values with 17-digit precision. Optional unused numeric fields and automatic limits use JSON `null`; normalization restores typed missing values. Dataset references, required numerical observations, summary SD/n validity, duplicate summary groups, ID counters, axes, intervals and graph/annotation settings are checked before an imported project replaces session state.

A project/upload has the existing configurable 500 MB per-file safety threshold. There is no limit based on study or interval count.

## Stable visual identity

`dataset_styles` is an array of `{dataset_id, label, color}` objects. Colors follow immutable study IDs, not legend positions. Missing overrides use a deterministic ID-based palette color. Version 1 automatic palette assignments are captured during migration when their original grouping is identifiable. Unique custom display labels appear verbatim; duplicate labels include study IDs. Compatible v1 positional colors migrate to ID overrides or `legacy_group_colors` keys containing explicit canonical grouping fields. If the old group composition cannot be identified, a migration notice is shown; the app does not guess.

`symbol_styles` and `line_styles` store explicit field/value keys and style values, so filtering cannot shift sex/subgroup symbols or line patterns. Compatible legacy positional lists migrate to these keys.

`highlight` stores study IDs. `settings.view.hidden` hides studies without deleting observations or deactivating their study records. Registry order and active selection determine visual order; style changes do not rewrite scientific data.

## Annotation definitions

`settings.graph.annotations` is an array with these fields:

- `id`, `type` (`asterisk`, `letter`, `text`, `bracket`, `pvalue`), `source` (`manual` or `automatic`), `label_mode`.
- Exact `dataset_id`, `endpoint`, `sex`, `generation`, `exposure`, `collection_time`, `subgroup`, and `treatment`. Brackets also have `treatment2`.
- Optional `dose` / `dose2` disambiguate repeated treatment names. Dose does not substitute for the biological identity fields.
- `text`, optional numerical `p`, optional original-unit `y`, original-unit `dx`/`dy`, `size` in mm, `color`, `face`, `visible`, `line_width`, and transformed-range-fraction `tip`.
- `evidence`: JSON text containing the validated analysis context for automatically generated rows. It is parsed as data only.

Manual assignments can be entered without any statistical engine results. Their source is identified in the figure note. Editing an automatic label or biological anchor through the manager converts it to manual; purely positional/style edits keep its evidence.

Automatic asterisks and p-labels retain numerical p-values and regenerate display text from current thresholds/formatting. Compact letters retain the valid all-pairs family and alpha used for their generation. An omnibus or control-only family cannot silently create a complete CLD. A data fingerprint mismatch hides stale automatic labels and produces a diagnostic. Version 1 result tables without provenance need an explicit analysis rerun before automatic annotation.

Resolved positions are derived at render time. Original X anchors (`x1`, `x2`) and visual group positions (`display_x1`, `display_x2`) remain separate. Dodging, log transforms and split-panel clipping do not rewrite dose values. Segmented bracket geometry and facet metadata are derived from the anchored definition. PNG/SVG/PDF share the same annotated ggplot layers.

## Compatibility behavior

Existing graph fields retain their names. Independent text properties and appearance fields receive defaults when absent. Version 1 source tables, mappings, results, metadata and presets remain available. The legacy single `annotation` string remains a user-provided corner label; it is not silently assigned to an experimental group.

Settings are saved only after validation. Invalid live drafts are not serialized as if they were applied figures. Undo/redo history is session-local and not included in the project; scientific observations are never part of graph undo.

## StatLab 2.2 interface compatibility

The guided interface uses the existing fields: study checkboxes update `selected`, `settings.view.hidden` and `settings.graph.highlight`; treatment checkboxes update `settings.statistics.groups`. No second selection or statistical settings model was introduced. Source labels use retained `datasets.*.metadata.source` and legacy synthetic-example notes; studies without recorded provenance are labeled as unknown rather than assigned a filename. Presentation rounding, hidden columns and collapsed UI sections do not change project observations or exported numerical results.
