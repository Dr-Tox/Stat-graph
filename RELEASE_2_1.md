# StatLab 2.1 implementation status

## Implemented

- Existing local modular Shiny architecture and all prior independent-study comparison/statistical/data-import capabilities.
- Unlimited user-selected dataset and interval counts at the software level; practical memory/rendering limits remain.
- Hierarchical Graph Editor with reactive validated edits, collapsing properties, responsive layout and shared graph configuration.
- Independent title/subtitle visibility, content, family, size, face, color, alignment and spacing; independent axis titles/units/tick typography; automatic metadata labels with retained manual overrides.
- Stable study colors, custom labels, point/line/error styles, alpha/highlights, appearance/margins, grouped interactive legends and optional static legends.
- Searchable study manager, arbitrary selection, activation, hiding, highlighting and registry/visual ordering.
- Editable interval table with any number of rows. Move actions edit the draft; rendered intervals must remain ascending, non-overlapping and nonempty. Invalid drafts do not replace valid axes.
- Manual asterisks, arbitrary letters, free group labels, p-value labels and within-study/stratum brackets. All annotations have exact biological anchors, independent editing, visibility, offsets and style.
- Manual letter table import; missing anchor fields are inferred only when they uniquely identify a single experimental stratum.
- Automatic labels from supported pairwise t-test, Tukey, Dunnett, Mann–Whitney and adjusted Dunn results. Valid complete all-pairs CLDs are supported. ANOVA/Kruskal omnibus tests require a justified post-hoc step before pair annotations.
- Automatic provenance/data checks, configurable thresholds, p-label formats and alpha-family-aware CLDs. Automatic labels are not inferred from arbitrary imported p tables.
- Split/log/pseudo-log/faceted annotations and segmented continuation brackets. Original and visually offset coordinates are separate.
- Four editable built-in styles; reusable user presets; individual-property/section reset; graph-only undo/redo.
- Project v1 migration and v2 precision/persistence, including annotation evidence, presets and stable colors. Unknown settings remain opaque and preserved.
- Reproducible synthetic complex figures, R tests, browser scripts, screenshots and performance measurements.

## Deliberate boundaries and pending refinements

- Brackets connect two treatments in one study, endpoint and complete experimental stratum. Cross-study inference, cross-stratum brackets and cross-facet spanning connectors are not generated. Separate anchored labels can be used for those presentation needs.
- A group annotation needs a unique X coordinate for its anchored group. Arbitrary-source scatter plots with varying within-group X values require a grouped X variable or a different annotation definition; the app explains and suppresses ambiguous anchors.
- Placement avoids error bars and stacks nearby labels, but dense publications can still require manual original-unit offsets and Y positions. There is no universal collision optimizer or drag-to-position canvas.
- A bracket continuation is a segmented visual connector, not a dose-response interpolation through omitted X regions. User-defined Y limits can crop annotations; increase those limits when needed.
- Font availability depends on installed OS fonts. The verified environment supports the supplied Greek Unicode and italic examples. Journal-specific certified templates, rich text within one label and arbitrary font uploads are not included.
- Interactive and static typography use different rendering engines. Data coordinates, anchors, ranges, breaks, style settings and panel organization are shared; text wrapping and physical pixel layout are not promised to be identical between Plotly and Cairo/SVG.
- Existing 2.0 corner labels are retained under the legacy control. Prefer the anchored manager for study-specific significance labels.
- A positional color list can be migrated only when its original group composition is identifiable. Otherwise the app retains it and explains the mismatch rather than guessing colors. New UI entries are converted to stable keys.
- Unsupported scientific observation-schema fields are rejected explicitly. Unknown non-scientific root/settings fields are preserved without execution or interpretation.
- Undo history is session-local, limited to the latest 100 configuration snapshots and excludes scientific data. It is not an observation-edit undo system or a full project-history archive.
- Arbitrary additional independently named experimental-factor columns, heterogeneous wide summary layouts and the other 2.0 scope limits remain as documented in `CORE_REQUIREMENTS.md`.
- Paired, repeated-measures, nested/mixed models and nonlinear fitting remain outside this iteration.
- No original experimental CSV was supplied; original-data numerical regression and domain-expert validation remain unverified. Linux is the tested OS; Windows/macOS font/rendering behavior needs separate testing.
