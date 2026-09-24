# Model-local aggregate scores and parcels (1.3.1-dev)

Select a latent variable with exactly one measurement indicator. The editor
button sits beside the sidebar latent-variable heading; the name and following
fields stay at the same height with or without the button. Its canvas shortcut
and properties button use the same eligibility rule. Multi-indicator and parcel
models do not show these buttons. The shortcut is hidden in result mode and
picture exports. Original items come from the currently loaded analysis data.

The editor opens as a non-modal right-side panel. Ctrl/Shift-select items from
the canvas's left observed-variable list and drag them into the original-item
area, or click Add selected items. Multiple items are accepted together and
duplicates are ignored. Selected items can be removed and reordered using the
shared analysis-list pointer drag (validated server-side permutation). Sum/mean
scoring and alpha/omega reliability use inline radio buttons. Reordering or
changing membership invalidates the preview; model application recalculates and
checks the current configuration. The underlying data remain unchanged.

## Single aggregate indicator

Select original, already reverse-coded numeric items, sum/mean scoring, and raw
Cronbach alpha or covariance-scale one-factor McDonald omega. The selected items
must reproduce the existing indicator values. Means support 80% or 100% minimum
item response; 80% requires ceiling(0.8 * item count) answered items, and uses
only those answered items for the mean. Sums and parcels remain complete-item
scores. Saved legacy definitions default to 100%; new mean definitions offer 80%.
Scores on cases below the threshold are rejected. Eligible cases with an existing
score define the analysis subset. Alpha uses pairwise available item covariances;
omega uses FIML when this subset contains incomplete rows. Sample score variance
(N−1) uses the same subset. Audit output records the threshold, analysis N,
incomplete cases, and minimum item-pair N. Applying full-scale reliability as one
common error variance to means with varying answered-item counts is explicitly
an approximation, not a missingness-specific reliability correction.

Omega fits a continuous congeneric one-factor covariance model with lavaan ML;
it is not ordinal omega, standardized-item omega, or hierarchical omega. Failed
or inadmissible fits and reliability outside (0,1) are rejected. The loading is
fixed to 1 and residual variance to (1−reliability) × score variance. This marker
scaling is retained even when the global standardized-latent option is enabled.

Calculation/preview is required before Apply. The original items, method,
scoring, N, variance and reliability are saved in the model. Reanalysis refreshes
the constraint from the current analysis cases, preserving diagram layout.
Bootstrap inference conditions on the estimated reliability constraint; it does
not propagate reliability-estimation uncertainty. Item selection does not prove
unidimensionality or validate the measurement instrument.

## Parcels

The panel now uses loading-balanced allocation for 3–5 parcels (at least two
items each). Fit the original items to a continuous one-factor CFA using ML/FIML;
rank standardized loadings from high to low, then assign rank blocks in alternating
forward/reverse parcel order. This is deterministic: variable names break ties,
and list-selection order does not change the fit or assignment. Reject inadequate
item data, nonconvergence, inadmissible solutions and mixed/nonpositive loadings
after orienting the factor. This is not a proof of unidimensionality or a search
for the best structural fit. Preview item loadings, assignments, parcel counts,
mean loadings and CFA CFI/TLI/RMSEA/SRMR, then record the review rationale.
Manual assignments in already saved models remain intact. New automatic designs
store the algorithm, loadings, fit, N and assignments; reanalysis/bootstraps keep
the stored assignment rather than searching for a new grouping.
Aggregate scores replace the existing single indicator, while structural paths
between latent variables remain intact. Extra paths/covariances attached to the
replaced indicator/error block replacement until reviewed.

Parcel item definitions are stored on nodes. Scores are generated in a local
analysis frame at each run, never written into the original data. Missing items
make their parcel missing. Existing variable-name collisions are rejected.
Reloaded models require the original item columns. Use undo to return from a newly generated parcel design to its single indicator;
the editor is deliberately unavailable on multi-indicator latent variables. Ordinary marker scaling is used for parcel
models; whole-scale alpha/omega is not imposed on every parcel.

The feature is shared by the CFA/SEM canvas. PLS/PLSc is not exposed to the
covariance-based reliability-constraint editor. Existing CFA lower-order-factor
parcel options are distinct from this actual sum/mean score workflow.

## Evidence

- `scripts/validate_canvas_scores.R`: raw alpha against the existing reliability
  engine, covariance omega, sum/mean scaling, actual SEM fits with both latent
  scaling options, invalid inputs, scope exclusions, parcel values and fitting,
  JSON persistence, reverting to a single score, current/accumulated exports.
- `scripts/validate_canvas_scores_browser.cjs`: real Shiny selection, preview,
  apply, reopen, parcel replacement, snapshot persistence and undo/redo.
- Screen audit definitions are captured in HTML/PDF/Word/HWPX/Excel exports;
  the analysis workbook also includes a numeric `Score_Definitions` sheet.

## 2026-09-20 follow-up verification

The 80% boundary (12 items: 10 accepted, 9 rejected), pairwise alpha, FIML omega,
variance, persisted rule and refresh pass on the fixture. Current and accumulated
HTML/PDF/DOCX/HWPX/XLSX preserve the additional audit fields and missing-data note.
Live browser tests pass the exact-one gate, stable sidebar field coordinates,
80%/100% radios, multi-item drag, preview/apply, parcel replacement and undo/redo.

Canvas label dictionaries are reused per language and concurrent Shiny/DOM
initialization requests are coalesced. The 194-column server HTML benchmark was
1.05 / 0.28 / 0.17 seconds before and 1.05 / 0.27 / 0.17 after. This does not
establish a meaningful menu-opening speedup; real application navigation delay
remains to be profiled. No installer was built.

## Parcel geometry and balanced allocation follow-up

Applied score/parcel nodes now use the same measurement reflow function as
ordinary indicators: centered groups, the selected placement/distance, error
positions and edge anchors. Only the target latent's measurement group is
reflowed; other groups keep their coordinates. Browser checks compare the
result against ordinary reflow for left/right/top/bottom, and verify undo/redo.
Statistical tests cover 3–5 parcels, alternating rank blocks, order invariance,
invalid inputs, a fitted SEM and serialized assignments. Both R 4.5.2/lavaan
0.6-21 and bundled R 4.5.3/lavaan 0.7-2 passed. Current and accumulated five-format
exports include the allocation audit. No installer was built.


Negative/nonfinite/out-of-range loading diagnostics now retain the complete
item-loading and fit tables instead of hiding them behind a generic reverse-code
message. The error lists affected item names and values, explicitly separates
an observed loading sign from proof of incorrect reverse scoring, and keeps
invalid automatic assignments blocked even when review is checked. No item
scores or loading signs are silently changed to bypass the diagnostic.
