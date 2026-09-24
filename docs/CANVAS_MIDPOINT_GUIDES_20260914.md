# Canvas midpoint guides

MM, CFA, SEM and PLS-SEM share midpoint alignment while dragging one primary variable or its complete measurement bundle.

- Stationary peers on the same row define a vertical red dashed guide at their horizontal center midpoint.
- Stationary peers on the same column define a horizontal red dashed guide at their vertical center midpoint.
- The moving primary variable's center is the reference, including when indicators/errors are selected with it. Selected peers are excluded from targets.
- Existing screen-distance tolerance, paper/model zoom handling and nearest alignment selection apply. Automatic alignment snaps the whole selection; when disabled the guide remains informational.
- Drop, cancellation and Escape clear the transient layer. Both image export clone paths explicitly omit it. Model snapshots contain no guide state.

Validation: `validate_canvas_midpoint_guides.cjs` checks both axes across all four editors, group center/rigidity, zoom, automatic alignment on/off, undo, cleanup and export exclusion. Existing drag-scale and drag-cancel validations also pass.
