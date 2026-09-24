# Repeated matrix validation in ordinal probability calculations — 2026-09-14

Read-only runtime review and isolated instrumentation; no production optimization was applied. Installed packages are polycor 0.8.2 and mvtnorm 1.3.3.

## Confirmed call structure

`polycor::polychor()` calls internal `binBvn()` repeatedly during optimization and Hessian calculation. Each `binBvn()` invocation builds one 2×2 correlation matrix from the current rho and evaluates each ordinal cell using `mvtnorm::pmvnorm()`. Each probability call runs `checkmvArgs()`, which calls `chkcorr()` for the matrix. Bounds differ between cells, but the matrix is constant inside that `binBvn()` invocation.

Installed `chkcorr()` checks matrix shape, strips dimension names, coerces storage mode, checks the min/max range and checks the diagonal with `all.equal()`. It is only one part of input validation; lower/upper bounds, means and other arguments must still be validated. The correlation parameter changes between optimizer evaluations, so a general unconditional 'already checked' flag would be incorrect.

## Instrumented evidence

Six 10,000-row synthetic ordinal pairs: two, three or five categories, each independent or associated. Each complete fit uses the application's `ML=FALSE, std.err=TRUE` setting. Table counts cover the whole single-pair fit, including uncertainty calculation.

| Categories | Pair | Matrix checks | Exactly same as immediately previous matrix | Share |
| --- | --- | ---: | ---: | ---: |
| 2 | Independent | 112 | 87 | 77.7% |
| 2 | Associated | 168 | 134 | 79.8% |
| 3 | Independent | 288 | 260 | 90.3% |
| 3 | Associated | 396 | 362 | 91.4% |
| 5 | Independent | 675 | 651 | 96.4% |
| 5 | Associated | 1,100 | 1,066 | 96.9% |

The observer compares the entire matrix with `identical(..., num.eq=FALSE)` and still invokes the original check on every call. It resets its retained matrix and counters for each pair. The counts show redundant inputs; they are **not speedup percentages**. All probability calls and RNG operations remain intact.

## Result preservation and implementation boundary

For all six pairs, the instrumented and original complete polychor results matched exactly, including rho, thresholds, variance output, diagnostics, captured stdout and RNG state. Coefficients were checked finite. Functions were cloned into isolated environments, and the cloned `binBvn()` references the cloned probability function; installed package namespaces were not patched.

A concrete next candidate is a single-entry cache of the original `chkcorr()` result, valid only for an exactly identical matrix and only after a quiet successful validation. It must retain bounds/mean checks, every probability evaluation, RNG calls and the original integration algorithm. It would require guarded integration with package internals, malformed-input/condition coverage, direct and full-analysis equivalence tests, and timing/memory measurements before adoption. None of those cache benefits is established by this counting experiment.

No source code in `R/`, statistical formulas, analysis/export output or installer changed. No performance or memory claim is made in this review. Artifacts in `output/ordinal-probability-review-20260914/` include installed function captures, `audit.R`, package versions, check counts and complete result snapshots.
