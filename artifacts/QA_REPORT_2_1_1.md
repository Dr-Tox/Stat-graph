# StatLab 2.1.1 patch verification

## Scope

Three reproduced rendering defects are fixed without changing observations, project format, dependencies or statistical calculations:

1. Boxplots and violins now group by protected series and the actual selected X value. Different exposure/collection times no longer collapse into one distribution. The minimum violin observation count uses the same groups.
2. A custom error-bar color retains the horizontal grouping offsets and uncertainty bounds of the default layer.
3. Constant positive responses on Log10 Y axes receive multiplicative automatic bounds (±0.05 log10 units). Explicit limits still override automatic bounds.

## Executed checks

- The new regression file was run against the unmodified implementation first: **30 failed assertions, 47 passing**, plus two invalid-log warnings. See `regressions-before-2.1.1.log`.
- After correction, `Rscript tests/run_tests.R` passed **48 blocks / 463 assertions**, **0 failures, 0 errors, 0 warnings**. This includes the previous 386 assertions plus 77 new regression assertions. See `tests-2.1.1.log` and `test-results.csv`.
- Tests cover exposure and collection-time distributions, both boxplots and violins, per-time singleton rejection, colored grouped errors across linear/log10/pseudo-log X with sex facets and separate controls, constant positive responses at 0.01/1/1000 with normal/reversed Log10 Y, and unchanged source observations.
- `Rscript scripts/verify_review_fixes.R` produced **nine PNG/SVG/PDF figures** across the three regression cases. All nine files passed size checks. The exposure and constant-log PNGs were visually inspected. See `exports-2.1.1.log` and `review-fixes-2.1.1/`.
- `R/statistical_methods.R` is unchanged from v2.1.0. No statistical methods or dependencies were modified.

## Verification boundary

This patch was verified in the existing Linux R environment. Browser workflows were not rerun for this patch; previous 2.1 browser results remain historical evidence, not new patch verification. The fixes operate in the shared ggplot rendering pipeline. Windows/macOS and the original experimental CSV remain unverified. Synthetic examples are not experimental findings.
