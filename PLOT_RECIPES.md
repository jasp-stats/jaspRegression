# Plot recipe migration

The first pass migrates the existing vector-only drawing boundaries. Plot dimensions,
labels, dependencies, statistical preparation, and error handling stay in the analysis.
The initial drawing is validated inside the existing `try()` blocks, then discarded;
only the recipe is stored. Drawing helpers are resolved from the installed namespace.

## Migrated

- Classical linear regression: residuals versus dependent, covariates, and predicted
  values; partial regression plots (including their confidence/prediction intervals);
  residual histograms, including standardized histograms; residual Q-Q plots.
- GLM residual Q-Q plots: residual computation (including quantile simulation)
  stays before recipe creation, and only realized residuals and interval settings
  are retained.
- Classical Pearson, Spearman, and Kendall correlation heatmaps: coefficient,
  p-value, ordering, and significance-label preparation stays in the analysis.
- Bayesian linear and logistic regression: posterior coefficient summaries, residuals
  versus predictions under BMA, cumulative model probabilities, model complexity,
  marginal inclusion probabilities, and residual Q-Q plots.
- Five shared drawing helpers replace duplicated Bayesian linear/logistic rendering.
  Fitted values, residuals, probabilities, and coefficient intervals are computed
  before recipe creation. No BAS model or formula is passed into these recipes.

## Postponed and suggested boundaries

| Family | Suggested next step |
| --- | --- |
| Correlation matrix and pair plots, classical and Bayesian | Prepare a plain description of each cell (scatter data, marginal density, numerical correlation/interval, posterior curve), then build every cell and `ggMatrixPlot` inside a single recipe. Callers currently arrange and inspect ggplots, so changing only the leaf return values would break composition. |
| Bayesian correlation prior/posterior, robustness, sequential plots | Separate BF computation and progress updates from rendering. Freeze the computed curve/annotation values; materialize panel drawings only inside the composite recipe. |
| Other GLM diagnostics | Extract residuals and prediction/partial-residual coordinates during analysis; pass realized numeric values and family/label settings to the drawing helper. Keep randomness outside redraws. |
| Linear marginal plots | Extract prediction grids and intervals from the fitted model in `.linregFillMarginalPlots`, then move the model-free drawing portion into a recipe; preserve interaction and factor handling. |
| Linear descriptives and response optimizer | Compute grouped summaries or optimizer/prediction coordinates and freeze realized jitter before recipe creation. Build themes, subplot lists, and the matrix arrangement inside the final drawing helper. |
| Classical logistic conditional estimates, residual diagnostics, independent/predicted, ROC and PR curves | Separate model prediction, residual extraction, cutoff selection, and predictor grids from geoms/scales in the existing `*PlotFill` helpers; store curves and label coordinates, never a glm model. |
| Bayesian posterior log odds | Replace the stored closure over a BAS object with precomputed model log odds and labels; add a pure base-graphics drawing function or reproduce the existing BAS plot from plain coordinates. |
| Bayesian coefficient posterior distributions | Separate the mixture distribution and point-mass calculations from the drawing block. Store grids, densities, intervals, and scalar annotations; construct plotmath inside the drawer. |

The module intentionally still contains ordinary plots in these deferred families.
No backwards compatibility shim is necessary: `renv.lock` pins recipe-capable
jaspBase, jaspGraphs, and jaspTools. Existing plot snapshots must remain unchanged.

## Validation

Used R-4.5.2 with an isolated installation of the modified module, the module's
existing dependency library, and an override containing the locked recipe core
revisions. jaspBase `b187388a`, jaspGraphs `288d2751`, and jaspTools `1a109435`
match the lockfile. Required dependencies of jaspTools were already present in the
lockfile; unrelated package pins remain unchanged.

- Seventeen representative SVGs match the original master drawing functions
  exactly under identical dependencies and seeds.
- Focused tests cover real producers, recipe serialization, mathematical beta
  labels, prepared heatmap labels/order, and deterministic redraw/RNG behavior.
- The full tracked test suite, including examples, completed with 732 passing
  assertions, one existing correlation-rank snapshot failure, and one test that
  inspected a recipe as a ggplot. The latter test now materializes the recipe;
  its complete Bayesian regression test file passes (58 assertions, one existing
  structural-fallback warning and one intentional matrix-plot skip).
- The original installed module under the same dependencies gives 666 passing
  assertions and the same correlation-rank snapshot failure, with no errors.
- Graphics deprecation warnings and existing structural snapshot fallbacks remain.
  Additional focused checks expose existing deprecated drawing calls; recipes
  also emit existing histogram `size` warnings on redraw. No snapshots were
  accepted or changed.

Tests ran in copied `/tmp` trees so snapshot cleanup could not touch user files.
CI should still verify the locked package restore and platform-specific rendering.
