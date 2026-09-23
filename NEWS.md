# jaspRegression Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
>
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md)
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspModuleTemplate (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Fixed`, etc.).
> * **Issue References:** Please reference the relevant GitHub Issue (if any) at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)`).
> * **Format Categories:** >   * **Added:** New template features, QML examples, or build tools.
>   * **Changed:** Updates to default configurations, boilerplate code, or dependencies.
>   * **Fixed:** Bug fixes in the build pipeline, R wrappers, or QML layouts.
>   * **Deprecated / Removed:** Outdated template components or legacy code.


---

# jaspRegression (development version)

## Changed
* Bayesian Linear and Logistic Regression: added the model prior "Uniform over model size", which is now the default (identical to the previous default, Beta binomial with a = 1 and b = 1); "Uniform" was renamed "Uniform over models" for clarity.
* Bayesian Linear and Logistic Regression: the g-prior now defaults to g = n (the unit-information prior) instead of g = 3, with a choice between g = n and a supplied value. In Bayesian Linear Regression, the g-prior, hyper-g, hyper-g-Laplace, and hyper-g-n priors now each have their own parameter instead of one shared alpha restricted to (2, 4).

## Added
* Added a "Descriptives Plots" section to Linear Regression for visualizing predictor–outcome relationships. Continuous predictors on the horizontal axis are shown as scatter plots with regression lines; categorical (or binned continuous) predictors are shown as group-means line plots, with optional "Separate Lines"/"Separate Plots" stratification, configurable scale-predictor grouping (SD or percentile), and error bars (CI or SE).

## Fixed
* Corrected the Logistic Regression assumptions in the help and info text (frequentist and Bayesian). Removed assumptions that do not apply to logistic regression (normality of residuals, homoscedasticity, a linear dependent-variable/predictor relationship) and now state the actual assumptions: binary outcome, independent observations, linearity of the logit in the continuous predictors, and no multicollinearity.
* Fixed residual export for weighted Bayesian linear regressions using the median-probability model and custom null-model terms.
* Updated Bayesian regression model-comparison headers to identify whether Bayes factors use the null or best model as their reference.

---

# jaspModuleTemplate 0.2.0
## Added
* Added NEWS.md
* Added workflow to remind users to update their `NEWS.md`.
* Added workflow to auto-bump version when user does not do so.

---

# jaspModuleTemplate 0.1.0

## Added
* Initial examples to showcase JASP module development

## Changed
* Use best practices for checking input ([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)).
* The main results table now defaults to displaying 95% Confidence Intervals for effect sizes.

## Fixed
* Remove deprecated dependencies from qml files ([Issue #14](https://github.com/jasp-stats/jaspModuleTemplate/issues/14)).
