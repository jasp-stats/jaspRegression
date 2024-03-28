Generalized Linear Models
===
A generalized linear model (GLM) is a flexible extension of ordinary linear regression. A widely used GLM is binary logistic regression. Generally speaking, a GLM consists of a **random component** and a **systematic component**:

- The random component specifies an appropriate probability distribution for the response variable. For example, a binomial distribution is appropriate for proportions of a total.
- The systematic component specifies how the explanatory variables relate to the mean of the response. For example, in binary logistic regression, the logit link function is used to map the responses (i.e. probabilities) to the linear combination of predictors (i.e. *linear predictor*).

### Family and link
The following table summarized the available distributions (also called families) and link functions, as well as the suitable type of response data. The asterisk * indicates the canonical/default link function for a specific family.

| Family           | Links                                               | Response Data       |
|------------------|-----------------------------------------------------|---------------------|
| Bernoulli        | Logit*, Probit, Cauchit, Complementary Log-Log, Log | Binary              |
| Binomial         | Logit*, Probit, Cauchit, Complementary Log-Log, Log | Proportions, Counts |
| Gaussian         | Identity*, Log, Inverse                             | Continuous          |
| Gamma            | Identity, Log, Inverse*                             | Positive continuous |
| Inverse Gaussian | Identity, Log, Inverse, 1/mu^2*                     | Positive continuous |
| Poisson          | Identity, Log*, Square-root                         | Counts              |

### Assumptions
- Lack of outliers: All responses were generated from the same process, so that the same model is appropriate for all the observations.
- Independence: The responses are independent of each other.
- Distribution: The responses come from the specified EDM.
- Link function: The correct link function is used.
- Linearity: All important explanatory variables are included, and each explanatory variable is included in the linear predictor on the correct scale.
- Variance function: The correct variance function is used.
- Dispersion parameter: The dispersion parameter is constant.

### Input and Output
#### Variables
- Dependent variable: The response variable.
- Covariates: Quantitative variables, such as age, height and IQ.
- Factors: Qualitative variables, such as gender and group memberships.
- Weights: Prior weights of the model. Mandatory when the binomial family is selected. In this case, the name changes to "Total Number of Trials", and the dependent variable now refers to the proportion of success (between 0 and 1).
- Family: Distribution for the response variable.
- Link: Link function.

#### Model
- Components and model terms
  - Components: All the independent variables and covariates that can be included in the model.
  - Model terms: The terms representing the factors and covariates included in the model. By default, only the main effects are included.
  - Add to null model: Each model term can be added to the null model.
- Include intercept: Selected by default. This adds an intercept term to the model.

#### Statistics
- Default Model Summary Table (always given)
  - Model: The different hypotheses (i.e. null vs. alternative) that are compared.
  - Deviance: -2*log(likelihood).
  - AIC: Akaike Information Criterion.
  - BIC: Bayesian Information Criterion.
  - df: The degree of freedom of the corresponding model.
  - X2: The chi-squared statistic used to compare the model (H1) against the null model (H0).
  - p: The p-value of the chi-squared test.
- Model Fit: A table providing information about the goodness-of-fit of the model, including the corresponding fit statistic, the degree of freedom (df) and the p-value.
  - Deviance goodness-of-fit: Goodness-of-fit test based on deviance residuals, comparing the current model against the saturated model.
  - Pearson goodness-of-fit: Goodness-of-fit test based on Pearson residuals, comparing the current model against the saturated model.  
- Parameter Estimates:
  - Estimates: Ticked by default. This gives a table summarizing the model's parameter estimates, standard error around the parameter estimates, the test statistic (t or z), and the p-value.
  - Confidence intervals: Provide the confidence intervals around the parameter estimates. The level of the confidence intervals can be specified (default is 95%).

#### Diagnostics
- Analysis of Residuals: The following options provide visual diagnostics of the model assumptions based on residuals.
  - Deviance Residuals: Provide different types of diagnostic plots based on deviance residuals.
    - Residuals vs. fitted: A scatter plot of the standardized deviance residuals against the fitted values (with constant-information scale transformations).
    - Residuals vs. predictor: Scatter plots of the standardized deviance residuals against every predictor on its original scale.
    - Q-Q plot: Q-Q plot of the standardized deviance residuals.
  - Pearson Residuals: Provide different types of diagnostic plots based on Pearson residuals.
    - Residuals vs. fitted: A scatter plot of the standardized Pearson residuals against the fitted values (with constant-information scale transformations).
    - Residuals vs. predictor: Scatter plots of the standardized Pearson residuals against every predictor on its original scale.
    - Q-Q plot: Q-Q plot of the standardized Pearson residuals.
  - Quantile Residuals: Provide different types of diagnostic plots based on quantile residuals. Highly recommended for discrete families (e.g. Bernoulli, Binomial, Poisson)
    - Residuals vs. fitted: A scatter plot of the standardized quantile residuals against the fitted values (with constant-information scale transformations).
    - Residuals vs. predictor: Scatter plots of the standardized quantile residuals against every predictor on its original scale.
    - Q-Q plot: Q-Q plot of the standardized quantile residuals.
  - Other Plots
    - Partial residual plots: Partial residual plots across predictors.
    - Working responses vs. linear predictor: A scatter plot of the model's working responses z against the predicted linear predictor values.
- Show outliers: A table showing the top n cases, ranked descendingly based on the size of the residuals. Note that the shown cases are not necessarily outliers. The column "Case Number" refers to the row number of the observation in the data set.
  - Top n standardized quantile residuals
  - Top n standardized deviance residuals
  - Top n studentized deviance residuals
- Residuals:
  - Casewise diagnostic: Casewise and summarized diagnostics for the residuals.
    - Standard residual > 3: Outliers outside x standard deviations: Display diagnostics for cases where the absolute value of the standardized residual is larger than x; default is x=3.
    - Cook's distance > 1 : Display diagnostics for cases where the value of Cook’s distance is larger than x; default is x = 1. 
    - All cases: Display diagnostics for all cases.
  - Cases are marked as influential in the table, according to the following thresholds:
    - DFBETAS: When the absolute value of DFBETAS is greater than 1.      
    - DFFITS: When the absolute value of DFFITS is greater than 3 * sqrt(k/(n-k)) where k refers to the number of parameters in the model and n refers to the sample size.
    - Covariance ratio: When the covariance ratio is greater than 3 * k/(n-k).
    - Cook's distance: When Cook's distance exceeds the 50th percentile of the F distribution with (k, n-k) degrees of freedom.
    - Leverages: When the leverages are greater than 3 * k/n.
  - Append residuals to data: Save the residuals of the most complex model as a new column in the data file.
- Multicollinearity: A table showing multicollinearity diagnostics of the model. The choices of measures are as follows.
  - Tolerance
  - VIF: Variance Inflation Factor.

#### Estimated Marginal Means and Contrast Analysis
- Model variables: Variables that can be used for computing estimated marginal means.
- Selected variables: Variables for which the estimated marginal means will be computed.
- Confidence interval: Width/level of the confidence interval for the estimated marginal means.
- Levels of covariates at mean +/- SD: What should be the "levels" of continuous variables (expressed in standard deviations) for which are the estimated marginal means computed. For instance, 1 means that the estimated marginal means for a covariate would be given at three values of the covariate, namely 1 SD below the mean, the mean, and 1 SD above the mean.
- Compare marginal means to: Value to which will be the estimated marginal means compared. The default is 0.
- Use response scale: Decide whether the estimated marginal means should be computed on the response scale or untransformed linear scale. The former is selected by default.
- Specify contrasts: Create a table for specifying contrasts based on the estimated marginal means. The row indices correspond to column "Level" in the estimated marginal means output table. Columns with variable names contain the (combinations of) variables levels for each estimated marginal mean. Columns named "Contrast 1, 2, ..." are used for specifying the contrasts. To set a contrast between two marginal means, enter -1 and 1 to the corresponding rows. Interactions can be tested by specifying differences between the changes in marginal means of one variable across levels of another variable.
- P-value adjustment: Only available when contrasts are specified. To correct for multiple comparison testing and avoid Type I errors, different methods for correcting the p-value are available:
  - Holm
  - Multivariate-t
  - Scheffe
  - Tukey
  - None
  - Bonferroni
  - Hommel

#### Advanced Options
- Set seed: This gives you the possibility of setting a random seed for the plots and tables based on quantile residuals in the Diagnostics Section. Concretely, every time you generate a plot or a table based on quantile residuals, you get a slightly different plot or table because one step in the calculation of the quantile residuals involves drawing random values from a distribution. By setting a random seed of your choice here (the default is 1), you make sure that you can get exactly the same plots and tables based on quantile residuals. 


### Referneces
---
Dunn, P. K., & Smyth, G. K. (2018). *Generalized linear models with examples in R*. New York: Springer.

### R Packages
---
- stats
- statmod
- emmeans
- ggplot2
