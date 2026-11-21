# Technical notes | Portfolio demonstration

The runnable R code is an **independent, synthetic example** showing selected modelling and validation concepts from a much larger research workflow.

## Mapping from demonstration to research concepts

| Demo element | Real-workflow concept |
|---|---|
| Repeated synthetic observations by site and day | Mobile and stationary measurements aggregated across space and time |
| Road density, vegetation and population | Static GIS predictors |
| Synthetic temperature, wind and cyclic day indicators | Daily meteorological and temporal predictors |
| `ranger` on `log(UFP)` | Log-target random-forest modelling |
| `log1p` observation-support weights | Accounting for variation in observation support |
| Out-of-bag residual smearing | Bias-aware retransformation to the concentration scale |
| Spatial k-means CV | Evaluation at geographically independent groups of sites |
| Later-date holdout | Temporal generalisation |
| Held-out synthetic sites | Simplified analogy of monitoring-station holdout |
| Predictive R², Pearson r², RMSE, MAE, bias | Complementary model diagnostics |

## Simplifications

1. All observations and coordinates are fictional; there is no real Swiss map or measurement data.
2. The demo uses only seven predictors and small forests suitable for a laptop, not the full research feature set and tuning.
3. Spatial CV clusters unique sites and keeps all of a site's days in the same fold. Spatial clusters may be imbalanced; results depend on geometry and are not guaranteed to reproduce empirical patterns.
4. The chronological demonstration uses one cut-off, not a full rolling temporal CV.
5. The leave-one-site-out example assesses three simulated sites, not the actual monitoring-network locations.
6. The demo generates data in memory; it does not reproduce operational Parquet processing or a national 25 m prediction.
7. Retransformation uses a single global factor estimated from training OOB residuals, rather than the multiple variants considered during research.

## Metric definitions

- **Predictive R²:** `1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2)`. This may be negative on held-out data.
- **Squared Pearson correlation:** `cor(observed, predicted)^2`. This tests association and is **not interchangeable** with predictive R².
- **RMSE:** Square root of mean squared prediction error.
- **MAE:** Mean absolute prediction error.
- **Bias:** Mean of `predicted - observed`.

The metrics are calculated on **held-out original-scale UFP values**. Global smearing factors are estimated from training-set out-of-bag residuals; test observations are not used to estimate them.

## No target score

The synthetic data are intended to illustrate how different holdouts answer different questions. They were not tuned to create a predetermined validation ranking. A synthetic result cannot establish the performance or validity of the research model.
