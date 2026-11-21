# Case study | What makes a prediction trustworthy?

**Research context.** Ultrafine particles (UFP) vary across streets, land-use settings and weather conditions. Mobile measurement campaigns provide valuable observations but cannot continuously monitor every location and day. MARKOPOLO explored how these measurements could support spatially resolved daily prediction across Switzerland, with related work in Denmark.

This case study describes selected work I carried out in a larger, collaborative research project. The example code in this repository uses **only synthetic data**.

## 1. From field measurements to a model-ready table

I worked on R workflows that linked daily UFP observations to a spatial grid and combined these observations with daily weather, lagged weather, time indicators and static covariates such as road/traffic metrics, population and land cover. The pipeline involved raster and vector GIS, tabular joins and data-quality checks.

**Engineering decision:** To reduce repeated raster extraction, the prediction workflow separated static grid predictors and daily weather predictors into keyed Parquet tables. This made it practical to assemble predictors by location and date.

## 2. Modelling environmental concentrations

I developed and assessed random-forest workflows in `ranger`, including a logarithmic target, structured predictor filtering, support-based observation weights and retransformation to the original UFP scale. Training a model was only one component; validation and large-scale prediction required distinct workflows.

**Modelling decision:** Exponentiating a log-scale prediction does not necessarily estimate the arithmetic mean concentration. Training-based smearing factors were therefore considered in the retransformation workflow.

## 3. Choosing a meaningful validation strategy

Randomly held-out observations may occur close to training sites, or even at the same grid cells on other dates. Such validation answers a different question from estimation in genuinely unobserved places.

| Validation approach | Main question | Interpretation challenge |
|---|---|---|
| Random observation holdout | How well are new rows predicted within the sampled setting? | Repeated locations and spatial dependence can give optimistic scores |
| Spatial k-means CV | How well does the model generalise to held-out geographical groups? | Results depend on spatial layout and training coverage |
| Temporal validation | How reliably does it predict later measurements? | Temporal changes and seasonality may affect transfer |
| Monitoring-site holdout | Can the model estimate levels at an unseen fixed site? | A limited number of heterogeneous sites restricts interpretation |

**Key finding from the research work:** Geographic transfer was substantially weaker than random-split performance suggested. I investigated sampling representativeness, spatial dependence, predictor support and prediction artifacts rather than reporting the most favourable evaluation alone.

## 4. Operating at national scale

I worked on daily prediction scripts using static and meteorological Parquet tables, keyed lookups, chunked processing and date-partitioned results. This approach reduced memory pressure when working with a high-resolution national grid and many prediction dates.

## What I learned

Reliable data science means more than fitting an accurate model on conveniently held-out rows. The choice of validation must match the intended application; problematic generalisation is itself an important result to communicate.

## Boundaries and attribution

MARKOPOLO was a collaborative project. The original research involved additional partners, datasets, model settings, geospatial preprocessing and project-specific quality controls that are not reproduced here. Neither the simulated values nor the demonstration metrics constitute empirical findings from MARKOPOLO.
