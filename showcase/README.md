# Research workflow showcase | Adapted original R scripts

This folder provides **two substantive code samples** adapted from the actual MARKOPOLO research workflow. Unlike the independently written [synthetic demonstration](../run_demo.R), the scripts here show the scale and structure of project-specific GIS processing and model validation.

| Script | Scope | Particularly relevant code |
|---|---|---|
| [master_dataset_pipeline.R](master_dataset_pipeline.R) | Roughly 3,100 lines | 25 m grid setup, daily measurement aggregation, weather alignment, raster covariate extraction, segment-level dataset creation |
| [random_forest_validation.R](random_forest_validation.R) | Roughly 2,600 lines | Random-forest training, feature filtering, observation-based weights, random/spatial/temporal validation, diagnostics, Duan-style bias correction |

## What was changed for publication?

- Replaced internal absolute file paths with neutral input/output locations derived from `MARKOPOLO_SHOWCASE_DATA_ROOT`.
- Removed institutional directory structure, named source-file releases, author-specific path comments and obsolete internal collaboration notes.
- Replaced some project-specific output filenames with descriptive neutral names.
- Disabled the optional research-area buffer filter by default because the corresponding research geometry is not supplied.
- Added public-facing headers and clarified the scripts' scope.

**The code retains research-specific data schemas and workflow assumptions, and has not been re-run after these adaptations.** The neutral path names are documentation placeholders, not a downloadable data package. Some sections rely on preprocessed project-specific rasters and tables, which are intentionally excluded.

## What is *not* here?

No original mobile observations, station measurements, location-level outputs, GIS rasters, internal trained models, datasets from research partners, or numeric research results are uploaded. This is an example of **code structure and methodological reasoning**, not a fully reproducible research release.

The source research code existed in a larger collaborative environment. These public adaptations highlight selected work and do not imply sole authorship of the full project or permission to redistribute third-party datasets.

## Where to start

Recruiters seeking a short executable example should use [`run_demo.R`](../run_demo.R) in the root directory. For evidence of how the analysis was implemented at project scale, skim the opening configuration and processing stages of `master_dataset_pipeline.R`, followed by the training, validation, and diagnostic sections of `random_forest_validation.R`.

### Configuration for institutional reuse (not tested)

The scripts refer to:

```r
project_data_root <- Sys.getenv("MARKOPOLO_SHOWCASE_DATA_ROOT", unset = "")
input_root <- file.path(project_data_root, "inputs")
output_root <- file.path(project_data_root, "outputs")
```

This does **not** mean they will run without modification: actual input schemas, permitted data access, raster naming conventions and dependencies must be checked first. To review the code as a portfolio sample, no setup or execution is required.
