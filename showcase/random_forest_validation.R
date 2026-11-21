# ==================================================================
# MARKOPOLO | Sanitized research code showcase
# Random-forest training and validation
# ==================================================================
# Adapted from a larger collaborative project for public code review.
# Real research processing logic is retained; this is NOT the runnable
# synthetic demo in ../run_demo.R. Original data and research models are
# not provided. Paths and some comments have been generalized.
# Requires separately authorized inputs and the original source schemas.
# This adapted copy has not been independently re-executed.
# ==================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(ranger)
  library(lubridate)
  library(tidyr)
  library(readr)
  library(sf)
})


# DATA CONFIGURATION (showcase).
# These filenames are illustrative placeholders, not an input package.
# Update to an authorized data release and verify schema compatibility.
project_data_root <- Sys.getenv("MARKOPOLO_SHOWCASE_DATA_ROOT", unset = "")
if (!nzchar(project_data_root)) {
  stop("Research showcase: set MARKOPOLO_SHOWCASE_DATA_ROOT to authorized input files.")
}
input_root <- file.path(project_data_root, "inputs")
output_root <- file.path(project_data_root, "outputs")
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

# -------------------- USER INPUTS --------------------
master_parquet <- file.path(input_root, "master_dataset.parquet")
set.seed(40)

# -------------------- OUTPUT / TOGGLES --------------------
run_validation_suite <- TRUE

# Optional summary CSV
write_summary_csv <- TRUE
out_summary_csv   <- file.path(output_root, "rf_validation_summary.csv")

# Optional final full-data model fit + save
save_final_model <- TRUE
out_model_rds    <- file.path(output_root, "random_forest_model.rds")

# Variable importance output for final saved model
write_vi_csv <- TRUE
out_vi_csv   <- file.path(output_root, "variable_importance.csv")

# Transform the target variable into Log with optional offset
log_transform_target <- TRUE
log_offset           <- 0

# Filter out variables above a certain correlation threshold
enable_redundancy_filter <- FALSE
corr_threshold           <- 0.90
corr_method              <- "pearson"

# Filter out target variable values above a certain cutoff
filter_target_upper_enable <- FALSE
filter_target_upper_value  <- 149999

# ============================================================
# OPTIONAL ROW FILTER: KEEP ONLY MASTER ROWS INSIDE AN SNC BUFFER GPKG
# ============================================================
# This restricts the modelling dataset to rows whose centroid_x / centroid_y
# falls inside the supplied SNC buffer outline.

filter_rows_to_snc_buffer <- FALSE  # Opt in only with the authorized spatial buffer input

snc_buffer_gpkg <- file.path(input_root, "optional_spatial_buffer_500m.gpkg")

snc_buffer_layer <- NA_character_
snc_buffer_coord_crs_epsg <- 2056
snc_buffer_x_col <- "centroid_x"
snc_buffer_y_col <- "centroid_y"
snc_buffer_point_chunk_n <- 250000L

# Optional exception:
# Keep NABEL rows even if their coordinates fall outside the SNC buffer.
snc_buffer_keep_nabel_outside <- TRUE
snc_buffer_nabel_col <- "NABEL_site"
snc_buffer_nabel_value <- 1L

# K-means settings
kmeans_k       <- 5L
random_split_p <- 0.80

# Variable importance filtering
enable_vi_filter        <- FALSE
vi_mode_for_filter      <- "permutation"   # recommended: "permutation"
vi_filter_type          <- "top_n"      # "absolute", "relative", or "top_n"
vi_cutoff_absolute      <- 0.001
vi_cutoff_relative      <- 0.05            # keep vars >= 5% of max importance
vi_top_n                <- 50L
vi_min_keep             <- 10L             # safeguard
write_vi_filter_csv     <- TRUE
out_vi_filter_csv       <- file.path(output_root, "variable_filter.csv")


# -------------------- BACK-TRANSFORM / SMEARING OPTIONS --------------------
# Smearing only applies when log_transform_target = TRUE.
# If log_transform_target = FALSE, the script automatically evaluates raw-scale predictions only.

eval_log_scale_metrics        <- TRUE   # Also report metrics on log target scale when log_transform_target = TRUE
eval_naive_backtransform      <- TRUE   # exp(pred_log) - offset
eval_duan_smearing            <- TRUE   # exp(pred_log) * smear_factor - offset

# "global" = one Duan factor for all predictions
# "pred_bin" = grouped smearing by predicted log-UFP quantile bins
smearing_methods <- c("global", "pred_bin")

smearing_n_bins <- 10L

# For split/CV validation, estimate smearing from training-set OOB residuals where possible.
# This is preferred over in-sample fitted residuals.
smearing_use_oob_residuals <- TRUE

# Save smearing info inside final model RDS when save_final_model = TRUE.
save_smearing_info <- TRUE

# -------------------- VARIABLE SPLIT WEIGHTS --------------------
enable_split_select_weights <- TRUE

# Default weight for all non-road predictors
split_weight_default_value <- 1.00

# Road-type equalization weights based on seasonal covered-road sums,
# scaled so that the highest road-type weight = 1.
#
# Original raw weights:
# motorway    = 0.590
# primary     = 1.035
# secondary   = 1.637
# tertiary    = 1.854
# residential = 0.841
#
# Scaled by max(raw weights) = 1.854
# road_type_split_weights <- c(
#   motorway    = 0.318,
#   primary     = 0.558,
#   secondary   = 0.883,
#   tertiary    = 1.000,
#   residential = 0.454
# )

# Example training-run road-type weights (must be re-estimated for new data).

road_type_split_weights <- c(
  motorway    = 0.256862,
  primary     = 0.490607,
  secondary   = 0.800125,
  tertiary    = 1.000000,
  residential = 0.564342
)

# Optional generic road-family penalty.
# Keep at 1.00 for road-type equalization only.
overall_road_family_penalty <- 1.00

# Generic road-related variables without road type.
# Keep at 1.00 unless you also want to downweight generic road variables.
generic_road_weight <- 1.00

# -------------------- CASE WEIGHTS: GRID-LEVEL MEASUREMENT SUPPORT --------------------
enable_case_weights <- TRUE

# Column from the master dataset:
# total number of raw measurements in this grid across the full campaign
case_weight_source_col <- "n_obs_grid_total"

# Name used internally in df_small
case_weight_col <- "case_weight"

# Recommended: "log1p" or "sqrt"
# - "raw"   = strongest weighting, probably too aggressive
# - "sqrt"  = moderate
# - "log1p" = conservative, recommended first test
case_weight_transform <- "log1p"   # "raw", "sqrt", or "log1p"

# Normalize to mean 1.
# This is generally better than scaling 0–1 for ranger case.weights.
normalize_case_weights_mean1 <- TRUE

# Optional cap after normalization to prevent very heavily measured grids from dominating
cap_case_weights <- TRUE
case_weight_cap_value <- 5

# -------------------- MODEL PARAMETERS --------------------
target <- "UFP"
factor_cols <- c()

num.trees         <- 500
mtry              <- 8
min.node.size     <- 429
max.depth         <- 21
replace           <- TRUE
sample.fraction   <- 0.969425888
splitrule         <- "variance"
num.random.splits <- 3
respect_unordered <- "order"
na_mode           <- "na.omit"
num.threads       <- max(1, parallel::detectCores() - 1)

# -------------------- PREDICTOR LIST --------------------
features <- c(
  "altitude_m",
  "t2m","d2m","u10","v10","sp","tp","sf","tcc","blh","RH_percent","uvb","ssrd",
  "t2m_lag1d","d2m_lag1d","u10_lag1d","v10_lag1d","sp_lag1d","tp_lag1d","sf_lag1d","tcc_lag1d","blh_lag1d","RH_percent_lag1d","uvb_lag1d","ssrd_lag1d",
  "weekday0_6", "doy0_365", "year",       # cyclical variables: "weekday_sin","weekday_cos","day_sin","day_cos"
  "Gas","Diesel","EV",
  "NDVI_buf25m","NDVI_buf250m","NDVI_buf500m","NDVI_buf1000m",
  "pop_buf50m","pop_buf100m","pop_buf250m",
  "Heating_no_pollution_r250","Heating_no_pollution_r500","Heating_no_pollution_r1000",
  "Heating_aux_pollution_r250","Heating_aux_pollution_r500","Heating_aux_pollution_r1000",
  "Heating_average_pollution_r250","Heating_average_pollution_r500","Heating_average_pollution_r1000",
  "Heating_high_pollution_r250","Heating_high_pollution_r500","Heating_high_pollution_r1000",
  "roads_motorway_den_km2_r25","roads_motorway_den_km2_r50","roads_motorway_den_km2_r100",
  "roads_primary_den_km2_r25","roads_primary_den_km2_r50","roads_primary_den_km2_r100",
  "roads_secondary_den_km2_r25","roads_secondary_den_km2_r50","roads_secondary_den_km2_r100",
  "roads_tertiary_den_km2_r25","roads_tertiary_den_km2_r50","roads_tertiary_den_km2_r100",
  "roads_residential_den_km2_r25","roads_residential_den_km2_r50","roads_residential_den_km2_r100",
  "urban_fabric_frac_r250","urban_fabric_frac_r500","urban_fabric_frac_r1000",
  "industry_frac_r250","industry_frac_r500","industry_frac_r1000",
  "commercial_frac_r250","commercial_frac_r500","commercial_frac_r1000",
  "airports_frac_r2500","airports_frac_r5000","airports_frac_r10000",
  "ports_frac_r2500", "ports_frac_r5000", "ports_frac_r10000",
  "mine_dump_and_construction_sites_frac_r250","mine_dump_and_construction_sites_frac_r500","mine_dump_and_construction_sites_frac_r1000",
  "artificial_green_frac_r250","artificial_green_frac_r500","artificial_green_frac_r1000",
  "arable_frac_r250","arable_frac_r500","arable_frac_r1000",
  "perm_crops_frac_r250","perm_crops_frac_r500","perm_crops_frac_r1000",
  "pastures_frac_r250","pastures_frac_r500","pastures_frac_r1000",
  "hetero_agri_frac_r250","hetero_agri_frac_r500","hetero_agri_frac_r1000",
  "forests_frac_r250","forests_frac_r500","forests_frac_r1000",
  "shrub_frac_r250","shrub_frac_r500","shrub_frac_r1000",
  "bare_frac_r250","bare_frac_r500","bare_frac_r1000",
  "wetlands_frac_r250","wetlands_frac_r500","wetlands_frac_r1000",
  "inland_waters_frac_r250","inland_waters_frac_r500","inland_waters_frac_r1000",
  "distance_to_chiasso","distance_to_major_road","car_meters_gauss","wood_burning",
  "motorway_asp_mean_r25","motorway_asp_mean_r50","motorway_asp_mean_r100",
  "primary_asp_mean_r25","primary_asp_mean_r50","primary_asp_mean_r100",
  "secondary_asp_mean_r25","secondary_asp_mean_r50","secondary_asp_mean_r100",
  "tertiary_asp_mean_r25","tertiary_asp_mean_r50","tertiary_asp_mean_r100",
  "residential_asp_mean_r25","residential_asp_mean_r50","residential_asp_mean_r100",
  "motorway_msp_mean_r25","motorway_msp_mean_r50","motorway_msp_mean_r100",
  "primary_msp_mean_r25","primary_msp_mean_r50","primary_msp_mean_r100",
  "secondary_msp_mean_r25","secondary_msp_mean_r50","secondary_msp_mean_r100",
  "tertiary_msp_mean_r25","tertiary_msp_mean_r50","tertiary_msp_mean_r100",
  "residential_msp_mean_r25","residential_msp_mean_r50","residential_msp_mean_r100",
  "motorway_dtv_mean_r25","motorway_dtv_mean_r50","motorway_dtv_mean_r100",
  "primary_dtv_mean_r25","primary_dtv_mean_r50","primary_dtv_mean_r100",
  "secondary_dtv_mean_r25","secondary_dtv_mean_r50","secondary_dtv_mean_r100",
  "tertiary_dtv_mean_r25","tertiary_dtv_mean_r50","tertiary_dtv_mean_r100",
  "residential_dtv_mean_r25","residential_dtv_mean_r50","residential_dtv_mean_r100",
  "highway_binary",
  "svf_brut",
  "centroid_x", "centroid_y",
  "NABEL_site", "Airview"
)



# -------------------- SIMPLE FEATURE-NAME FILTER --------------------
# Removes predictors whose names contain one or more specified strings.
#
# Examples:
#   exclude_feature_strings <- c("500")
#     removes all predictors containing "500"
#
#   exclude_feature_strings <- c("500", "1000")
#     removes all predictors containing either "500" or "1000"
#
#   exclude_feature_strings <- c("_lag1d")
#     removes all lagged variables
#
# Matching is literal by default, not regex-based.

enable_feature_name_filter <- TRUE

exclude_feature_strings <- c(
  "dtv_mean_r25", "dtv_mean_r100",
  "asp_mean_r25", "asp_mean_r50", "asp_mean_r100",
  "msp_mean_r25", "msp_mean_r50", "msp_mean_r100",
  "den_km2_r25", "den_km2_r50",
  "frac_r250", "frac_r500",
  "pollution_r250", "pollution_r500",
  "pop_buf50m", "pop_buf100m",
  "NDVI_buf25m", "NDVI_buf250m", "NDVI_buf500m",
  "ssrd", "d2m",
  "highway_binary",
  "distance_to_chiasso",
  "Gas","Diesel","EV"
)

if (isTRUE(enable_feature_name_filter)) {
  
  # Remove empty strings defensively
  exclude_feature_strings <- exclude_feature_strings[
    !is.na(exclude_feature_strings) &
      nzchar(exclude_feature_strings)
  ]
  
  if (!length(exclude_feature_strings)) {
    message("\nFeature-name filter ACTIVE, but no valid exclusion strings were supplied.")
    
  } else {
    
    features_before_name_filter <- features
    
    # TRUE for every feature matching at least one exclusion string
    drop_feature <- Reduce(
      `|`,
      lapply(
        exclude_feature_strings,
        function(txt) grepl(txt, features, fixed = TRUE)
      )
    )
    
    dropped_name_features <- features[drop_feature]
    features <- features[!drop_feature]
    
    message("\nFeature-name filter ACTIVE")
    message(
      "  Exclusion strings: ",
      paste(exclude_feature_strings, collapse = ", ")
    )
    message("  Features before: ", length(features_before_name_filter))
    message("  Features after:  ", length(features))
    message("  Dropped:         ", length(dropped_name_features))
    
    if (length(dropped_name_features)) {
      message("  Dropped features:")
      message(
        paste0(
          "    - ",
          dropped_name_features,
          collapse = "\n"
        )
      )
    } else {
      message("  No feature names matched the exclusion strings.")
    }
  }
}

# -------------------- FEATURE GROUPS --------------------
weather_vars <- c(
  "t2m","d2m","u10","v10","sp","tp","sf","tcc","blh","RH_percent","uvb","ssrd",
  "t2m_lag1d","d2m_lag1d","u10_lag1d","v10_lag1d","sp_lag1d","tp_lag1d","sf_lag1d",
  "tcc_lag1d","blh_lag1d","RH_percent_lag1d","uvb_lag1d","ssrd_lag1d"
)

time_vars <- c(
  "weekday_sin","weekday_cos","day_sin","day_cos"
)

fuel_vars <- c(
  "Gas_share","Diesel_share","HEV_share","PHEV_share","BEV_share"
)

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))

mae <- function(y, yhat) mean(abs(y - yhat), na.rm = TRUE)

bias <- function(y, yhat) mean(yhat - y, na.rm = TRUE)

r2 <- function(y, yhat) {
  ok <- is.finite(y) & is.finite(yhat)
  y <- y[ok]
  yhat <- yhat[ok]
  if (length(y) < 2) return(NA_real_)
  denom <- sum((y - mean(y))^2)
  if (!is.finite(denom) || denom == 0) return(NA_real_)
  1 - sum((y - yhat)^2) / denom
}

safe_exp_backtransform <- function(x, offset = 0) {
  out <- exp(x) - offset
  out[out < 0] <- 0
  out
}

score_metrics <- function(y_true, y_pred) {
  data.frame(
    rmse = rmse(y_true, y_pred),
    mae  = mae(y_true, y_pred),
    bias = bias(y_true, y_pred),
    r2   = r2(y_true, y_pred),
    stringsAsFactors = FALSE
  )
}

make_pred_bins <- function(x, n_bins = 10L) {
  qs <- stats::quantile(
    x,
    probs = seq(0, 1, length.out = n_bins + 1L),
    na.rm = TRUE,
    type = 7
  )
  
  qs <- unique(as.numeric(qs))
  
  if (length(qs) < 3L) {
    return(NULL)
  }
  
  qs[1] <- -Inf
  qs[length(qs)] <- Inf
  qs
}

compute_smearing_info <- function(y_log, pred_log_train,
                                  method = "global",
                                  n_bins = 10L) {
  ok <- is.finite(y_log) & is.finite(pred_log_train)
  y_log <- y_log[ok]
  pred_log_train <- pred_log_train[ok]
  
  if (length(y_log) == 0L) {
    stop("No valid rows available for smearing calculation.")
  }
  
  resid_log <- y_log - pred_log_train
  
  global_smear <- mean(exp(resid_log), na.rm = TRUE)
  
  if (!is.finite(global_smear)) {
    stop("Global smearing factor is not finite.")
  }
  
  if (method == "global") {
    return(list(
      method = "global",
      global_smear = global_smear,
      n_rows = length(y_log)
    ))
  }
  
  if (method == "pred_bin") {
    breaks <- make_pred_bins(pred_log_train, n_bins = n_bins)
    
    if (is.null(breaks)) {
      warning("Could not create prediction bins for grouped smearing. Falling back to global smearing.")
      return(list(
        method = "global",
        global_smear = global_smear,
        n_rows = length(y_log),
        fallback_reason = "insufficient_unique_prediction_values"
      ))
    }
    
    bin <- cut(
      pred_log_train,
      breaks = breaks,
      include.lowest = TRUE,
      right = TRUE
    )
    
    tab <- data.frame(
      pred_bin = bin,
      exp_resid = exp(resid_log)
    )
    
    smear_tab <- tab |>
      dplyr::group_by(pred_bin) |>
      dplyr::summarise(
        smear_factor = mean(exp_resid, na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      )
    
    smear_tab$pred_bin <- as.character(smear_tab$pred_bin)
    
    return(list(
      method = "pred_bin",
      global_smear = global_smear,
      breaks = breaks,
      smear_table = smear_tab,
      n_rows = length(y_log),
      n_bins_requested = n_bins
    ))
  }
  
  stop("Unknown smearing method: ", method)
}

apply_backtransform_mode <- function(pred_raw, is_log, offset,
                                     pred_type = "raw_or_naive",
                                     smearing_info = NULL) {
  if (!isTRUE(is_log)) {
    return(pred_raw)
  }
  
  if (pred_type == "log_scale") {
    return(pred_raw)
  }
  
  if (pred_type == "naive") {
    return(safe_exp_backtransform(pred_raw, offset = offset))
  }
  
  if (pred_type %in% c("duan_global", "duan_pred_bin")) {
    if (is.null(smearing_info)) {
      stop("smearing_info is required for Duan back-transformation.")
    }
    
    if (pred_type == "duan_global") {
      out <- exp(pred_raw) * smearing_info$global_smear - offset
      out[out < 0] <- 0
      return(out)
    }
    
    if (pred_type == "duan_pred_bin") {
      if (!identical(smearing_info$method, "pred_bin")) {
        warning("Grouped smearing info unavailable. Falling back to global Duan smearing.")
        out <- exp(pred_raw) * smearing_info$global_smear - offset
        out[out < 0] <- 0
        return(out)
      }
      
      pred_bin <- cut(
        pred_raw,
        breaks = smearing_info$breaks,
        include.lowest = TRUE,
        right = TRUE
      )
      
      pred_bin_chr <- as.character(pred_bin)
      
      sf <- smearing_info$smear_table$smear_factor[
        match(pred_bin_chr, smearing_info$smear_table$pred_bin)
      ]
      
      # Fallback for empty/unmatched bins
      sf[!is.finite(sf)] <- smearing_info$global_smear
      
      out <- exp(pred_raw) * sf - offset
      out[out < 0] <- 0
      return(out)
    }
  }
  
  stop("Unknown pred_type: ", pred_type)
}

get_training_predictions_for_smearing <- function(rf, train_df, use_oob = TRUE) {
  pred <- NULL
  
  if (isTRUE(use_oob)) {
    pred <- try(as.numeric(rf$predictions), silent = TRUE)
    if (inherits(pred, "try-error")) pred <- NULL
  }
  
  if (is.null(pred) || length(pred) != nrow(train_df) || any(!is.finite(pred))) {
    warning("OOB predictions unavailable/incomplete for smearing. Falling back to in-sample training predictions.")
    pred <- as.numeric(predict(rf, data = train_df)$predictions)
  }
  
  pred
}

score_predictions_multi <- function(y_obs_transformed, pred_raw,
                                    is_log, offset,
                                    smearing_infos = NULL,
                                    scheme_name,
                                    n_train,
                                    n_test) {
  out_list <- list()
  
  if (isTRUE(is_log) && isTRUE(eval_log_scale_metrics)) {
    met <- score_metrics(y_obs_transformed, pred_raw)
    met$scheme <- scheme_name
    met$pred_type <- "log_scale"
    met$target_scale <- "log"
    met$n_train <- n_train
    met$n_test <- n_test
    out_list[["log_scale"]] <- met
  }
  
  if (!isTRUE(is_log)) {
    met <- score_metrics(y_obs_transformed, pred_raw)
    met$scheme <- scheme_name
    met$pred_type <- "raw"
    met$target_scale <- "original"
    met$n_train <- n_train
    met$n_test <- n_test
    out_list[["raw"]] <- met
  } else {
    y_true_orig <- safe_exp_backtransform(y_obs_transformed, offset = offset)
    
    if (isTRUE(eval_naive_backtransform)) {
      pred_naive <- apply_backtransform_mode(
        pred_raw,
        is_log = TRUE,
        offset = offset,
        pred_type = "naive"
      )
      
      met <- score_metrics(y_true_orig, pred_naive)
      met$scheme <- scheme_name
      met$pred_type <- "naive_backtransform"
      met$target_scale <- "original"
      met$n_train <- n_train
      met$n_test <- n_test
      out_list[["naive"]] <- met
    }
    
    if (isTRUE(eval_duan_smearing) && !is.null(smearing_infos)) {
      if ("global" %in% names(smearing_infos)) {
        pred_duan_global <- apply_backtransform_mode(
          pred_raw,
          is_log = TRUE,
          offset = offset,
          pred_type = "duan_global",
          smearing_info = smearing_infos[["global"]]
        )
        
        met <- score_metrics(y_true_orig, pred_duan_global)
        met$scheme <- scheme_name
        met$pred_type <- "duan_global"
        met$target_scale <- "original"
        met$n_train <- n_train
        met$n_test <- n_test
        out_list[["duan_global"]] <- met
      }
      
      if ("pred_bin" %in% names(smearing_infos)) {
        pred_duan_bin <- apply_backtransform_mode(
          pred_raw,
          is_log = TRUE,
          offset = offset,
          pred_type = "duan_pred_bin",
          smearing_info = smearing_infos[["pred_bin"]]
        )
        
        met <- score_metrics(y_true_orig, pred_duan_bin)
        met$scheme <- scheme_name
        met$pred_type <- "duan_pred_bin"
        met$target_scale <- "original"
        met$n_train <- n_train
        met$n_test <- n_test
        out_list[["duan_pred_bin"]] <- met
      }
    }
  }
  
  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  
  out[, c(
    "scheme", "pred_type", "target_scale",
    "n_train", "n_test",
    "rmse", "mae", "bias", "r2"
  )]
}

select_features_by_vi <- function(df, target, predictor_vec,
                                  importance_mode = "permutation",
                                  filter_type = "relative",
                                  cutoff_absolute = 0.001,
                                  cutoff_relative = 0.05,
                                  top_n = 50L,
                                  min_keep = 10L) {
  
  if (length(predictor_vec) == 0L) {
    stop("predictor_vec is empty in select_features_by_vi().")
  }
  
  form_vi <- make_form(target, predictor_vec)
  
  rf_vi <- fit_ranger(form_vi, df, importance_mode = importance_mode)
  
  vi_obj <- rf_vi$variable.importance
  if (is.null(vi_obj)) {
    stop("Variable importance could not be extracted from ranger model.")
  }
  
  vi_df <- data.frame(
    variable   = names(vi_obj),
    importance = as.numeric(vi_obj),
    stringsAsFactors = FALSE
  )
  
  vi_df <- vi_df[order(vi_df$importance, decreasing = TRUE), , drop = FALSE]
  vi_df$rank <- seq_len(nrow(vi_df))
  
  max_imp <- suppressWarnings(max(vi_df$importance, na.rm = TRUE))
  
  if (!is.finite(max_imp)) {
    stop("Variable importance values are not finite.")
  }
  
  if (filter_type == "absolute") {
    keep_vars <- vi_df$variable[vi_df$importance >= cutoff_absolute]
    
  } else if (filter_type == "relative") {
    thr <- max_imp * cutoff_relative
    keep_vars <- vi_df$variable[vi_df$importance >= thr]
    
  } else if (filter_type == "top_n") {
    keep_n <- max(1L, min(as.integer(top_n), nrow(vi_df)))
    keep_vars <- vi_df$variable[seq_len(keep_n)]
    
  } else {
    stop("Unknown filter_type: ", filter_type)
  }
  
  # safeguard
  if (length(keep_vars) < min_keep) {
    keep_vars <- vi_df$variable[seq_len(min(min_keep, nrow(vi_df)))]
  }
  
  vi_df$kept <- vi_df$variable %in% keep_vars
  
  list(
    keep_vars = keep_vars,
    vi_df = vi_df,
    model = rf_vi
  )
}

make_case_weights <- function(data) {
  if (!isTRUE(enable_case_weights)) {
    return(NULL)
  }
  
  if (!case_weight_source_col %in% names(data)) {
    stop(
      "Case weights are enabled, but source column '",
      case_weight_source_col,
      "' is not present in the modelling data."
    )
  }
  
  x <- data[[case_weight_source_col]]
  
  if (!is.numeric(x)) {
    x <- suppressWarnings(as.numeric(x))
  }
  
  if (any(!is.finite(x))) {
    stop("Case weight source column contains non-finite values.")
  }
  
  if (any(x < 0)) {
    stop("Case weight source column contains negative values.")
  }
  
  if (all(x == 0)) {
    stop("All case weight source values are zero.")
  }
  
  if (case_weight_transform == "raw") {
    w <- x
  } else if (case_weight_transform == "sqrt") {
    w <- sqrt(x)
  } else if (case_weight_transform == "log1p") {
    w <- log1p(x)
  } else {
    stop("Unknown case_weight_transform: ", case_weight_transform)
  }
  
  # Avoid exact zero weights unless you intentionally want to exclude rows
  min_positive <- min(w[w > 0], na.rm = TRUE)
  w[!is.finite(w) | w <= 0] <- min_positive
  
  if (isTRUE(normalize_case_weights_mean1)) {
    w <- w / mean(w, na.rm = TRUE)
  }
  
  if (isTRUE(cap_case_weights)) {
    w <- pmin(w, case_weight_cap_value)
    
    # Re-normalize after capping so average weight remains ~1
    if (isTRUE(normalize_case_weights_mean1)) {
      w <- w / mean(w, na.rm = TRUE)
    }
  }
  
  if (any(!is.finite(w))) {
    stop("Final case weights contain non-finite values.")
  }
  
  if (any(w < 0)) {
    stop("Final case weights contain negative values.")
  }
  
  if (sum(w) <= 0) {
    stop("Final case weights sum to zero.")
  }
  
  message(
    "Case weights enabled from column '", case_weight_source_col,
    "' using transform = ", case_weight_transform,
    ". Weight range: ",
    sprintf("%.3f", min(w, na.rm = TRUE)),
    " to ",
    sprintf("%.3f", max(w, na.rm = TRUE)),
    "; mean = ",
    sprintf("%.3f", mean(w, na.rm = TRUE))
  )
  
  w
}
fit_ranger <- function(form, data, importance_mode = "none") {
  
  predictor_vec <- all.vars(form)[-1L]
  
  # Variable-level split-selection weights, if enabled
  split_w <- make_split_select_weights(predictor_vec)
  
  # Row-level case weights, if enabled
  case_w <- make_case_weights(data)
  
  ranger(
    formula = form,
    data = data,
    num.trees = num.trees,
    mtry = min(mtry, length(all.vars(form)) - 1L),
    min.node.size = min.node.size,
    max.depth = max.depth,
    replace = replace,
    sample.fraction = sample.fraction,
    splitrule = splitrule,
    num.random.splits = num.random.splits,
    respect.unordered.factors = respect_unordered,
    importance = importance_mode,
    oob.error = TRUE,
    na.action = na_mode,
    split.select.weights = split_w,
    case.weights = case_w,
    num.threads = num.threads,
    seed = 42
  )
}

score_split <- function(train_df, test_df, form, target, is_log, offset,
                        scheme_name = "Split") {
  rf <- fit_ranger(form, train_df, importance_mode = "none")
  
  pred_test_raw <- as.numeric(predict(rf, data = test_df)$predictions)
  
  smearing_infos <- NULL
  
  if (isTRUE(is_log) && isTRUE(eval_duan_smearing)) {
    pred_train_for_smear <- get_training_predictions_for_smearing(
      rf = rf,
      train_df = train_df,
      use_oob = smearing_use_oob_residuals
    )
    
    smearing_infos <- list()
    
    if ("global" %in% smearing_methods) {
      smearing_infos[["global"]] <- compute_smearing_info(
        y_log = train_df[[target]],
        pred_log_train = pred_train_for_smear,
        method = "global",
        n_bins = smearing_n_bins
      )
    }
    
    if ("pred_bin" %in% smearing_methods) {
      smearing_infos[["pred_bin"]] <- compute_smearing_info(
        y_log = train_df[[target]],
        pred_log_train = pred_train_for_smear,
        method = "pred_bin",
        n_bins = smearing_n_bins
      )
    }
  }
  
  score_predictions_multi(
    y_obs_transformed = test_df[[target]],
    pred_raw = pred_test_raw,
    is_log = is_log,
    offset = offset,
    smearing_infos = smearing_infos,
    scheme_name = scheme_name,
    n_train = nrow(train_df),
    n_test = nrow(test_df)
  )
}

time_block <- function(label, expr) {
  t0 <- proc.time()[["elapsed"]]
  value <- force(expr)
  dt <- proc.time()[["elapsed"]] - t0
  message(sprintf("[TIMER] %s: %.2f min (%.2f h)", label, dt / 60, dt / 3600))
  list(value = value, elapsed_sec = dt)
}

make_form <- function(target, predictor_vec) {
  safe_terms  <- sprintf("`%s`", predictor_vec)
  safe_target <- sprintf("`%s`", target)
  stats::as.formula(paste(safe_target, "~", paste(safe_terms, collapse = " + ")))
}

get_vi <- function(model) {
  try_primary <- try(model$variable.importance, silent = TRUE)
  if (!inherits(try_primary, "try-error") && !is.null(try_primary)) {
    return(try_primary)
  }
  try_perm <- try(ranger::importance(model, type = "permutation"), silent = TRUE)
  if (!inherits(try_perm, "try-error") && !is.null(try_perm)) {
    return(try_perm)
  }
  try_imp <- try(ranger::importance(model, type = "impurity"), silent = TRUE)
  if (!inherits(try_imp, "try-error") && !is.null(try_imp)) {
    return(try_imp)
  }
  NULL
}

read_first_gpkg_layer_if_needed <- function(gpkg_path, layer = NA_character_) {
  if (!file.exists(gpkg_path)) {
    stop("GPKG does not exist: ", gpkg_path)
  }
  
  if (is.na(layer) || !nzchar(layer)) {
    layers <- sf::st_layers(gpkg_path)$name
    if (!length(layers)) stop("No layers found in GPKG: ", gpkg_path)
    layer <- layers[1]
    message("SNC buffer layer was NA; using first layer: ", layer)
  }
  
  sf::st_read(gpkg_path, layer = layer, quiet = TRUE)
}

filter_df_to_snc_buffer <- function(
    df,
    gpkg_path,
    layer = NA_character_,
    x_col = "centroid_x",
    y_col = "centroid_y",
    coord_crs_epsg = 2056,
    point_chunk_n = 250000L,
    keep_nabel_outside = TRUE,
    nabel_col = "NABEL_station",
    nabel_value = 1L
) {
  if (!all(c(x_col, y_col) %in% names(df))) {
    stop(
      "SNC buffer row filter requires coordinate columns: ",
      paste(c(x_col, y_col), collapse = ", ")
    )
  }
  
  point_chunk_n <- as.integer(point_chunk_n)
  if (length(point_chunk_n) != 1L || is.na(point_chunk_n) || point_chunk_n < 1L) {
    stop("snc_buffer_point_chunk_n must be one integer >= 1.")
  }
  
  n_before <- nrow(df)
  x <- suppressWarnings(as.numeric(df[[x_col]]))
  y <- suppressWarnings(as.numeric(df[[y_col]]))
  valid_xy <- is.finite(x) & is.finite(y)
  
  nabel_exception <- rep(FALSE, n_before)
  
  if (isTRUE(keep_nabel_outside)) {
    if (!nabel_col %in% names(df)) {
      warning(
        "SNC buffer NABEL exception is enabled, but column '",
        nabel_col,
        "' was not found. No outside-buffer NABEL rows will be rescued."
      )
    } else {
      nabel_raw <- df[[nabel_col]]
      
      if (is.logical(nabel_raw)) {
        nabel_exception <- !is.na(nabel_raw) & nabel_raw
      } else {
        nabel_num <- suppressWarnings(as.numeric(nabel_raw))
        nabel_exception <- is.finite(nabel_num) & nabel_num == as.numeric(nabel_value)
      }
    }
  }
  
  message("\n============================================================")
  message("SNC buffer row filter ACTIVE")
  message("GPKG: ", gpkg_path)
  message("Coordinate columns: ", x_col, ", ", y_col)
  message("Coordinate CRS: EPSG:", coord_crs_epsg)
  message("Rows before filter: ", format(n_before, big.mark = ","))
  message("Rows with finite coordinates: ", format(sum(valid_xy), big.mark = ","))
  message("Keep NABEL outside buffer: ", keep_nabel_outside)
  message("NABEL exception column: ", nabel_col)
  message("Rows marked as NABEL exception: ", format(sum(nabel_exception), big.mark = ","))
  
  if (!any(valid_xy) && !any(nabel_exception)) {
    stop("SNC buffer row filter: no rows have finite centroid coordinates and no NABEL exception rows.")
  }
  
  v_buf <- read_first_gpkg_layer_if_needed(gpkg_path, layer)
  
  if (is.na(sf::st_crs(v_buf))) {
    warning("SNC buffer GPKG has no CRS. Assuming EPSG:", coord_crs_epsg, ".")
    sf::st_crs(v_buf) <- coord_crs_epsg
  }
  
  v_buf <- sf::st_make_valid(v_buf)
  v_buf <- sf::st_transform(v_buf, coord_crs_epsg)
  v_buf <- sf::st_union(v_buf)
  v_buf <- sf::st_as_sf(sf::st_sfc(v_buf, crs = coord_crs_epsg))
  
  bb <- sf::st_bbox(v_buf)
  bbox_ok <- valid_xy &
    x >= bb[["xmin"]] & x <= bb[["xmax"]] &
    y >= bb[["ymin"]] & y <= bb[["ymax"]]
  
  message("Rows inside buffer bbox: ", format(sum(bbox_ok), big.mark = ","))
  
  inside <- rep(FALSE, n_before)
  candidate_idx <- which(bbox_ok)
  
  if (length(candidate_idx)) {
    chunk_starts <- seq.int(1L, length(candidate_idx), by = point_chunk_n)
    
    for (chunk_i in seq_along(chunk_starts)) {
      a <- chunk_starts[chunk_i]
      b <- min(a + point_chunk_n - 1L, length(candidate_idx))
      idx <- candidate_idx[a:b]
      
      pts <- sf::st_as_sf(
        data.frame(row_id = idx, x = x[idx], y = y[idx]),
        coords = c("x", "y"),
        crs = coord_crs_epsg,
        remove = FALSE
      )
      
      inside[idx] <- lengths(sf::st_intersects(pts, v_buf)) > 0L
      
      rm(pts)
      gc(FALSE)
    }
  }
  
  keep_row <- inside | nabel_exception
  
  n_inside <- sum(inside)
  n_nabel_exception <- sum(nabel_exception)
  n_nabel_rescued <- sum(nabel_exception & !inside)
  n_kept <- sum(keep_row)
  n_removed <- n_before - n_kept
  
  message("Rows inside SNC buffer: ", format(n_inside, big.mark = ","))
  message("NABEL rows kept regardless of buffer: ", format(n_nabel_exception, big.mark = ","))
  message("NABEL rows rescued outside buffer: ", format(n_nabel_rescued, big.mark = ","))
  message("Rows kept after combined filter: ", format(n_kept, big.mark = ","))
  message("Rows removed: ", format(n_removed, big.mark = ","))
  message("Share retained: ", sprintf("%.2f%%", 100 * n_kept / n_before))
  
  if (n_kept == 0L) {
    stop("SNC buffer row filter removed all rows. Check CRS, GPKG, coordinate columns, and NABEL exception settings.")
  }
  
  message("============================================================\n")
  
  df[keep_row, , drop = FALSE]
}

make_split_select_weights <- function(predictor_vec) {
  if (!isTRUE(enable_split_select_weights)) {
    return(NULL)
  }
  
  w <- rep(split_weight_default_value, length(predictor_vec))
  names(w) <- predictor_vec
  
  for (rt in names(road_type_split_weights)) {
    
    rt_weight <- road_type_split_weights[[rt]] * overall_road_family_penalty
    
    is_rt_var <- grepl(paste0("^roads_", rt, "_"), predictor_vec) |
      grepl(paste0("^", rt, "_asp_mean_"), predictor_vec) |
      grepl(paste0("^", rt, "_msp_mean_"), predictor_vec) |
      grepl(paste0("^", rt, "_dtv_mean_"), predictor_vec)
    
    w[is_rt_var] <- rt_weight
  }
  
  is_generic_road_var <- predictor_vec %in% c(
    "distance_to_major_road",
    "car_meters_gauss"
  )
  
  w[is_generic_road_var] <- generic_road_weight * overall_road_family_penalty
  
  if (any(!is.finite(w))) {
    stop("Invalid split.select.weights: some weights are not finite.")
  }
  
  if (any(w < 0)) {
    stop("Invalid split.select.weights: weights must be non-negative.")
  }
  
  if (any(w > 1)) {
    stop("Invalid split.select.weights: weights must be <= 1 for ranger.")
  }
  
  if (sum(w) <= 0) {
    stop("Invalid split.select.weights: at least one predictor must have weight > 0.")
  }
  
  message("Split-select weights enabled.")
  message("Number of predictors: ", length(predictor_vec))
  message("Weight range: ",
          sprintf("%.3f", min(w)), " to ", sprintf("%.3f", max(w)))
  
  road_diag <- data.frame(
    road_type = names(road_type_split_weights),
    split_weight = as.numeric(road_type_split_weights) * overall_road_family_penalty,
    stringsAsFactors = FALSE
  )
  
  message("Road-type split weights:")
  print(road_diag)
  
  message("Default non-road weight: ", sprintf("%.3f", split_weight_default_value))
  message("Generic road weight: ", sprintf("%.3f", generic_road_weight * overall_road_family_penalty))
  
  w
}

# -------------------- GLOBAL TIMER START --------------------
script_t0 <- proc.time()[["elapsed"]]

results_list <- list()
timing_tab <- list()

# -------------------- READ & PREPARE --------------------
message("Reading master parquet…")
df <- arrow::read_parquet(master_parquet)

# ============================================================
# OPTIONAL: HOLD OUT ONE NABEL STATION GRID
#
# Purpose:
#   Remove all training rows from one NABEL station grid before fitting.
#   The held-out grid can later be predicted separately over 2010–2026
#   and compared against the observed NABEL time series.
#
# Important:
#   The strict default removes ALL rows from the selected grid_id,
#   including any mobile measurements in the same grid cell.
#   This avoids leakage at the held-out station location.
# ============================================================

hold_out_nabel_station <- FALSE

# Give the station a readable label for output folders / filenames
held_out_station_name <- "BASEL"

# Replace with the actual grid_id of the station
held_out_grid_id <- 52151072L

if (isTRUE(hold_out_nabel_station)) {
  
  if (!"grid_id" %in% names(df)) {
    stop("Cannot hold out NABEL station: grid_id column not found in df.")
  }
  
  n_before <- nrow(df)
  n_removed <- sum(df$grid_id == held_out_grid_id, na.rm = TRUE)
  
  if (n_removed == 0) {
    warning(
      "No rows found for held-out grid_id = ", held_out_grid_id,
      " (", held_out_station_name, "). Check the grid_id."
    )
  }
  
  df <- df[
    is.na(df$grid_id) | df$grid_id != held_out_grid_id,
    ,
    drop = FALSE
  ]
  
  message("\n============================================================")
  message("NABEL station holdout enabled")
  message("Held-out station: ", held_out_station_name)
  message("Held-out grid_id: ", held_out_grid_id)
  message("Rows before filtering: ", format(n_before, big.mark = ","))
  message("Rows removed: ", format(n_removed, big.mark = ","))
  message("Rows remaining: ", format(nrow(df), big.mark = ","))
  message("============================================================\n")
}

# -------------------- OPTIONAL FILTER: REMOVE SINGLE-OBSERVATION GRIDS --------------------
filter_single_obs_grids <- FALSE
single_obs_col <- "n_obs"

if (isTRUE(filter_single_obs_grids)) {
  if (!single_obs_col %in% names(df)) {
    stop("Column '", single_obs_col, "' not found, but filter_single_obs_grids = TRUE.")
  }
  
  n0 <- nrow(df)
  df <- df %>%
    dplyr::filter(.data[[single_obs_col]] > 1)
  n1 <- nrow(df)
  
  message(sprintf(
    "Single-observation grid filter active: kept %s / %s rows; dropped %s rows with %s <= 1.",
    format(n1, big.mark = ","),
    format(n0, big.mark = ","),
    format(n0 - n1, big.mark = ","),
    single_obs_col
  ))
}

names(df) <- gsub("_r([0-9]+)_r\\1$", "_r\\1", names(df))

dup_names <- names(df)[duplicated(names(df))]
if (length(dup_names)) {
  stop("Duplicate column names after renaming: ", paste(unique(dup_names), collapse = ", "))
}

if (isTRUE(filter_rows_to_snc_buffer)) {
  df <- filter_df_to_snc_buffer(
    df = df,
    gpkg_path = snc_buffer_gpkg,
    layer = snc_buffer_layer,
    x_col = snc_buffer_x_col,
    y_col = snc_buffer_y_col,
    coord_crs_epsg = snc_buffer_coord_crs_epsg,
    point_chunk_n = snc_buffer_point_chunk_n,
    keep_nabel_outside = snc_buffer_keep_nabel_outside,
    nabel_col = snc_buffer_nabel_col,
    nabel_value = snc_buffer_nabel_value
  )
} else {
  message("\nSNC buffer row filter inactive.")
}

if (isTRUE(filter_target_upper_enable)) {
  if (!target %in% names(df)) stop("Target column '", target, "' not found in data.")
  n0 <- nrow(df)
  df <- df %>% dplyr::filter(.data[[target]] < filter_target_upper_value)
  n1 <- nrow(df)
  n_drop <- n0 - n1
  message(sprintf("Target upper filter active: kept %s / %s rows (dropped %s with %s >= %g).",
                  format(n1, big.mark=","), format(n0, big.mark=","),
                  format(n_drop, big.mark=","), target, filter_target_upper_value))
  if (n1 == 0L) stop("All rows were filtered out by 'filter_target_upper_value'.")
}

coord_cols <- c("centroid_x", "centroid_y")
coord_keep <- intersect(coord_cols, names(df))

date_col <- "date_utc"

extra_keep_cols <- character(0)

if (isTRUE(enable_case_weights)) {
  extra_keep_cols <- c(extra_keep_cols, case_weight_source_col)
}

keep_cols <- unique(c(target, features, coord_keep, date_col, extra_keep_cols))
keep_cols <- intersect(keep_cols, names(df))

df_small <- df %>%
  dplyr::select(dplyr::all_of(keep_cols)) %>%
  tidyr::drop_na()

if (!date_col %in% names(df_small)) {
  stop("Required column '", date_col, "' not found in data.")
}

df_small[[date_col]] <- as.Date(df_small[[date_col]])

df_small <- as.data.frame(df_small)

for (nm in intersect(factor_cols, names(df_small))) {
  df_small[[nm]] <- as.factor(df_small[[nm]])
}

for (nm in setdiff(names(df_small), c(target, factor_cols))) {
  if (!is.numeric(df_small[[nm]])) {
    df_small[[nm]] <- suppressWarnings(as.numeric(df_small[[nm]]))
  }
}

if (log_transform_target) {
  df_small[[target]] <- log(df_small[[target]] + log_offset)
}

avail <- intersect(features, names(df_small))
if (!length(avail)) stop("No requested predictors found in data.")

if (enable_redundancy_filter) {
  message(sprintf("Redundancy filter ON (method=%s, threshold=%.2f)…", corr_method, corr_threshold))
  num_names <- setdiff(intersect(avail, names(df_small)), factor_cols)
  sds <- vapply(num_names, function(nm) stats::sd(df_small[[nm]], na.rm = TRUE), numeric(1))
  zero_var <- names(which(!is.finite(sds) | sds == 0))
  if (length(zero_var)) num_names <- setdiff(num_names, zero_var)
  
  if (length(num_names) >= 2) {
    num_df <- as.data.frame(df_small[, num_names, drop = FALSE])
    cm <- suppressWarnings(stats::cor(num_df, use = "pairwise.complete.obs", method = corr_method))
    cm[is.na(cm)] <- 0
    drop <- character(0)
    keep <- colnames(cm)
    
    repeat {
      if (length(keep) < 2) break
      cm_sub <- abs(cm[keep, keep, drop = FALSE])
      cm_sub[lower.tri(cm_sub, diag = TRUE)] <- 0
      max_val <- max(cm_sub, na.rm = TRUE)
      if (!is.finite(max_val) || max_val <= corr_threshold) break
      idx <- which(cm_sub == max_val, arr.ind = TRUE)[1, ]
      v1 <- keep[idx[1]]
      v2 <- keep[idx[2]]
      m1 <- mean(cm_sub[v1, ], na.rm = TRUE)
      m2 <- mean(cm_sub[v2, ], na.rm = TRUE)
      drop_one <- if (m1 >= m2) v1 else v2
      drop <- c(drop, drop_one)
      keep <- setdiff(keep, drop_one)
    }
    
    avail_num_kept <- setdiff(num_names, drop)
    avail_fcts     <- intersect(factor_cols, names(df_small))
    avail          <- intersect(avail, union(avail_num_kept, avail_fcts))
  }
}

vi_filter_df <- NULL
avail_before_vi <- avail

if (isTRUE(enable_vi_filter)) {
  message("Variable importance filter ON...")
  
  vi_sel_out <- time_block("Variable importance filtering", {
    select_features_by_vi(
      df = df_small,
      target = target,
      predictor_vec = avail,
      importance_mode = vi_mode_for_filter,
      filter_type = vi_filter_type,
      cutoff_absolute = vi_cutoff_absolute,
      cutoff_relative = vi_cutoff_relative,
      top_n = vi_top_n,
      min_keep = vi_min_keep
    )
  })
  
  keep_vars <- vi_sel_out$value$keep_vars
  vi_filter_df <- vi_sel_out$value$vi_df
  timing_tab[["vi_filter"]] <- data.frame(
    block = "Variable_Importance_Filter",
    elapsed_sec = vi_sel_out$elapsed_sec
  )
  
  dropped_vars <- setdiff(avail, keep_vars)
  avail <- intersect(avail, keep_vars)
  
  message("VI filter kept ", length(avail), " / ", length(avail_before_vi), " predictors.")
  
  if (length(dropped_vars)) {
    message("Dropped by VI filter: ", paste(dropped_vars, collapse = ", "))
  }
  
  if (isTRUE(write_vi_filter_csv) && !is.null(vi_filter_df)) {
    readr::write_csv(vi_filter_df, out_vi_filter_csv)
    message("✓ VI filter CSV written to: ", out_vi_filter_csv)
  }
}

temporal_features <- intersect(c(weather_vars, time_vars, fuel_vars), avail)
spatial_features  <- setdiff(avail, temporal_features)

if (!length(spatial_features)) stop("No spatial predictors left after filtering.")
if (!length(temporal_features)) warning("No temporal predictors found in the data.")

form_all      <- make_form(target, avail)
form_spatial  <- make_form(target, spatial_features)
form_temporal <- if (length(temporal_features)) make_form(target, temporal_features) else NULL

cat("\n============================================================\n")
cat("UNIFIED RF VALIDATION SUITE (DAILY)\n")
cat("Log-transform target:", log_transform_target, "\n")
cat("Log offset:", log_offset, "\n")
cat("Evaluate log-scale metrics:", eval_log_scale_metrics, "\n")
cat("Evaluate naive backtransform:", eval_naive_backtransform, "\n")
cat("Evaluate Duan smearing:", eval_duan_smearing, "\n")
cat("Smearing methods:", paste(smearing_methods, collapse = ", "), "\n")
cat("Smearing bins:", smearing_n_bins, "\n")
cat("Predictors used:", length(avail), "\n")
cat("Rows used:", nrow(df_small), "\n")
cat("Target column:", target, "\n")
cat("Save final full-data model:", save_final_model, "\n")
cat("Write variable importance CSV:", write_vi_csv, "\n")
cat("============================================================\n\n")

if (!isTRUE(run_validation_suite) && !isTRUE(save_final_model)) {
  stop("Both run_validation_suite and save_final_model are FALSE. Nothing to do.")
}



# -------------------- 1) OOB VALIDATION (ALL) --------------------
if (isTRUE(run_validation_suite)) {
  out <- time_block("OOB validation (all predictors)", {
    rf_oob <- fit_ranger(form_all, df_small, importance_mode = "none")
    
    oob_pred_raw <- as.numeric(rf_oob$predictions)
    
    smearing_infos <- NULL
    
    if (isTRUE(log_transform_target) && isTRUE(eval_duan_smearing)) {
      smearing_infos <- list()
      
      if ("global" %in% smearing_methods) {
        smearing_infos[["global"]] <- compute_smearing_info(
          y_log = df_small[[target]],
          pred_log_train = oob_pred_raw,
          method = "global",
          n_bins = smearing_n_bins
        )
      }
      
      if ("pred_bin" %in% smearing_methods) {
        smearing_infos[["pred_bin"]] <- compute_smearing_info(
          y_log = df_small[[target]],
          pred_log_train = oob_pred_raw,
          method = "pred_bin",
          n_bins = smearing_n_bins
        )
      }
    }
    
    score_predictions_multi(
      y_obs_transformed = df_small[[target]],
      pred_raw = oob_pred_raw,
      is_log = log_transform_target,
      offset = log_offset,
      smearing_infos = smearing_infos,
      scheme_name = "All_OOB",
      n_train = nrow(df_small),
      n_test = nrow(df_small)
    )
  })
  
  results_list[["oob"]] <- out$value
  timing_tab[["oob"]] <- data.frame(block = "All_OOB", elapsed_sec = out$elapsed_sec)
}

# -------------------- 2) RANDOM 80/20 (ALL) --------------------
if (isTRUE(run_validation_suite)) {
  out <- time_block("Random 80/20 (all predictors)", {
    set.seed(40)
    idx_all <- sample.int(nrow(df_small), size = floor(random_split_p * nrow(df_small)))
    train_all <- df_small[idx_all, , drop = FALSE]
    test_all  <- df_small[-idx_all, , drop = FALSE]
    
    res <- score_split(
      train_df = train_all,
      test_df = test_all,
      form = form_all,
      target = target,
      is_log = log_transform_target,
      offset = log_offset,
      scheme_name = "All_Random80_20"
    )
    
    res
  })
  results_list[["random"]] <- out$value
  timing_tab[["random"]] <- data.frame(block = "All_Random80_20", elapsed_sec = out$elapsed_sec)
}

# -------------------- 3) 5-FOLD K-MEANS CV (UNIQUE OBS + UNIQUE XY) --------------------
# REQUIREMENTS / ASSUMPTIONS:
# - coord_cols contains exactly two coordinate columns, e.g. c("centroid_x", "centroid_y")
# - df_small contains those coordinate columns
# - score_split(), time_block(), results_list, timing_tab already exist in the script
# - form_all, target, log_transform_target, log_offset, kmeans_k already exist
# - The raster template path below should point to your Swiss 25m reference grid raster

kmeans_time_col            <- "date_utc"   # set this to your temporal column
cluster_template_raster    <- file.path(input_root, "grid_reference_25m.tif")
output_dir <- file.path(output_root, "spatial_cv_fold_maps")
cluster_output_dir         <- file.path(output_dir, "kmeans_cluster_rasters")

# Optional: if TRUE, also write one observation-cluster raster per date/time value.
# Warning: this can create many files if you have many unique dates/times.
write_obs_date_rasters     <- FALSE

# Optional: cap how many per-date rasters are written when write_obs_date_rasters = TRUE
max_obs_date_rasters       <- 50

dir.create(cluster_output_dir, recursive = TRUE, showWarnings = FALSE)

# -------------------- helpers --------------------
mode_first <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_integer_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

as_time_numeric <- function(x) {
  if (inherits(x, "POSIXt")) return(as.numeric(x))
  if (inherits(x, "Date"))   return(as.numeric(x))
  if (is.numeric(x))         return(x)
  
  x_posix <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
  if (sum(!is.na(x_posix)) > 0) return(as.numeric(x_posix))
  
  x_date <- suppressWarnings(as.Date(x))
  if (sum(!is.na(x_date)) > 0) return(as.numeric(x_date))
  
  stop("Could not convert kmeans_time_col to numeric. Please check the time column.")
}

summarise_kmeans_cv <- function(fold_tab, scheme_name) {
  if (is.null(fold_tab) || nrow(fold_tab) == 0) return(NULL)
  
  out <- fold_tab |>
    dplyr::group_by(pred_type, target_scale) |>
    dplyr::summarise(
      scheme  = scheme_name,
      n_train = sum(n_train, na.rm = TRUE),
      n_test  = sum(n_test,  na.rm = TRUE),
      rmse    = mean(rmse, na.rm = TRUE),
      mae     = mean(mae,  na.rm = TRUE),
      bias    = mean(bias, na.rm = TRUE),
      r2      = mean(r2,   na.rm = TRUE),
      .groups = "drop"
    )
  
  out <- as.data.frame(out)
  
  out[, c(
    "scheme", "pred_type", "target_scale",
    "n_train", "n_test",
    "rmse", "mae", "bias", "r2"
  )]
}

run_kmeans_cv_from_foldcol <- function(df_in, fold_col, scheme_name) {
  fold_ids <- sort(unique(df_in[[fold_col]]))
  fold_ids <- fold_ids[!is.na(fold_ids)]
  
  fold_scores <- lapply(fold_ids, function(fold_id) {
    test_fold  <- df_in[df_in[[fold_col]] == fold_id, , drop = FALSE]
    train_fold <- df_in[df_in[[fold_col]] != fold_id, , drop = FALSE]
    
    if (nrow(test_fold) == 0 || nrow(train_fold) == 0) return(NULL)
    
    score_split(
      train_df = train_fold,
      test_df = test_fold,
      form = form_all,
      target = target,
      is_log = log_transform_target,
      offset = log_offset,
      scheme_name = paste0(scheme_name, "_fold", fold_id)
    )
  })
  
  fold_scores <- Filter(Negate(is.null), fold_scores)
  if (!length(fold_scores)) return(NULL)
  
  fold_tab <- do.call(rbind, fold_scores)
  summarise_kmeans_cv(fold_tab, scheme_name)
}

write_cluster_raster <- function(df_xy, value_col, template_raster_path, out_path, coord_cols) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required to write cluster rasters.")
  }
  
  stopifnot(length(coord_cols) == 2)
  
  # template
  r_template <- terra::rast(template_raster_path)
  
  # keep only needed columns
  df_xy <- df_xy[, c(coord_cols, value_col), drop = FALSE]
  df_xy <- df_xy[stats::complete.cases(df_xy), , drop = FALSE]
  
  if (nrow(df_xy) == 0) {
    warning("No valid rows available for raster writing: ", out_path)
    return(invisible(NULL))
  }
  
  # points at cell centers
  v <- terra::vect(df_xy, geom = coord_cols, crs = terra::crs(r_template))
  
  # empty output raster
  r_out <- r_template
  r_out[] <- NA
  
  # rasterize
  r_out <- terra::rasterize(v, r_out, field = value_col, fun = "first", background = NA)
  
  terra::writeRaster(
    r_out, out_path,
    overwrite = TRUE,
    wopt = list(gdal = c("COMPRESS=LZW"))
  )
  
  invisible(out_path)
}

# -------------------- 3A) 5-FOLD K-MEANS CV (UNIQUE OBSERVATIONS: x, y, t) --------------------
if (isTRUE(run_validation_suite) &&
    all(coord_cols %in% names(df_small)) &&
    kmeans_time_col %in% names(df_small)) {
  
  out <- time_block("5-fold k-means CV (unique observations: x,y,t)", {
    
    # keep only rows with complete x, y, t
    valid_obs <- stats::complete.cases(df_small[, c(coord_cols, kmeans_time_col), drop = FALSE])
    df_obs0   <- df_small[valid_obs, , drop = FALSE]
    
    # unique observation keys = unique (x, y, t)
    obs_key <- unique(df_obs0[, c(coord_cols, kmeans_time_col), drop = FALSE])
    
    if (nrow(obs_key) < kmeans_k) return(NULL)
    
    # numeric time for clustering
    obs_key$..time_num <- as_time_numeric(obs_key[[kmeans_time_col]])
    
    # cluster on x, y, t
    clust_mat_obs <- as.matrix(obs_key[, c(coord_cols, "..time_num"), drop = FALSE])
    
    set.seed(40)
    km_obs <- stats::kmeans(scale(clust_mat_obs), centers = kmeans_k, nstart = 25)
    obs_key$km5_cluster_unique_obs <- km_obs$cluster
    
    # join fold ID back to all rows sharing the same (x, y, t)
    df_km_obs <- merge(
      df_obs0,
      obs_key[, c(coord_cols, kmeans_time_col, "km5_cluster_unique_obs"), drop = FALSE],
      by = c(coord_cols, kmeans_time_col),
      all.x = TRUE,
      sort = FALSE
    )
    
    # CV scoring
    cv_out <- run_kmeans_cv_from_foldcol(
      df_in       = df_km_obs,
      fold_col    = "km5_cluster_unique_obs",
      scheme_name = "All_KMeans5_UniqueObs"
    )
    
    # ---- Raster outputs for QGIS ----
    # A single raster cannot store a changing cluster through time at the same pixel.
    # Therefore we create:
    #   1) a summary raster = modal cluster per (x, y)
    #   2) optional per-date rasters if desired
    
    # summary raster: modal observation-cluster by location
    obs_mode_xy <- stats::aggregate(
      obs_key$km5_cluster_unique_obs,
      by = obs_key[, coord_cols, drop = FALSE],
      FUN = mode_first
    )
    names(obs_mode_xy)[ncol(obs_mode_xy)] <- "km5_cluster_unique_obs_mode"
    
    write_cluster_raster(
      df_xy                = obs_mode_xy,
      value_col            = "km5_cluster_unique_obs_mode",
      template_raster_path = cluster_template_raster,
      out_path             = file.path(cluster_output_dir, "All_KMeans5_UniqueObs_MODE_byXY_clusters.tif"),
      coord_cols           = coord_cols
    )
    
    # optional: write one raster per date/time value
    if (isTRUE(write_obs_date_rasters)) {
      time_vals <- unique(obs_key[[kmeans_time_col]])
      if (length(time_vals) > max_obs_date_rasters) {
        warning("Skipping per-date observation rasters because number of unique time values (",
                length(time_vals), ") exceeds max_obs_date_rasters = ", max_obs_date_rasters)
      } else {
        for (tt in time_vals) {
          sub_tt <- obs_key[obs_key[[kmeans_time_col]] == tt, c(coord_cols, "km5_cluster_unique_obs"), drop = FALSE]
          if (nrow(sub_tt) == 0) next
          
          tt_tag <- gsub("[^0-9A-Za-z_-]", "_", as.character(tt))
          
          write_cluster_raster(
            df_xy                = sub_tt,
            value_col            = "km5_cluster_unique_obs",
            template_raster_path = cluster_template_raster,
            out_path             = file.path(cluster_output_dir, paste0("All_KMeans5_UniqueObs_", tt_tag, ".tif")),
            coord_cols           = coord_cols
          )
        }
      }
    }
    
    cv_out
  })
  
  if (!is.null(out$value)) {
    results_list[["kmeans_unique_obs"]] <- out$value
    timing_tab[["kmeans_unique_obs"]] <- data.frame(
      block = "All_KMeans5_UniqueObs",
      elapsed_sec = out$elapsed_sec
    )
  }
}

# -------------------- 3B) 5-FOLD K-MEANS CV (UNIQUE GEO-LOCATIONS: x, y) --------------------
if (isTRUE(run_validation_suite) && all(coord_cols %in% names(df_small))) {
  
  out <- time_block("5-fold k-means CV (unique geo-locations: x,y)", {
    
    valid_xy <- stats::complete.cases(df_small[, coord_cols, drop = FALSE])
    df_xy0   <- df_small[valid_xy, , drop = FALSE]
    
    # unique geo-locations = unique (x, y)
    xy_key <- unique(df_xy0[, coord_cols, drop = FALSE])
    
    if (nrow(xy_key) < kmeans_k) return(NULL)
    
    # cluster on x, y only
    clust_mat_xy <- as.matrix(xy_key[, coord_cols, drop = FALSE])
    
    set.seed(40)
    km_xy <- stats::kmeans(scale(clust_mat_xy), centers = kmeans_k, nstart = 25)
    xy_key$km5_cluster_unique_xy <- km_xy$cluster
    
    # join fold ID back to all rows sharing the same (x, y)
    df_km_xy <- merge(
      df_xy0,
      xy_key[, c(coord_cols, "km5_cluster_unique_xy"), drop = FALSE],
      by = coord_cols,
      all.x = TRUE,
      sort = FALSE
    )
    
    # CV scoring
    cv_out <- run_kmeans_cv_from_foldcol(
      df_in       = df_km_xy,
      fold_col    = "km5_cluster_unique_xy",
      scheme_name = "All_KMeans5_UniqueXY"
    )
    
    # raster output for QGIS
    write_cluster_raster(
      df_xy                = xy_key,
      value_col            = "km5_cluster_unique_xy",
      template_raster_path = cluster_template_raster,
      out_path             = file.path(cluster_output_dir, "All_KMeans5_UniqueXY_clusters.tif"),
      coord_cols           = coord_cols
    )
    
    cv_out
  })
  
  if (!is.null(out$value)) {
    results_list[["kmeans_unique_xy"]] <- out$value
    timing_tab[["kmeans_unique_xy"]] <- data.frame(
      block = "All_KMeans5_UniqueXY",
      elapsed_sec = out$elapsed_sec
    )
  }
}

# # -------------------- 4) SPATIAL-ONLY MODEL --------------------
# if (isTRUE(run_validation_suite)) {
#   out <- time_block("Spatial-only model (random 80/20)", {
#     set.seed(40)
#     idx_sp <- sample.int(nrow(df_small), size = floor(random_split_p * nrow(df_small)))
#     train_sp <- df_small[idx_sp, , drop = FALSE]
#     test_sp  <- df_small[-idx_sp, , drop = FALSE]
#     
#     res <- score_split(
#       train_df = train_sp,
#       test_df = test_sp,
#       form = form_spatial,
#       target = target,
#       is_log = log_transform_target,
#       offset = log_offset,
#       scheme_name = "SpatialOnly_Random80_20"
#     )
#     
#     res
#   })
#   results_list[["spatial_only"]] <- out$value
#   timing_tab[["spatial_only"]] <- data.frame(block = "SpatialOnly_Random80_20", elapsed_sec = out$elapsed_sec)
# }

# ============================================================
# 5) ROW-BALANCED BLOCKED TEMPORAL CV BY DATE (ALL PREDICTORS)
# ============================================================
#
# PURPOSE:
#   Create k temporally contiguous CV folds while keeping complete dates
#   together and making the number of modelling rows in each fold as similar
#   as possible.
#
# IMPORTANT:
#   - All observations from the same date always remain in the same fold.
#   - Folds remain chronologically contiguous.
#   - Fold boundaries are chosen according to cumulative ROW counts,
#     not equal numbers of dates or equal calendar duration.
#   - Therefore long periods containing sparse NABEL measurements do not
#     automatically receive the same fold weight as dense campaign periods.
#   - Smearing is still estimated from the training rows only inside
#     score_split().
#
# Example interpretation:
#   Fold 1 may cover 2010-2023,
#   whereas Fold 5 may cover only part of 2025-2026,
#   if those periods contain similar numbers of modelling rows.
# ============================================================

if (isTRUE(run_validation_suite)) {
  
  out <- time_block(
    "Row-balanced blocked temporal CV by date (all predictors)",
    {
      
      # --------------------------------------------------------
      # CHECK DATE COLUMN
      # --------------------------------------------------------
      
      if (!"date_utc" %in% names(df_small)) {
        stop(
          "date_utc is required for row-balanced temporal CV."
        )
      }
      
      df_small$date_utc <- as.Date(
        df_small$date_utc
      )
      
      if (any(is.na(df_small$date_utc))) {
        stop(
          "date_utc contains NA values in df_small."
        )
      }
      
      
      # --------------------------------------------------------
      # BUILD DATE-LEVEL TABLE WITH NUMBER OF MODELLING ROWS
      # --------------------------------------------------------
      
      day_key <- df_small |>
        dplyr::count(
          date_utc,
          name = "n_rows"
        ) |>
        dplyr::arrange(
          date_utc
        )
      
      if (nrow(day_key) < kmeans_k) {
        stop(
          "Need at least kmeans_k unique dates for temporal CV. Found ",
          nrow(day_key),
          " unique dates, but kmeans_k = ",
          kmeans_k,
          "."
        )
      }
      
      total_rows <- sum(
        day_key$n_rows
      )
      
      target_rows_per_fold <-
        total_rows / kmeans_k
      
      
      # --------------------------------------------------------
      # FIND CONTIGUOUS FOLD BOUNDARIES
      # --------------------------------------------------------
      #
      # We want boundaries closest to:
      #
      #   20%, 40%, 60%, 80%
      #
      # of cumulative modelling rows for k = 5.
      #
      # Dates themselves are NEVER split.
      # --------------------------------------------------------
      
      day_key <- day_key |>
        dplyr::mutate(
          cumulative_rows = cumsum(n_rows)
        )
      
      # Desired cumulative row totals at the end of folds
      target_boundaries <-
        target_rows_per_fold *
        seq_len(kmeans_k - 1L)
      
      boundary_idx <- integer(
        length(target_boundaries)
      )
      
      previous_idx <- 0L
      
      for (b in seq_along(target_boundaries)) {
        
        target_b <- target_boundaries[b]
        
        # Leave enough dates for all remaining folds.
        min_idx <- previous_idx + 1L
        
        max_idx <-
          nrow(day_key) -
          (kmeans_k - b)
        
        candidate_idx <- seq.int(
          min_idx,
          max_idx
        )
        
        # Select date whose cumulative row count is closest
        # to the desired cumulative target.
        best_local <- which.min(
          abs(
            day_key$cumulative_rows[
              candidate_idx
            ] -
              target_b
          )
        )
        
        boundary_idx[b] <-
          candidate_idx[
            best_local
          ]
        
        previous_idx <- boundary_idx[b]
      }
      
      
      # --------------------------------------------------------
      # ASSIGN TEMPORAL FOLD IDs
      # --------------------------------------------------------
      
      fold_end_idx <- c(
        boundary_idx,
        nrow(day_key)
      )
      
      fold_start_idx <- c(
        1L,
        boundary_idx + 1L
      )
      
      day_key$temporal_fold <- NA_integer_
      
      for (fold_id in seq_len(kmeans_k)) {
        
        idx <- seq.int(
          fold_start_idx[fold_id],
          fold_end_idx[fold_id]
        )
        
        day_key$temporal_fold[idx] <-
          fold_id
      }
      
      if (any(is.na(day_key$temporal_fold))) {
        stop(
          "Some dates were not assigned to a temporal fold."
        )
      }
      
      
      # --------------------------------------------------------
      # FOLD-DESIGN DIAGNOSTICS
      # --------------------------------------------------------
      
      fold_design <- day_key |>
        dplyr::group_by(
          temporal_fold
        ) |>
        dplyr::summarise(
          test_start = min(date_utc),
          test_end = max(date_utc),
          
          n_test_days = dplyr::n(),
          
          n_test_rows = sum(
            n_rows
          ),
          
          pct_total_rows =
            100 *
            n_test_rows /
            total_rows,
          
          .groups = "drop"
        ) |>
        dplyr::mutate(
          target_pct =
            100 / kmeans_k,
          
          row_difference_from_target =
            n_test_rows -
            target_rows_per_fold,
          
          pct_point_difference_from_target =
            pct_total_rows -
            target_pct
        )
      
      cat(
        "\n============================================================\n"
      )
      cat(
        "ROW-BALANCED BLOCKED TEMPORAL CV FOLD DESIGN\n"
      )
      cat(
        "Target rows per fold: ",
        format(
          round(target_rows_per_fold),
          big.mark = ","
        ),
        "\n",
        sep = ""
      )
      cat(
        "Target percentage per fold: ",
        sprintf(
          "%.2f%%",
          100 / kmeans_k
        ),
        "\n\n",
        sep = ""
      )
      
      print(fold_design)
      
      cat(
        "============================================================\n\n"
      )
      
      
      # --------------------------------------------------------
      # JOIN DATE-LEVEL FOLDS BACK TO ALL MODELLING ROWS
      # --------------------------------------------------------
      
      df_tm <- df_small |>
        dplyr::left_join(
          day_key[
            ,
            c(
              "date_utc",
              "temporal_fold"
            )
          ],
          by = "date_utc"
        )
      
      if (any(is.na(df_tm$temporal_fold))) {
        stop(
          "Some modelling rows could not be assigned to ",
          "a temporal fold."
        )
      }
      
      
      # --------------------------------------------------------
      # RUN TEMPORAL CROSS-VALIDATION
      # --------------------------------------------------------
      
      fold_ids <- sort(
        unique(
          df_tm$temporal_fold
        )
      )
      
      fold_scores <- lapply(
        fold_ids,
        function(fold_id) {
          
          test_tm <- df_tm[
            df_tm$temporal_fold == fold_id,
            ,
            drop = FALSE
          ]
          
          train_tm <- df_tm[
            df_tm$temporal_fold != fold_id,
            ,
            drop = FALSE
          ]
          
          if (
            nrow(train_tm) == 0L ||
            nrow(test_tm) == 0L
          ) {
            
            warning(
              "Temporal fold ",
              fold_id,
              " produced an empty train or test set. Skipping."
            )
            
            return(NULL)
          }
          
          train_days <- sort(
            unique(
              train_tm$date_utc
            )
          )
          
          test_days <- sort(
            unique(
              test_tm$date_utc
            )
          )
          
          res <- score_split(
            train_df = train_tm,
            test_df = test_tm,
            form = form_all,
            target = target,
            is_log = log_transform_target,
            offset = log_offset,
            
            scheme_name = paste0(
              "All_TemporalBalanced",
              kmeans_k,
              "_fold",
              fold_id
            )
          )
          
          res$temporal_fold <-
            fold_id
          
          res$n_train_days <-
            length(train_days)
          
          res$n_test_days <-
            length(test_days)
          
          res$test_start <-
            min(test_days)
          
          res$test_end <-
            max(test_days)
          
          # Useful diagnostics
          res$test_pct_rows <-
            100 *
            nrow(test_tm) /
            nrow(df_tm)
          
          res$target_pct_rows <-
            100 / kmeans_k
          
          res
        }
      )
      
      
      # --------------------------------------------------------
      # COMBINE FOLD RESULTS
      # --------------------------------------------------------
      
      fold_scores <- Filter(
        Negate(is.null),
        fold_scores
      )
      
      if (!length(fold_scores)) {
        stop(
          "Row-balanced temporal CV produced no valid fold scores."
        )
      }
      
      fold_tab <- dplyr::bind_rows(
        fold_scores
      )
      
      
      # --------------------------------------------------------
      # PRINT ACTUAL TEST FOLD SIZES
      # --------------------------------------------------------
      
      fold_size_check <- fold_tab |>
        dplyr::filter(
          pred_type ==
            dplyr::first(pred_type)
        ) |>
        dplyr::select(
          temporal_fold,
          test_start,
          test_end,
          n_test_days,
          n_test,
          test_pct_rows
        ) |>
        dplyr::distinct() |>
        dplyr::arrange(
          temporal_fold
        )
      
      cat(
        "\nActual temporal validation folds:\n"
      )
      
      print(fold_size_check)
      
      
      # --------------------------------------------------------
      # SUMMARY ACROSS THE k TEMPORAL FOLDS
      # --------------------------------------------------------
      #
      # Metrics are averaged across folds exactly as in the previous
      # temporal-CV implementation.
      # --------------------------------------------------------
      
      summary_tab <- fold_tab |>
        dplyr::group_by(
          pred_type,
          target_scale
        ) |>
        dplyr::summarise(
          
          scheme = paste0(
            "All_TemporalBalanced",
            kmeans_k
          ),
          
          n_train =
            sum(
              n_train,
              na.rm = TRUE
            ),
          
          n_test =
            sum(
              n_test,
              na.rm = TRUE
            ),
          
          mean_n_train_per_fold =
            mean(
              n_train,
              na.rm = TRUE
            ),
          
          mean_n_test_per_fold =
            mean(
              n_test,
              na.rm = TRUE
            ),
          
          n_train_days =
            sum(
              n_train_days,
              na.rm = TRUE
            ),
          
          n_test_days =
            sum(
              n_test_days,
              na.rm = TRUE
            ),
          
          mean_n_train_days_per_fold =
            mean(
              n_train_days,
              na.rm = TRUE
            ),
          
          mean_n_test_days_per_fold =
            mean(
              n_test_days,
              na.rm = TRUE
            ),
          
          mean_test_pct_rows =
            mean(
              test_pct_rows,
              na.rm = TRUE
            ),
          
          min_test_pct_rows =
            min(
              test_pct_rows,
              na.rm = TRUE
            ),
          
          max_test_pct_rows =
            max(
              test_pct_rows,
              na.rm = TRUE
            ),
          
          rmse =
            mean(
              rmse,
              na.rm = TRUE
            ),
          
          mae =
            mean(
              mae,
              na.rm = TRUE
            ),
          
          bias =
            mean(
              bias,
              na.rm = TRUE
            ),
          
          r2 =
            mean(
              r2,
              na.rm = TRUE
            ),
          
          .groups = "drop"
        )
      
      summary_tab <- as.data.frame(
        summary_tab
      )
      
      summary_tab[
        ,
        c(
          "scheme",
          "pred_type",
          "target_scale",
          
          "n_train",
          "n_test",
          
          "mean_n_train_per_fold",
          "mean_n_test_per_fold",
          
          "n_train_days",
          "n_test_days",
          
          "mean_n_train_days_per_fold",
          "mean_n_test_days_per_fold",
          
          "mean_test_pct_rows",
          "min_test_pct_rows",
          "max_test_pct_rows",
          
          "rmse",
          "mae",
          "bias",
          "r2"
        )
      ]
    }
  )
  
  
  # ----------------------------------------------------------
  # SAVE INTO EXISTING RESULT OBJECTS
  # ----------------------------------------------------------
  
  results_list[["temporal_balanced"]] <- out$value
  
  timing_tab[["temporal_balanced"]] <- data.frame(
    block = paste0(
      "All_TemporalBalanced",
      kmeans_k
    ),
    elapsed_sec = out$elapsed_sec
  )
  
}

# -------------------- OPTIONAL FINAL FULL-DATA MODEL SAVE --------------------
final_model_info <- NULL
vi_df <- NULL

if (isTRUE(save_final_model)) {
  out <- time_block("Final full-data model fit + save + VI", {
    rf_final <- fit_ranger(form_all, df_small, importance_mode = if (write_vi_csv) "permutation" else "none")
    
    final_smearing_infos <- NULL
    
    if (isTRUE(save_smearing_info) &&
        isTRUE(log_transform_target) &&
        isTRUE(eval_duan_smearing)) {
      
      pred_train_final_for_smear <- get_training_predictions_for_smearing(
        rf = rf_final,
        train_df = df_small,
        use_oob = smearing_use_oob_residuals
      )
      
      final_smearing_infos <- list()
      
      if ("global" %in% smearing_methods) {
        final_smearing_infos[["global"]] <- compute_smearing_info(
          y_log = df_small[[target]],
          pred_log_train = pred_train_final_for_smear,
          method = "global",
          n_bins = smearing_n_bins
        )
      }
      
      if ("pred_bin" %in% smearing_methods) {
        final_smearing_infos[["pred_bin"]] <- compute_smearing_info(
          y_log = df_small[[target]],
          pred_log_train = pred_train_final_for_smear,
          method = "pred_bin",
          n_bins = smearing_n_bins
        )
      }
    }
    
    saveRDS(list(
      model = rf_final,
      log_transform = log_transform_target,
      offset = log_offset,
      target = target,
      features_used = avail,
      n_predictors = length(avail),
      n_rows = nrow(df_small),
      snc_buffer_filter = list(
        enabled = filter_rows_to_snc_buffer,
        gpkg = if (isTRUE(filter_rows_to_snc_buffer)) snc_buffer_gpkg else NA_character_,
        layer = if (isTRUE(filter_rows_to_snc_buffer)) snc_buffer_layer else NA_character_,
        coord_crs_epsg = snc_buffer_coord_crs_epsg,
        x_col = snc_buffer_x_col,
        y_col = snc_buffer_y_col,
        keep_nabel_outside = snc_buffer_keep_nabel_outside,
        nabel_col = snc_buffer_nabel_col,
        nabel_value = snc_buffer_nabel_value
      ),
      smearing_info = final_smearing_infos,
      smearing_settings = list(
        eval_naive_backtransform = eval_naive_backtransform,
        eval_duan_smearing = eval_duan_smearing,
        smearing_methods = smearing_methods,
        smearing_n_bins = smearing_n_bins,
        smearing_use_oob_residuals = smearing_use_oob_residuals
      ),
      model_params = list(
        num.trees = num.trees,
        mtry = min(mtry, length(all.vars(form_all)) - 1L),
        min.node.size = min.node.size,
        max.depth = max.depth,
        replace = replace,
        sample.fraction = sample.fraction,
        splitrule = splitrule,
        num.random.splits = num.random.splits,
        respect.unordered = respect_unordered,
        na_mode = na_mode,
        num.threads = num.threads
      )
    ), out_model_rds)
    
    vi_obj <- get_vi(rf_final)
    vi_out <- NULL
    if (!is.null(vi_obj) && isTRUE(write_vi_csv)) {
      vi_out <- data.frame(
        variable   = names(vi_obj),
        importance = as.numeric(vi_obj),
        stringsAsFactors = FALSE
      )
      vi_out <- vi_out[order(vi_out$importance, decreasing = TRUE), , drop = FALSE]
      vi_out$rank <- seq_len(nrow(vi_out))
      
      rng <- range(vi_out$importance, na.rm = TRUE)
      if (is.finite(rng[1]) && is.finite(rng[2]) && diff(rng) > 0) {
        vi_out$importance_scaled_0_1 <- (vi_out$importance - rng[1]) / (rng[2] - rng[1])
      } else {
        vi_out$importance_scaled_0_1 <- NA_real_
      }
      
      readr::write_csv(vi_out, out_vi_csv)
    }
    
    list(
      model_info = data.frame(
        block = "Final_Model_Saved",
        out_model_rds = out_model_rds,
        stringsAsFactors = FALSE
      ),
      vi_df = vi_out
    )
  })
  
  final_model_info <- out$value$model_info
  vi_df <- out$value$vi_df
  timing_tab[["final_model"]] <- data.frame(block = "Final_Model_Saved", elapsed_sec = out$elapsed_sec)
  
  message("✓ Saved final full-data model to: ", out_model_rds)
  if (!is.null(vi_df) && isTRUE(write_vi_csv)) {
    message("✓ Variable importance CSV written to: ", out_vi_csv)
  }
}

# -------------------- PRINT RESULTS --------------------
if (length(results_list)) {
  results_tab <- dplyr::bind_rows(results_list)
  rownames(results_tab) <- NULL
  
  # -------------------- CONSISTENT RESULT ORDER --------------------
  scheme_order <- c(
    "All_OOB",
    "All_Random80_20",
    "All_KMeans5_UniqueObs",
    "All_KMeans5_UniqueXY",
    "SpatialOnly_Random80_20",
    paste0("All_TemporalKMeans", kmeans_k)
  )
  
  pred_type_order <- c(
    "log_scale",
    "naive_backtransform",
    "duan_global",
    "duan_pred_bin"
  )
  
  target_scale_order <- c(
    "log",
    "original"
  )
  
  results_tab <- results_tab |>
    dplyr::mutate(
      scheme = as.character(scheme),
      pred_type = as.character(pred_type),
      target_scale = as.character(target_scale),
      
      scheme_order_id = match(scheme, scheme_order),
      pred_type_order_id = match(pred_type, pred_type_order),
      target_scale_order_id = match(target_scale, target_scale_order),
      
      # Unknown/new schemes are placed after known schemes
      scheme_order_id = dplyr::if_else(
        is.na(scheme_order_id),
        length(scheme_order) + 1L,
        scheme_order_id
      ),
      pred_type_order_id = dplyr::if_else(
        is.na(pred_type_order_id),
        length(pred_type_order) + 1L,
        pred_type_order_id
      ),
      target_scale_order_id = dplyr::if_else(
        is.na(target_scale_order_id),
        length(target_scale_order) + 1L,
        target_scale_order_id
      )
    ) |>
    dplyr::arrange(
      scheme_order_id,
      scheme,
      pred_type_order_id,
      target_scale_order_id
    ) |>
    dplyr::select(
      -scheme_order_id,
      -pred_type_order_id,
      -target_scale_order_id
    )
  
  cat("\n================ RF VALIDATION SUMMARY ================\n")
  print(results_tab, row.names = FALSE)
  cat("-------------------------------------------------------\n")
  cat("Log-transform target:", log_transform_target, "\n")
  cat("Predictors used:", length(avail), "\n")
  cat("Rows used:", nrow(df_small), "\n")
  cat("Target:", target, "\n")
  cat("=======================================================\n")
}

if (length(timing_tab)) {
  timing_df <- dplyr::bind_rows(timing_tab)
  timing_df$elapsed_min <- timing_df$elapsed_sec / 60
  timing_df$elapsed_h   <- timing_df$elapsed_sec / 3600
  
  cat("\n================ TIMING SUMMARY ================\n")
  print(timing_df, row.names = FALSE)
  cat("================================================\n")
}

if (isTRUE(write_summary_csv) && length(results_list)) {
  readr::write_csv(results_tab, out_summary_csv)
  message("✓ Validation summary written to: ", out_summary_csv)
}

script_elapsed <- proc.time()[["elapsed"]] - script_t0
message(sprintf("TOTAL SCRIPT TIME: %.2f min (%.2f h)", script_elapsed / 60, script_elapsed / 3600))