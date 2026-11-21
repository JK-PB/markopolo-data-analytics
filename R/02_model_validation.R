# MARKOPOLO portfolio demonstration
# Trained only on synthetic data. Not the research production model.
# The key question: how does evaluation design affect generalisation claims?

model_features <- c(
  "road_density", "ndvi", "population_density", "temperature_c",
  "wind_speed", "day_sin", "day_cos"
)

predictive_metrics <- function(observed, predicted) {
  stopifnot(length(observed) == length(predicted), length(observed) >= 2)
  good <- is.finite(observed) & is.finite(predicted)
  observed <- observed[good]
  predicted <- predicted[good]
  if (length(observed) < 2L) stop("Insufficient valid validation rows")

  sst <- sum((observed - mean(observed))^2)
  r_squared <- if (sst > 0) {
    1 - sum((observed - predicted)^2) / sst
  } else NA_real_
  pearson_r_squared <- if (stats::sd(observed) > 0 && stats::sd(predicted) > 0) {
    stats::cor(observed, predicted)^2
  } else NA_real_

  data.frame(
    n_holdout = length(observed),
    r_squared = r_squared,
    pearson_r_squared = pearson_r_squared,
    rmse = sqrt(mean((predicted - observed)^2)),
    mae = mean(abs(predicted - observed)),
    bias = mean(predicted - observed)
  )
}

fit_and_predict <- function(train, test, method, fold, seed = 42L) {
  if (nrow(train) < 100L || nrow(test) < 2L) {
    stop("Too few observations in a training or holdout split")
  }
  stopifnot(!anyNA(train[, model_features]), !anyNA(test[, model_features]))
  stopifnot(all(train$UFP > 0), all(test$UFP > 0))

  # Support weights approximate differences in site measurement support.
  weights <- log1p(train$n_obs_grid_total)
  weights <- weights / mean(weights)
  weights <- pmin(weights, 5)

  training_table <- train[, model_features, drop = FALSE]
  training_table$log_ufp <- log(train$UFP)
  fit <- ranger::ranger(
    dependent.variable.name = "log_ufp",
    data = training_table,
    num.trees = 180L,
    mtry = 3L,
    min.node.size = 5L,
    importance = "impurity",
    case.weights = weights,
    seed = seed
  )

  # Estimate Duan-style mean retransformation with training OOB residuals.
  # The validation observations never influence this estimate.
  oob <- as.numeric(fit$predictions)
  residuals_oob <- training_table$log_ufp - oob
  usable <- is.finite(residuals_oob)
  if (!any(usable)) stop("No valid OOB predictions for smearing")
  smear <- mean(exp(residuals_oob[usable]))

  pred_log <- as.numeric(predict(
    fit, data = test[, model_features, drop = FALSE]
  )$predictions)
  pred_ufp <- exp(pred_log) * smear

  data.frame(
    method = method,
    fold = as.character(fold),
    grid_id = test$grid_id,
    date = test$date,
    observed_ufp = test$UFP,
    predicted_ufp = pred_ufp,
    predicted_log_ufp = pred_log,
    training_smear_factor = smear,
    stringsAsFactors = FALSE
  )
}

run_validation_suite <- function(dat, seed = 2026L) {
  stopifnot(is.data.frame(dat), all(c(
    "grid_id", "x_m", "y_m", "date", "UFP", "n_obs_grid_total",
    model_features
  ) %in% names(dat)))

  predictions <- list()
  add <- function(result) predictions[[length(predictions) + 1L]] <<- result

  # 1) Random row holdout. Same sites can occur in train and test:
  # these results are not evidence of geographic extrapolation.
  set.seed(seed)
  test_idx <- sample(seq_len(nrow(dat)), size = floor(0.20 * nrow(dat)))
  add(fit_and_predict(
    dat[-test_idx, , drop = FALSE], dat[test_idx, , drop = FALSE],
    method = "Random 80/20", fold = "holdout", seed = seed
  ))

  # 2) Spatial five-fold k-means. Cluster UNIQUE locations first, so every
  # day's measurement from the same site belongs to one held-out fold.
  unique_sites <- unique(dat[, c("grid_id", "x_m", "y_m")])
  set.seed(seed)
  site_clusters <- stats::kmeans(
    scale(unique_sites[, c("x_m", "y_m")]),
    centers = 5L, nstart = 25L, iter.max = 100L
  )$cluster
  fold_lookup <- stats::setNames(site_clusters, unique_sites$grid_id)
  folds <- unname(fold_lookup[dat$grid_id])
  stopifnot(!anyNA(folds))

  for (held_out in sort(unique(folds))) {
    train <- dat[folds != held_out, , drop = FALSE]
    test <- dat[folds == held_out, , drop = FALSE]
    stopifnot(length(intersect(unique(train$grid_id), unique(test$grid_id))) == 0L)
    add(fit_and_predict(
      train, test, method = "Spatial 5-fold k-means",
      fold = paste0("cluster_", held_out), seed = seed + held_out
    ))
  }

  # 3) Chronological holdout. Train only on earlier dates.
  available_dates <- sort(unique(dat$date))
  cutoff <- available_dates[floor(0.8 * length(available_dates))]
  train <- dat[dat$date <= cutoff, , drop = FALSE]
  test <- dat[dat$date > cutoff, , drop = FALSE]
  stopifnot(max(train$date) < min(test$date))
  add(fit_and_predict(
    train, test, method = "Future-date holdout", fold = "last_20pct",
    seed = seed + 101L
  ))

  # 4) Analogue of leave-one-monitoring-site-out. Three selected synthetic
  # sites are separately omitted from training. These are NOT NABEL stations.
  site_positions <- unique(as.integer(round(seq(1, nrow(unique_sites), length.out = 3))))
  sites_to_hold_out <- unique_sites$grid_id[site_positions]
  for (j in seq_along(sites_to_hold_out)) {
    site <- sites_to_hold_out[j]
    add(fit_and_predict(
      dat[dat$grid_id != site, , drop = FALSE],
      dat[dat$grid_id == site, , drop = FALSE],
      method = "Leave-one-site-out demo", fold = site,
      seed = seed + 200L + j
    ))
  }

  heldout <- do.call(rbind, predictions)
  rownames(heldout) <- NULL
  method_order <- c(
    "Random 80/20", "Spatial 5-fold k-means",
    "Future-date holdout", "Leave-one-site-out demo"
  )
  summaries <- lapply(method_order, function(method) {
    sub <- heldout[heldout$method == method, , drop = FALSE]
    cbind(data.frame(method = method), predictive_metrics(
      sub$observed_ufp, sub$predicted_ufp
    ))
  })
  summary <- do.call(rbind, summaries)
  rownames(summary) <- NULL
  list(predictions = heldout, metrics = summary)
}
