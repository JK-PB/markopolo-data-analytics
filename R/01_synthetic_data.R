# MARKOPOLO portfolio demonstration
# Creates entirely synthetic spatially repeated daily observations.
# These are NOT original MARKOPOLO data, coordinates or research results.

make_synthetic_ufp <- function(n_sites = 64L, n_days = 60L, seed = 42L) {
  stopifnot(n_sites >= 15L, n_days >= 20L)
  set.seed(seed)

  # Simulated locations are spaced on a 25 m lattice but are fictitious.
  sites <- data.frame(
    grid_id = sprintf("SYN_%03d", seq_len(n_sites)),
    x_m = sample(seq(0, 9000, by = 25), n_sites, replace = FALSE),
    y_m = sample(seq(0, 9000, by = 25), n_sites, replace = FALSE),
    road_density = stats::runif(n_sites, 0.05, 1.0),
    ndvi = stats::runif(n_sites, 0.05, 0.9),
    population_density = exp(stats::runif(n_sites, 2.0, 7.0)),
    n_obs_grid_total = sample(5:120, n_sites, replace = TRUE)
  )

  panel <- expand.grid(
    site_index = seq_len(n_sites), day_index = seq_len(n_days),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  dat <- cbind(panel, sites[panel$site_index, , drop = FALSE])
  rownames(dat) <- NULL
  dat$date <- as.Date("2025-01-01") + dat$day_index - 1L

  doy <- as.integer(format(dat$date, "%j"))
  dat$day_sin <- sin(2 * pi * doy / 365.25)
  dat$day_cos <- cos(2 * pi * doy / 365.25)
  dat$temperature_c <- 8 + 7 * dat$day_sin + 0.0003 * dat$y_m +
    stats::rnorm(nrow(dat), sd = 2)
  dat$wind_speed <- pmax(0.1, 3 + 1.3 * dat$day_cos +
    stats::rnorm(nrow(dat), sd = 0.8))

  # Unobserved site/region effects make geographic transfer harder.
  site_effect <- stats::rnorm(n_sites, sd = 0.35)
  region_effect <- ifelse(sites$x_m > 4500, 0.28, -0.18) +
    ifelse(sites$y_m > 4500, 0.22, -0.12)
  daily_background <- stats::rnorm(n_days, sd = 0.09)

  latent_log_ufp <- 8.0 +
    0.75 * dat$road_density -
    0.45 * dat$ndvi +
    0.16 * log1p(dat$population_density) -
    0.025 * dat$temperature_c -
    0.065 * dat$wind_speed +
    site_effect[dat$site_index] +
    region_effect[dat$site_index] +
    daily_background[dat$day_index] +
    stats::rnorm(nrow(dat), sd = 0.17)

  dat$UFP <- round(exp(latent_log_ufp))
  dat <- dat[, c(
    "grid_id", "x_m", "y_m", "date", "UFP", "n_obs_grid_total",
    "road_density", "ndvi", "population_density", "temperature_c",
    "wind_speed", "day_sin", "day_cos"
  )]
  stopifnot(all(dat$UFP > 0), !anyNA(dat))
  dat
}
