# ==================================================================
# MARKOPOLO | Sanitized research code showcase
# Daily 25 m grid and master dataset assembly
# ==================================================================
# Adapted from a larger collaborative project for public code review.
# Real research processing logic is retained; this is NOT the runnable
# synthetic demo in ../run_demo.R. Original data and research models are
# not provided. Paths and some comments have been generalized.
# Requires separately authorized inputs and the original source schemas.
# This adapted copy has not been independently re-executed.
# ==================================================================

library(sf)         
library(terra)      
library(DBI)        
library(RSQLite)
library(dplyr)
library(arrow)      
library(readr)
library(lubridate)
library(RANN)
library(purrr)
library(base64enc)
library(data.table)


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

# ================== USER INPUTS ==================
# --- Inputs ---
grid_gpkg     <- file.path(input_root, "grid_reference_25m.gpkg")
grid_layer    <- NULL    # if NULL, first layer is used
# Required attribute column names present in the grid table (Will be used to do a quick centroid calculation):
col_left      <- "left"
col_right     <- "right"
col_top       <- "top"
col_bottom    <- "bottom"

# Optional helpful columns (kept if present)
col_row       <- "row_index"
col_col       <- "col_index"
col_id_guess  <- c("id","grid_id","ID","Id")

# Cantons (polygon source)
cant_gpkg     <- file.path(input_root, "canton_boundaries.gpkg")
cant_layer    <- "tlm_kantonsgebiet"
canton_name_col <- "name"

# --- Processing knobs ---
cellsize_m    <- 25
chunk_size    <- 5e6      # <-- with 500 GB RAM, start at 5M; try 10M if stable. How many rows each parquet chunk will contain
# Output mode: "parquet" (recommended for big data) or "memory" (small runs/testing)
out_mode      <- "parquet"
out_dir       <- file.path(output_root, "grid_canton_chunks")
out_prefix    <- "grid_canton"  # chunk files will be prefix_0001.parquet, etc.

# --- Inputs Altitude ---

in_dir_altitude   <- file.path(output_root, "grid_canton_chunks")
out_dir_altitude  <- file.path(output_root, "grid_with_altitude")
alt_tif  <- file.path(input_root, "elevation.tif")


# --- Inputs for daily measurement averaging ---

csv_path         <- file.path(input_root, "mobile_station_observations.csv")
master_parquet   <- out_dir_altitude             #  <- file path to previously created swisswide lookup table (See previous chunk)
out_parquet_daily_average      <- file.path(output_root, "daily_grid_measurements.parquet")

# --- Weather raster tiffs folders ---
# One or more roots, each scanned recursively.
# Example structure:
#   .../Tiff_daily_100m/2025/t2m_20250701.tiff
#   .../Tiff_daily_100m/2026/t2m_20260101.tiff
#   .../UV_Tiff_daily_100m/2025/uvb_20250701.tiff
weather_dir_roots <- c(
  file.path(input_root, "weather", "daily"),
  file.path(input_root, "weather", "uv_daily")
)

# --- Inputs for adding federal holidays
federal_holidays_csv <- file.path(input_root, "public_holidays.csv")
listed_col_name  <- "date_dd.mm"               # required column in the CSV

# ---- Population buffered rasters ----
pop_dir <- file.path(input_root, "population_rasters")
pop_tifs <- c(
  `25`  = file.path(pop_dir, "population_aligned_25m_counts.tif"),
  `50`  = file.path(pop_dir, "population_buffer_50m_mean.tif"),
  `100`  = file.path(pop_dir, "population_buffer_100m_mean.tif"),
  `250` = file.path(pop_dir, "population_buffer_250m_mean.tif")
)

# ---- NDVI buffered rasters (1 band each) ----
ndvi_dir <- file.path(input_root, "vegetation_rasters")
ndvi_tifs <- c(
  `25`  = file.path(ndvi_dir, "ndvi_filled_aligned_to_template.tif"),
  `250`  = file.path(ndvi_dir, "ndvi_filled_buffer_250m_mean.tif"),
  `500`  = file.path(ndvi_dir, "ndvi_filled_buffer_500m_mean.tif"),
  `1000` = file.path(ndvi_dir, "ndvi_filled_buffer_1000m_mean.tif")
)

# ---- Landcover buffered rasters ----
# Standard CORINE rasters: 16 bands each, fractions 0..1.
# The airports band is retained in these source rasters for compatibility,
# but it is intentionally NOT written to the master dataset from these files.
# Airports are assigned separately from dedicated larger-buffer, 1-band TIFs.
lc_dir <- file.path(input_root, "land_cover_rasters")
lc_tifs <- c(
  `250`  = file.path(lc_dir, "landcover_fraction_buffer_250m.tif"),
  `500`  = file.path(lc_dir, "landcover_fraction_buffer_500m.tif"),
  `1000` = file.path(lc_dir, "landcover_fraction_buffer_1000m.tif")
)

lc_band_names <- c(                                            # names of reclassified categories
  "urban_fabric","industry","commercial", "ports", "airports",
  "mine_dump_and_construction_sites","artificial_green",
  "arable","perm_crops","pastures","hetero_agri",
  "forests","shrub","bare","wetlands","inland_waters","marine_waters"
)

# Standard LC bands to retain from the multi-band files.
# "ports" and "airports" are excluded because they are replaced
# by dedicated larger-buffer rasters below.
lc_special_large_buffer_classes <- c("ports", "airports")

lc_standard_band_idx <- which(
  !(lc_band_names %in% lc_special_large_buffer_classes)
)

# Dedicated port-fraction rasters: 1 band each, fractions 0..1.
# Adjust raster names to match authorized inputs.
port_lc_tifs <- c(
  `2500`  = file.path(lc_dir, "port_frac_r2500.tif"),
  `5000`  = file.path(lc_dir, "port_frac_r5000.tif"),
  `10000` = file.path(lc_dir, "port_frac_r10000.tif")
)

# Dedicated airport-fraction rasters: 1 band each, fractions 0..1.
# Adjust raster names to match authorized inputs.
airport_lc_tifs <- c(
  `2500`  = file.path(lc_dir, "airport_frac_r2500.tif"),
  `5000`  = file.path(lc_dir, "airport_frac_r5000.tif"),
  `10000` = file.path(lc_dir, "airport_frac_r10000.tif")
)

# ---- ROADS: auto-detect types & buffers from filenames ----
# Pattern: roads_<type>_den_km2_r<buffer>.tif  (buffer ∈ {25,50,100})
roads_dir <- file.path(input_root, "road_density_rasters")

# ---- Highway binary flag ----
# Uses the 25 m no-tunnel motorway-density raster.
# If extracted value > 0, the grid is considered highway-adjacent / highway-influenced.
highway_flag_tif <- file.path(
  roads_dir,
  "NoTunnel_roads_motorway_den_km2_r25.tif"
)

highway_flag_col <- "highway_binary"

# --- traffic raster folder ---

traffic_root   <- file.path(input_root, "traffic_rasters")

# ---- Heating buffered rasters (4 bands each) ----
heating_dir <- file.path(input_root, "heating_rasters")
heating_tifs <- c(
  `250`  = file.path(heating_dir, "heating_category_counts_r250m_CH.tif"),
  `500`  = file.path(heating_dir, "heating_category_counts_r500m_CH.tif"),
  `1000` = file.path(heating_dir, "heating_category_counts_r1000m_CH.tif")
)

heating_band_names <- c(
  "Heating_no_pollution",
  "Heating_aux_pollution",
  "Heating_average_pollution",
  "Heating_high_pollution"
)

# ---- Additional Raster variables ----
ctm_tif   <- file.path(input_root, "chemical_transport_model.tif")
nabel_tif <- file.path(input_root, "station_background.tif")
distance_to_chiasso_tif <- file.path(input_root, "reference_distance.tif")
distance_to_major_road_tif <- file.path(input_root, "distance_major_roads.tif")
car_meters_gauss_tif <- file.path(input_root, "vehicle_activity.tif")
wood_burning_tif <- file.path(input_root, "wood_emissions.tif")
cyprus_ctm_tif <- file.path(input_root, "external_ctm.tif")
municipality_type_tif <- file.path(input_root, "municipality_type.tif")
fuel_shares_csv <- file.path(input_root, "vehicle_fleet.csv")
heating_tif <- file.path(input_root, "heating_multiband.tif")
svf_brut_tif <- paste0(
  file.path(input_root, "sky_view_factor.tif")
)



# --- Output for finished master dataset
out_parquet <- file.path(output_root, "master_dataset.parquet")


# ================== 0) EARLY SKIP GUARD — run before anything else ==================
# If chunked parquet files already exist, skip ALL processing below (incl. altitude)
# Uses the output configuration defined above.

skip_chunk_processing <- FALSE

if (identical(out_mode, "parquet")) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  existing_chunks <- list.files(
    out_dir,
    pattern = sprintf("^%s_\\d{4}\\.parquet$", out_prefix),
    full.names = TRUE
  )
  if (length(existing_chunks) > 0L) {
    message("Detected existing chunked parquet files in: ", out_dir)
    message("Skipping grid extraction AND altitude enrichment (", length(existing_chunks), " files found).")
    message(
      "Open lazily as one dataset with Arrow:\n",
      "  library(arrow)\n",
      "  ds <- open_dataset('", out_dir, "')\n",
      "  ds %>% select(grid_id, canton_name) %>% head(10) %>% collect()"
    )
    skip_chunk_processing <- TRUE
  }
}

# From here on, do nothing if we decided to skip
if (!isTRUE(skip_chunk_processing)) {
  
  # ================== 1a) Terra runtime knobs ==================
  terraOptions(todisk = TRUE, memfrac = 0.8, progress = 1)
  
  # ================== 1b) Check canton gpkgs for proper names and CRS ==================
  message("Reading cantons …")
  cantons <- st_read(cant_gpkg, layer = cant_layer, quiet = TRUE)
  stopifnot(nrow(cantons) > 0)
  stopifnot(canton_name_col %in% names(cantons))
  cantons <- cantons |>
    dplyr::rename(canton_name = !!canton_name_col)
  
  if (is.na(st_crs(cantons))) stop("Cantons layer lacks a CRS.")
  if (st_crs(cantons)$epsg != 2056) {
    message("Transforming cantons to EPSG:2056 …")
    cantons <- st_transform(cantons, 2056)
  }
  
  cantons$canton_code <- as.integer(factor(cantons$canton_name))
  lut <- cantons |> st_drop_geometry() |> distinct(canton_code, canton_name)
  
  # ================== 1c) Rasterize canton polygons for fast lookup ==================
  bb_c <- st_bbox(cantons)
  tmpl <- rast(
    extent = ext(bb_c["xmin"], bb_c["xmax"], bb_c["ymin"], bb_c["ymax"]),
    resolution = cellsize_m, crs = "EPSG:2056"
  )
  message("Rasterizing cantons at 25 m …")
  r_canton <- terra::rasterize(vect(cantons), tmpl, field = "canton_code", touches = TRUE)
  
  rm(cantons); gc()
  
  # ================== 1d) Inspect the gpkg using DBI (no geometry load) ==================
  ly <- if (is.null(grid_layer)) sf::st_layers(grid_gpkg)$name[1] else grid_layer
  message("Using grid layer: ", ly)
  
  con <- DBI::dbConnect(RSQLite::SQLite(), grid_gpkg)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  cols_present <- DBI::dbGetQuery(con, sprintf('PRAGMA table_info("%s");', ly))$name
  
  req <- c(col_left, col_right, col_top, col_bottom)
  if (!all(req %in% cols_present)) {
    stop("Required grid columns missing: ", paste(setdiff(req, cols_present), collapse=", "))
  }
  
  id_col <- intersect(col_id_guess, cols_present)
  id_col <- if (length(id_col)) id_col[1] else NA_character_
  
  total_rows <- DBI::dbGetQuery(con, sprintf('SELECT COUNT(*) AS n FROM "%s";', ly))$n
  message("Total grid rows: ", format(total_rows, big.mark=","))
  
  if (identical(out_mode, "parquet")) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  chunk_index <- 0L
  processed   <- 0L
  start_time  <- Sys.time()
  
  read_chunk <- function(offset, n) {
    DBI::dbGetQuery(con, sprintf('SELECT * FROM "%s" LIMIT %d OFFSET %d;', ly, n, offset))
  }
  
  # ================== 1f) Process chunks ==================
  repeat {
    n_take <- min(chunk_size, total_rows - processed)
    if (n_take <= 0) break
    
    chunk_index <- chunk_index + 1L
    offset <- processed
    
    message(sprintf("Chunk %d: reading rows %s–%s …",
                    chunk_index,
                    format(offset + 1, big.mark=","),
                    format(offset + n_take, big.mark=",")))
    
    d <- read_chunk(offset, n_take)
    
    # Compute centroids (LV95 m)
    cx <- (d[[col_left]]  + d[[col_right]]) / 2
    cy <- (d[[col_bottom]] + d[[col_top]])  / 2
    
    # Sample canton raster at centroids
    pts  <- vect(data.frame(x = cx, y = cy), geom = c("x","y"), crs = "EPSG:2056")
    vals <- terra::extract(r_canton, pts)[,2]
    rm(pts); gc()
    
    # Attach new fields
    d$centroid_x  <- cx
    d$centroid_y  <- cy
    d$canton_code <- as.integer(vals)
    d$canton_name <- lut$canton_name[match(d$canton_code, lut$canton_code)]
    
    # Ensure stable id
    if (is.na(id_col)) {
      d$grid_id <- seq.int(from = offset + 1L, length.out = n_take)
    } else {
      names(d)[names(d) == id_col] <- "grid_id"
    }
    
    keep_first <- c("grid_id", col_row, col_col, "centroid_x", "centroid_y", "canton_code", "canton_name")
    keep_first <- intersect(keep_first, names(d))
    d <- d[, c(keep_first, setdiff(names(d), keep_first))]
    
    if (identical(out_mode, "parquet")) {
      fn <- file.path(out_dir, sprintf("%s_%04d.parquet", out_prefix, chunk_index))
      arrow::write_parquet(d, fn)
      rm(d); gc()
    }
    
    processed <- processed + n_take
    dt <- difftime(Sys.time(), start_time, units = "mins")
    message(sprintf("  processed %s / %s (%.1f%%) in %.1f min",
                    format(processed, big.mark=","),
                    format(total_rows, big.mark=","),
                    100*processed/total_rows, as.numeric(dt)))
  }
  
  if (identical(out_mode, "parquet")) {
    message("Done. Wrote chunked files to: ", out_dir,
            "\nOpen lazily as one dataset with Arrow:",
            "\n  library(arrow)",
            "\n  ds <- open_dataset('", out_dir, "')",
            "\n  ds %>% select(grid_id, canton_name) %>% head(10) %>% collect()")
  } else {
    result <- dplyr::bind_rows(result_list)
    rm(result_list); gc()
    message("Done. Result in memory: object 'result' with ",
            format(nrow(result), big.mark=","), " rows.")
  }
  
  # ================== 1g) Add altitude (m) to existing Parquet chunks ==================
  # NOTE: This whole block is ALSO skipped when early-skip is TRUE (top guard)
  dir.create(out_dir_altitude, showWarnings = FALSE, recursive = TRUE)
  
  r_alt <- rast(alt_tif)
  if (is.na(crs(r_alt))) stop("Altitude GeoTIFF has no CRS set.")
  
  files <- list.files(in_dir_altitude, "\\.parquet$", full.names = TRUE)
  stopifnot(length(files) > 0)
  
  for (f in files) {
    message("Processing: ", basename(f))
    df <- read_parquet(f)
    
    if (!all(c("centroid_x","centroid_y") %in% names(df))) {
      stop("centroid_x/centroid_y not found in ", f)
    }
    
    pts  <- vect(df[, c("centroid_x","centroid_y")], geom = c("centroid_x","centroid_y"), crs = "EPSG:2056")
    elev <- terra::extract(r_alt, pts)[,2]
    rm(pts); gc()
    
    df$altitude_m <- elev
    
    write_parquet(df, file.path(out_dir_altitude, basename(f)), compression = "zstd")
    rm(df); gc()
  }
  
  message("Done. New dataset at: ", out_dir_altitude,
          "\nQuick check:\n",
          "  library(arrow); library(dplyr)\n",
          "  ds <- open_dataset('", out_dir_altitude, "')\n",
          "  ds %>% select(centroid_x, centroid_y, canton_name, altitude_m) %>%",
          " filter(!is.na(altitude_m)) %>% head(10) %>% collect()")
  
} # <-- end of !skip guard


# ===============================================
#
#   2. Join the grid lookup with observation records to obtain sampled grid-days.
#   with observations aggregated to the day and grid cell.
#
# ===============================================

# ================== 2a) Check names, pull desired columns and pollutants ==================
# Expected centroid column names in master parquet (auto-detected if NULL)
master_centroid_x_col <- "centroid_x"   
master_centroid_y_col <- "centroid_y"   

# Also pull the index rows, canton names and altitude, drop the rest like geometry
maybe_cols <- c("row_index", "col_index", "canton_name", "altitude_m")

# Pollutants to average
pollutant_cols <- c(
  "UFP", "NO2", "CO2", "PM25", "PNC", "PNC10", "PNC26", "PNC42",
  "PNC69", "PNC113", "PNC184", "PNC300",
  "BC1_MA", "BC6_MA", "BC_BB"
)

# Snap tolerance (max distance from centroid for a valid in-cell match)
cell_size_m     <- 25
snap_max_m      <- sqrt(2) * (cell_size_m/2)  # 17.677...
bbox_pad_m      <- 100                        # expand bbox when querying master centroids

# ================== 2b) Read measurement CSV and check columns ==================
dat_raw <- readr::read_csv(csv_path, guess_max = 200000, show_col_types = FALSE) 

# Detect station predictor columns included by upstream preprocessing.
# Example: BAS_NABEL, HAE_NABEL, etc.
nabel_predictor_cols <- grep("_NABEL$", names(dat_raw), value = TRUE)

if (length(nabel_predictor_cols)) {
  message(
    "Detected NABEL predictor columns: ",
    paste(nabel_predictor_cols, collapse = ", ")
  )
  
  dat_raw <- dat_raw %>%
    mutate(
      across(
        all_of(nabel_predictor_cols),
        ~ suppressWarnings(as.numeric(.x))
      )
    )
} else {
  message("No *_NABEL predictor columns detected in raw measurement CSV.")
}

need_cols <- c("Longitude","Latitude","DateTime","NABEL_site")
missing_cols <- setdiff(need_cols, names(dat_raw))
if (length(missing_cols)) stop("Missing column(s) in CSV: ", paste(missing_cols, collapse=", "))

if (!inherits(dat_raw$DateTime, "POSIXct")) stop("'DateTime' must be POSIXct.")
attr(dat_raw$DateTime, "tzone") <- "UTC"

# Explicitly define / clean NABEL_site as integer 0/1
dat_raw$NABEL_site <- suppressWarnings(as.integer(dat_raw$NABEL_site))

bad_nabel <- which(is.na(dat_raw$NABEL_site) | !(dat_raw$NABEL_site %in% c(0L, 1L)))
if (length(bad_nabel) > 0) {
  warning(
    "Found ", length(bad_nabel),
    " row(s) with invalid NABEL_site values. Setting them to NA."
  )
  dat_raw$NABEL_site[bad_nabel] <- NA_integer_
}

dat_raw <- dat_raw %>%
  filter(!is.na(Longitude), !is.na(Latitude), !is.na(DateTime), !is.na(NABEL_site))

# ================== 2c) Transform into CRS:2056 ==================
crs_wgs84 <- "EPSG:4326"
crs_lv95  <- "EPSG:2056"

pts_v <- vect(dat_raw, geom = c("Longitude", "Latitude"), crs = crs_wgs84)
pts_lv95 <- project(pts_v, crs_lv95)
xy <- crds(pts_lv95)

dat_lv95 <- dat_raw %>%
  mutate(
    x_lv95 = xy[, 1],
    y_lv95 = xy[, 2]
  )

rm(pts_v, pts_lv95, xy)

# ================== 2d) Determine bbox and load lookup table centroids ==================
ds <- arrow::open_dataset(master_parquet)
ds_cols <- names(ds)

if (!"grid_id" %in% ds_cols) {
  stop("The master parquet must contain a 'grid_id' column. Please add it or point to the correct dataset.")
}

x_min <- min(dat_lv95$x_lv95, na.rm = TRUE) - bbox_pad_m
x_max <- max(dat_lv95$x_lv95, na.rm = TRUE) + bbox_pad_m
y_min <- min(dat_lv95$y_lv95, na.rm = TRUE) - bbox_pad_m
y_max <- max(dat_lv95$y_lv95, na.rm = TRUE) + bbox_pad_m

select_cols <- c("grid_id", master_centroid_x_col, master_centroid_y_col, intersect(maybe_cols, ds_cols))

centroids <- ds %>%
  dplyr::select(dplyr::all_of(select_cols)) %>%
  dplyr::filter(
    !!as.name(master_centroid_x_col) >= x_min,
    !!as.name(master_centroid_x_col) <= x_max,
    !!as.name(master_centroid_y_col) >= y_min,
    !!as.name(master_centroid_y_col) <= y_max
  ) %>%
  dplyr::distinct() %>%
  dplyr::collect()

if (nrow(centroids) == 0L) {
  stop("No master centroids found in the bbox of the measurements. ",
       "Check CRS, bbox padding, or centroid column names.")
}

centroids <- centroids %>%
  rename(
    centroid_x = !!master_centroid_x_col,
    centroid_y = !!master_centroid_y_col
  )

# ================== 2e) Nearest Neighbor snap of measurement points to centroids  ==================
cent_mat <- as.matrix(centroids[, c("centroid_x","centroid_y")])
qry_mat  <- as.matrix(dat_lv95[, c("x_lv95","y_lv95")])

nn <- RANN::nn2(cent_mat, query = qry_mat, k = 1)

dat_matched <- dat_lv95 %>%
  mutate(
    nn_index = nn$nn.idx[,1],
    nn_dist  = nn$nn.dists[,1]
  ) %>%
  filter(is.finite(nn_dist) & nn_dist <= snap_max_m) %>%
  bind_cols(centroids[.$nn_index, , drop = FALSE]) %>%
  select(
    DateTime,
    NABEL_site,
    all_of(pollutant_cols[pollutant_cols %in% names(.)]),
    any_of(nabel_predictor_cols),
    grid_id,
    centroid_x, centroid_y,
    any_of(intersect(maybe_cols, names(.)))
  )

n_total <- nrow(dat_lv95)
n_kept  <- nrow(dat_matched)
n_drop  <- n_total - n_kept
drop_pct <- round(100 * n_drop / max(1, n_total), 3)

message("Matched points: ", n_kept, " / ", n_total, " (dropped ", n_drop, " = ", drop_pct, "%) ",
        "with snap_max_m = ", round(snap_max_m, 3), " m.")

if (n_kept == 0L) {
  stop("All points were outside the snap radius. Consider increasing 'bbox_pad_m' or verifying CRS.")
}

# Helper: keep the first non-NA value in a group.
# Station predictor columns have already been aggregated daily.
first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  x[1]
}

# ================== 2f) Daily means of pollutants + campaign-level observation counts ==================

dat_matched <- dat_matched %>%
  mutate(date_utc = as.Date(DateTime, tz = "UTC"))

# Total number of raw matched measurements per grid over the entire campaign
grid_obs_total <- dat_matched %>%
  group_by(grid_id) %>%
  summarise(
    n_obs_grid_total = dplyr::n(),
    n_days_grid_total = dplyr::n_distinct(date_utc),
    .groups = "drop"
  )

# Daily grid-level pollutant averages
pollutant_cols_present <- intersect(pollutant_cols, names(dat_matched))
nabel_predictor_cols_present <- intersect(nabel_predictor_cols, names(dat_matched))

dat_daily <- dat_matched %>% 
  group_by(
    grid_id,
    centroid_x, centroid_y,
    NABEL_site,
    across(any_of(intersect(maybe_cols, names(.)))),
    date_utc
  ) %>%
  summarise(
    # Raw pollutant measurements: average within grid/day
    across(
      all_of(pollutant_cols_present),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}"
    ),
    
    # NABEL predictor columns: already daily, so just carry the daily value through
    across(
      all_of(nabel_predictor_cols_present),
      ~ first_non_na(.x),
      .names = "{.col}"
    ),
    
    n_obs = dplyr::n(),
    .groups = "drop"
  ) %>%
  left_join(grid_obs_total, by = "grid_id")

# ================== 2g) Write the daily parquet separately  ==================
arrow::write_parquet(dat_daily, out_parquet_daily_average)
cat("✓ Daily-by-grid data written to:\n   ", out_parquet_daily_average, "\n")

df <- arrow::read_parquet(out_parquet_daily_average)
glimpse(df)


# ===============================================
# 3) Daily weather from DAILY GeoTIFFs (CRS: EPSG:2056)
#    OPTIMISED VERSION:
#      - assign raster-cell IDs once
#      - extract unique weather cells only
#      - extract each source date once
#      - reuse source-date values for NOW and LAG
# ===============================================

# Use data.table for the weather block so adding/filling many columns
# happens by reference instead of repeatedly copying a large data.frame.
master_df <- data.table::as.data.table(dat_daily)

stopifnot(
  all(
    c("date_utc", "centroid_x", "centroid_y") %in%
      names(master_df)
  )
)

master_df$date_utc <- as.Date(master_df$date_utc)

weather_vars <- c(
  "t2m",
  "d2m",
  "u10",
  "v10",
  "sp",
  "tp",
  "sf",
  "snowc",
  "tcc",
  "blh",
  "RH_percent",
  "uvb",
  "ssrd"
)

weather_vars_plus <- weather_vars
lag_suffix        <- "_lag1d"

fmt_day <- function(xDate) {
  format(as.Date(xDate), "%Y%m%d")
}

# ============================================================
# A. Check weather roots
# ============================================================

if (!length(weather_dir_roots)) {
  stop("weather_dir_roots is empty.")
}

weather_dir_roots <- unique(
  normalizePath(
    weather_dir_roots,
    winslash = "/",
    mustWork = FALSE
  )
)

missing_roots <- weather_dir_roots[
  !dir.exists(weather_dir_roots)
]

if (length(missing_roots)) {
  stop(
    "The following weather root folder(s) do not exist:\n",
    paste(
      missing_roots,
      collapse = "\n"
    )
  )
}

# ============================================================
# B. Build weather-file inventory
# ============================================================

scan_one_weather_root <- function(root) {
  list.files(
    root,
    pattern    = "\\.(tif|tiff)$",
    full.names = TRUE,
    recursive  = TRUE,
    ignore.case = TRUE
  )
}

parse_weather_meta <- function(path) {
  
  bn <- basename(path)
  
  m <- regexec(
    "^([A-Za-z0-9_]+)_([0-9]{8})\\.(?:tif|tiff)$",
    bn,
    ignore.case = TRUE
  )
  
  res <- regmatches(
    bn,
    m
  )[[1]]
  
  if (!length(res)) {
    return(
      data.frame(
        var       = NA_character_,
        date_tag  = NA_character_,
        full_path = path,
        stringsAsFactors = FALSE
      )
    )
  }
  
  data.frame(
    var       = res[2],
    date_tag  = res[3],
    full_path = path,
    stringsAsFactors = FALSE
  )
}

safe_rast <- function(fp) {
  tryCatch(
    terra::rast(fp),
    error = function(e) NULL
  )
}

all_weather_files <- unlist(
  lapply(
    weather_dir_roots,
    scan_one_weather_root
  ),
  use.names = FALSE
)

if (!length(all_weather_files)) {
  stop(
    "No .tif/.tiff weather files found under any supplied ",
    "weather_dir_roots:\n",
    paste(
      weather_dir_roots,
      collapse = "\n"
    )
  )
}

weather_meta_df <- do.call(
  rbind,
  lapply(
    all_weather_files,
    parse_weather_meta
  )
)

weather_meta_df <- weather_meta_df[
  !is.na(weather_meta_df$var) &
    weather_meta_df$var %in% weather_vars,
  ,
  drop = FALSE
]

if (!nrow(weather_meta_df)) {
  stop(
    "No weather files matched <var>_YYYYMMDD.tif(f) for vars: ",
    paste(
      weather_vars,
      collapse = ", "
    )
  )
}

# ============================================================
# C. Restrict inventory to dates required by the master table
# ============================================================

dates_needed <- sort(
  unique(
    master_df$date_utc
  )
)

# Each master date requires:
#   - current-day weather
#   - previous-day weather for lag1d
source_dates_needed <- sort(
  unique(
    c(
      dates_needed,
      dates_needed - 1L
    )
  )
)

source_tags_needed <- fmt_day(
  source_dates_needed
)

weather_meta_df <- weather_meta_df[
  weather_meta_df$date_tag %in% source_tags_needed,
  ,
  drop = FALSE
]

if (!nrow(weather_meta_df)) {
  stop(
    "No weather files remain after filtering to required dates."
  )
}

message(
  "Weather inventory ready: ",
  format(
    nrow(weather_meta_df),
    big.mark = ","
  ),
  " files for ",
  length(
    unique(
      weather_meta_df$date_tag
    )
  ),
  " required source dates across ",
  length(weather_dir_roots),
  " root folder(s)."
)

# ============================================================
# D. Stop if duplicate rasters exist
# ============================================================

dup_tab <- weather_meta_df %>%
  dplyr::count(
    var,
    date_tag,
    name = "n"
  ) %>%
  dplyr::filter(
    n > 1
  )

if (nrow(dup_tab) > 0) {
  
  dup_show <- weather_meta_df %>%
    dplyr::semi_join(
      dup_tab,
      by = c(
        "var",
        "date_tag"
      )
    ) %>%
    dplyr::arrange(
      date_tag,
      var,
      full_path
    )
  
  stop(
    "Duplicate weather rasters found across supplied roots.\n",
    "Each (var, date_tag) combination must exist at most once.\n\n",
    paste(
      sprintf(
        "%s | %s | %s",
        dup_show$var,
        dup_show$date_tag,
        dup_show$full_path
      ),
      collapse = "\n"
    )
  )
}

# ============================================================
# E. Fast weather-file lookup
# ============================================================

weather_lookup <- setNames(
  weather_meta_df$full_path,
  paste(
    weather_meta_df$var,
    weather_meta_df$date_tag,
    sep = "|"
  )
)

get_weather_file <- function(var, date_tag) {
  
  key <- paste(
    var,
    date_tag,
    sep = "|"
  )
  
  val <- weather_lookup[key]
  
  if (
    length(val) == 0L ||
    is.na(val) ||
    !nzchar(val)
  ) {
    return(NA_character_)
  }
  
  unname(
    val[[1]]
  )
}

# ============================================================
# F. Find one valid reference raster
#
# All daily weather rasters must share the same:
#   - CRS
#   - extent
#   - resolution
#   - raster origin
#   - dimensions
#
# This is required because raster-cell IDs are calculated once
# and then reused for all dates and variables.
# ============================================================

reference_raster <- NULL
reference_path   <- NA_character_

candidate_paths <- unique(
  weather_meta_df$full_path
)

for (fp in candidate_paths) {
  
  r_try <- safe_rast(fp)
  
  if (is.null(r_try)) {
    next
  }
  
  if (terra::nlyr(r_try) != 1L) {
    next
  }
  
  reference_raster <- r_try
  reference_path   <- fp
  
  break
}

if (is.null(reference_raster)) {
  stop(
    "Could not find a readable single-layer weather raster ",
    "to use as the reference grid."
  )
}

message(
  "Reference weather raster: ",
  basename(reference_path)
)

# ============================================================
# G. Calculate weather-raster cell IDs once
#
# This replaces repeated spatial point extraction.
#
# Many 25 m master-grid centroids fall within the same coarser
# weather-raster cell. We therefore extract each weather cell
# only once and copy its values to all corresponding master rows.
# ============================================================

xy_all <- as.matrix(
  master_df[
    ,
    c(
      "centroid_x",
      "centroid_y"
    ),
    drop = FALSE
  ]
)

storage.mode(xy_all) <- "double"

weather_cell_by_row <- terra::cellFromXY(
  reference_raster,
  xy_all
)

outside_weather_extent <- which(
  is.na(weather_cell_by_row)
)

if (length(outside_weather_extent)) {
  warning(
    format(
      length(outside_weather_extent),
      big.mark = ","
    ),
    " master rows fall outside the weather-raster extent. ",
    "Their weather values will remain NA."
  )
}

n_unique_coordinates <- data.table::uniqueN(
  master_df,
  by = c(
    "centroid_x",
    "centroid_y"
  )
)

n_unique_weather_cells <- length(
  unique(
    stats::na.omit(
      weather_cell_by_row
    )
  )
)

message(
  "Weather-cell reduction: ",
  format(
    nrow(master_df),
    big.mark = ","
  ),
  " master rows -> ",
  format(
    n_unique_coordinates,
    big.mark = ","
  ),
  " unique 25 m coordinates -> ",
  format(
    n_unique_weather_cells,
    big.mark = ","
  ),
  " unique weather-raster cells."
)

# ============================================================
# H. Split master rows by date once
#
# This replaces:
#
#   which(master_df$date_utc == dd)
#
# inside every loop iteration.
# ============================================================

master_date_tags <- fmt_day(
  master_df$date_utc
)

rows_by_date_tag <- split(
  seq_len(
    nrow(master_df)
  ),
  master_date_tags
)

get_rows_for_tag <- function(date_tag) {
  
  rows <- rows_by_date_tag[[date_tag]]
  
  if (is.null(rows)) {
    return(
      integer(0)
    )
  }
  
  rows
}

# Also cache the unique weather cells needed by each master date.
# This avoids rebuilding unique(cell IDs) from row vectors in every
# source-date iteration. It is small, cheap to keep in memory, and
# especially useful when the campaign contains many observed days.
weather_cells_by_date_tag <- lapply(
  rows_by_date_tag,
  function(rows) {
    sort(
      unique(
        stats::na.omit(
          weather_cell_by_row[
            rows
          ]
        )
      )
    )
  }
)

get_cells_for_tag <- function(date_tag) {
  
  cells <- weather_cells_by_date_tag[[date_tag]]
  
  if (is.null(cells)) {
    return(
      numeric(0)
    )
  }
  
  cells
}

# ============================================================
# I. Preallocate all output columns
# ============================================================

# Pre-allocate by reference. This avoids repeated full-table copies.
data.table::setalloccol(
  master_df,
  ncol(master_df) + 2L * length(weather_vars) + 10L
)

for (v in weather_vars) {
  data.table::set(
    master_df,
    j = v,
    value = NA_real_
  )
  data.table::set(
    master_df,
    j = paste0(v, lag_suffix),
    value = NA_real_
  )
}

# ============================================================
# J. Build a weather stack for one source date
#
# Fast path:
#   Open all files for the source date as one stack.
#
# Fallback path:
#   If one raster is malformed or geometrically inconsistent,
#   inspect files individually and retain all valid layers.
# ============================================================

build_weather_stack_for_date <- function(
    date_tag,
    weather_vars,
    get_weather_file,
    reference_raster
) {
  
  fps <- vapply(
    weather_vars,
    function(v) {
      get_weather_file(
        v,
        date_tag
      )
    },
    character(1)
  )
  
  names(fps) <- weather_vars
  
  present <- !is.na(fps) &
    nzchar(fps) &
    file.exists(fps)
  
  if (!any(present)) {
    return(
      list(
        stack                 = NULL,
        missing_vars          = weather_vars,
        bad_open              = character(),
        bad_geom              = character(),
        unexpected_multilayer = character()
      )
    )
  }
  
  fps_present <- fps[
    present
  ]
  
  # ----------------------------------------------------------
  # Fast path
  # ----------------------------------------------------------
  
  stack_fast <- tryCatch(
    terra::rast(
      unname(
        fps_present
      )
    ),
    error = function(e) NULL
  )
  
  fast_path_valid <- !is.null(stack_fast) &&
    terra::nlyr(stack_fast) == length(fps_present) &&
    isTRUE(
      terra::compareGeom(
        stack_fast,
        reference_raster,
        stopOnError = FALSE
      )
    )
  
  if (fast_path_valid) {
    
    names(stack_fast) <- names(
      fps_present
    )
    
    return(
      list(
        stack                 = stack_fast,
        missing_vars          = setdiff(
          weather_vars,
          names(stack_fast)
        ),
        bad_open              = character(),
        bad_geom              = character(),
        unexpected_multilayer = character()
      )
    )
  }
  
  # ----------------------------------------------------------
  # Robust fallback path
  # ----------------------------------------------------------
  
  rasters <- list()
  
  bad_open <- character()
  bad_geom <- character()
  unexpected_multilayer <- character()
  
  for (v in names(fps_present)) {
    
    fp <- fps_present[[v]]
    
    r <- safe_rast(fp)
    
    if (is.null(r)) {
      bad_open <- c(
        bad_open,
        fp
      )
      
      next
    }
    
    if (terra::nlyr(r) != 1L) {
      unexpected_multilayer <- c(
        unexpected_multilayer,
        fp
      )
      
      next
    }
    
    same_geometry <- terra::compareGeom(
      r,
      reference_raster,
      stopOnError = FALSE
    )
    
    if (!isTRUE(same_geometry)) {
      bad_geom <- c(
        bad_geom,
        fp
      )
      
      next
    }
    
    names(r) <- v
    
    rasters[[v]] <- r
  }
  
  if (!length(rasters)) {
    return(
      list(
        stack                 = NULL,
        missing_vars          = weather_vars,
        bad_open              = bad_open,
        bad_geom              = bad_geom,
        unexpected_multilayer = unexpected_multilayer
      )
    )
  }
  
  s <- terra::rast(
    rasters
  )
  
  names(s) <- names(
    rasters
  )
  
  list(
    stack = s,
    missing_vars = setdiff(
      weather_vars,
      names(s)
    ),
    bad_open              = bad_open,
    bad_geom              = bad_geom,
    unexpected_multilayer = unexpected_multilayer
  )
}

# ============================================================
# K. Extract each source date exactly once
#
# Example:
#
#   Source weather date: 2026-02-06
#
#   Its values are used:
#     - as NOW columns for master date 2026-02-06
#     - as LAG columns for master date 2026-02-07
#
# This removes the duplicated lag extraction.
# ============================================================

dates_skipped_now <- as.Date(
  character()
)

dates_skipped_lag <- as.Date(
  character()
)

partial_weather_dates <- character()

failed_open_all <- character()
failed_geom_all <- character()
unexpected_multilayer_all <- character()

weather_assignment_start <- Sys.time()

n_source_dates <- length(
  source_dates_needed
)

for (i in seq_along(source_dates_needed)) {
  
  source_date <- source_dates_needed[i]
  source_tag  <- fmt_day(source_date)
  
  # ----------------------------------------------------------
  # Rows receiving current-day weather from source_date
  # ----------------------------------------------------------
  
  rows_now <- get_rows_for_tag(
    source_tag
  )
  
  # ----------------------------------------------------------
  # Rows receiving lagged weather from source_date
  #
  # Weather from source_date is lag1d weather for source_date + 1.
  # ----------------------------------------------------------
  
  lag_target_date <- source_date + 1L
  lag_target_tag  <- fmt_day(lag_target_date)
  
  rows_lag <- get_rows_for_tag(
    lag_target_tag
  )
  
  rows_requiring_source <- c(
    rows_now,
    rows_lag
  )
  
  if (!length(rows_requiring_source)) {
    next
  }
  
  # ----------------------------------------------------------
  # Extract only unique weather-raster cells
  # ----------------------------------------------------------
  
  cells_needed <- sort(
    unique(
      c(
        get_cells_for_tag(source_tag),
        get_cells_for_tag(lag_target_tag)
      )
    )
  )
  
  if (!length(cells_needed)) {
    next
  }
  
  # ----------------------------------------------------------
  # Open source-date stack once
  # ----------------------------------------------------------
  
  stack_info <- build_weather_stack_for_date(
    date_tag         = source_tag,
    weather_vars     = weather_vars,
    get_weather_file = get_weather_file,
    reference_raster = reference_raster
  )
  
  failed_open_all <- c(
    failed_open_all,
    stack_info$bad_open
  )
  
  failed_geom_all <- c(
    failed_geom_all,
    stack_info$bad_geom
  )
  
  unexpected_multilayer_all <- c(
    unexpected_multilayer_all,
    stack_info$unexpected_multilayer
  )
  
  # ----------------------------------------------------------
  # Keep NAs if no valid source raster is available
  # ----------------------------------------------------------
  
  if (is.null(stack_info$stack)) {
    
    if (length(rows_now)) {
      dates_skipped_now <- c(
        dates_skipped_now,
        source_date
      )
    }
    
    if (length(rows_lag)) {
      dates_skipped_lag <- c(
        dates_skipped_lag,
        lag_target_date
      )
    }
    
    next
  }
  
  if (length(stack_info$missing_vars)) {
    partial_weather_dates <- c(
      partial_weather_dates,
      source_tag
    )
  }
  
  # ----------------------------------------------------------
  # Read raster values once using raster-cell IDs
  #
  # Passing numeric cell IDs avoids repeated spatial matching
  # between point geometries and raster cells.
  #
  # IMPORTANT:
  # ID = FALSE must not be supplied here because cells_needed is
  # a numeric vector of raster-cell IDs rather than a SpatVector.
  # ----------------------------------------------------------
  
  extracted <- terra::extract(
    stack_info$stack,
    cells_needed
  )
  
  extracted <- data.table::as.data.table(
    extracted,
    keep.rownames = FALSE
  )
  
  vars_available <- intersect(
    weather_vars,
    names(extracted)
  )
  
  if (!length(vars_available)) {
    next
  }
  
  # ----------------------------------------------------------
  # Assign NOW weather
  # ----------------------------------------------------------
  
  if (length(rows_now)) {
    
    position_now <- match(
      weather_cell_by_row[
        rows_now
      ],
      cells_needed
    )
    
    valid_now <- !is.na(
      position_now
    )
    
    if (any(valid_now)) {
      
      rhs_now <- extracted[
        position_now[valid_now],
        vars_available,
        with = FALSE
      ]
      
      master_df[
        rows_now[valid_now],
        (vars_available) := rhs_now
      ]
      
      rm(rhs_now)
    }
  }
  
  # ----------------------------------------------------------
  # Assign lagged weather
  # ----------------------------------------------------------
  
  if (length(rows_lag)) {
    
    position_lag <- match(
      weather_cell_by_row[
        rows_lag
      ],
      cells_needed
    )
    
    valid_lag <- !is.na(
      position_lag
    )
    
    lag_columns_available <- paste0(
      vars_available,
      lag_suffix
    )
    
    if (any(valid_lag)) {
      
      rhs_lag <- extracted[
        position_lag[valid_lag],
        vars_available,
        with = FALSE
      ]
      
      master_df[
        rows_lag[valid_lag],
        (lag_columns_available) := rhs_lag
      ]
      
      rm(rhs_lag)
    }
  }
  
  rm(
    stack_info,
    extracted,
    cells_needed,
    rows_requiring_source,
    rows_now,
    rows_lag,
    vars_available
  )
  
  # Running gc() after every date adds avoidable overhead.
  # Every 250 source dates is sufficient for a long run.
  if (
    i %% 250L == 0L ||
    i == n_source_dates
  ) {
    
    elapsed_minutes <- as.numeric(
      difftime(
        Sys.time(),
        weather_assignment_start,
        units = "mins"
      )
    )
    
    message(
      "Processed weather source date ",
      as.character(source_date),
      " | ",
      format(
        i,
        big.mark = ","
      ),
      " / ",
      format(
        n_source_dates,
        big.mark = ","
      ),
      " | elapsed: ",
      round(
        elapsed_minutes,
        1
      ),
      " min"
    )
    
    gc(FALSE)
  }
}

# ============================================================
# L. Finalise output
# ============================================================

master_with_weather <- as.data.frame(master_df) %>%
  dplyr::relocate(
    dplyr::any_of(
      c(
        weather_vars_plus,
        paste0(
          weather_vars_plus,
          lag_suffix
        )
      )
    ),
    .after = dplyr::last_col()
  )

# ============================================================
# M. Diagnostics
# ============================================================

if (length(dates_skipped_now)) {
  message(
    "\n=== MASTER DATES KEPT WITH ALL NOW WEATHER VALUES AS NA ===\n",
    paste(
      sort(
        unique(
          as.character(
            dates_skipped_now
          )
        )
      ),
      collapse = ", "
    )
  )
}

if (length(dates_skipped_lag)) {
  message(
    "\n=== MASTER DATES KEPT WITH ALL LAG WEATHER VALUES AS NA ===\n",
    paste(
      sort(
        unique(
          as.character(
            dates_skipped_lag
          )
        )
      ),
      collapse = ", "
    )
  )
}

if (length(partial_weather_dates)) {
  message(
    "\nSome source dates contained only a subset of the requested ",
    "weather variables. Missing variable values were retained as NA.\n",
    "Number of affected source dates: ",
    length(
      unique(
        partial_weather_dates
      )
    )
  )
}

if (length(failed_open_all)) {
  message(
    "\n=== FILES THAT FAILED TO OPEN ==="
  )
  
  print(
    sort(
      unique(
        failed_open_all
      )
    )
  )
}

if (length(failed_geom_all)) {
  message(
    "\n=== FILES WITH GEOMETRY MISMATCH ===\n",
    "These files were skipped because the fast cell-ID method ",
    "requires one common weather-raster grid."
  )
  
  print(
    sort(
      unique(
        failed_geom_all
      )
    )
  )
}

if (length(unexpected_multilayer_all)) {
  message(
    "\n=== UNEXPECTED MULTILAYER WEATHER FILES ===\n",
    "Daily weather GeoTIFFs should contain exactly one layer. ",
    "The following files were skipped:"
  )
  
  print(
    sort(
      unique(
        unexpected_multilayer_all
      )
    )
  )
}

weather_assignment_elapsed <- as.numeric(
  difftime(
    Sys.time(),
    weather_assignment_start,
    units = "mins"
  )
)

message(
  "\nDaily weather (incl. RH) + 1-day lag joined for ",
  format(
    nrow(master_with_weather),
    big.mark = ","
  ),
  " rows across ",
  format(
    length(dates_needed),
    big.mark = ","
  ),
  " master dates.\n",
  "Weather extraction elapsed time: ",
  round(
    weather_assignment_elapsed,
    1
  ),
  " min."
)




# ===============================================
#
#   4. Add temporal identifiers and holidays
#
# ===============================================

df <- master_with_weather

# ================== 4a) Add cyclical time identifiers ==================
df <- df %>%
  mutate(
    # Weekday: 1 = Monday ... 7 = Sunday
    weekday_num = lubridate::wday(date_utc, week_start = 1),
    
    # Convert to 0–6 for cyclic transform
    weekday0_6  = weekday_num - 1L,
    
    # Cyclical encoding of weekday (7-day cycle)
    weekday_sin = sin(2 * pi * (weekday0_6 / 7)),
    weekday_cos = cos(2 * pi * (weekday0_6 / 7)),
    
    # Day-of-year (0–364 or 365)
    doy0_365    = lubridate::yday(date_utc) - 1L,
    
    # Cyclical encoding of annual cycle
    day_sin     = sin(2 * pi * (doy0_365 / 365)),
    day_cos     = cos(2 * pi * (doy0_365 / 365))
  ) %>%
  relocate(
    weekday_num, weekday_sin, weekday_cos,
    doy0_365, day_sin, day_cos,
    .after = dplyr::last_col()
  )

master_with_weather <- df


# ================== 4b) Add binary Swiss COVID-lockdown flag ==================
# Strict national restriction windows used in the previous NABEL analysis.
# Boundaries are inclusive. The reopening period 2020-04-27 to 2020-06-07
# is intentionally excluded and therefore remains covid_lockdown = 0.
covid_lockdown_periods <- data.frame(
  start = as.Date(c("2020-03-16", "2020-12-22")),
  end   = as.Date(c("2020-04-26", "2021-02-28"))
)

master_with_weather <- master_with_weather %>%
  mutate(
    covid_lockdown = as.integer(
      (date_utc >= covid_lockdown_periods$start[1] &
         date_utc <= covid_lockdown_periods$end[1]) |
        (date_utc >= covid_lockdown_periods$start[2] &
           date_utc <= covid_lockdown_periods$end[2])
    )
  ) %>%
  relocate(covid_lockdown, .after = last_col())

message(
  "COVID-lockdown flag added: ",
  sum(master_with_weather$covid_lockdown == 1L, na.rm = TRUE),
  " master row(s) across ",
  dplyr::n_distinct(master_with_weather$date_utc[master_with_weather$covid_lockdown == 1L]),
  " observed date(s)."
)


# ================== 4c) Add federal holidays ==================

# --- Make sure the format in the csv is consitent ---
normalize_ddmm <- function(x_chr) {
  x_chr <- trimws(as.character(x_chr))
  x_chr <- gsub("\\s+", "", x_chr)      # remove any stray spaces
  parts <- strsplit(x_chr, "\\.")
  oklen <- lengths(parts) == 2
  parts <- parts[oklen]
  if (!length(parts)) return(character(0))
  d <- suppressWarnings(as.integer(vapply(parts, `[[`, character(1), 1)))
  m <- suppressWarnings(as.integer(vapply(parts, `[[`, character(1), 2)))
  ok <- is.finite(d) & is.finite(m) & d >= 1 & d <= 31 & m >= 1 & m <= 12
  if (!any(ok)) return(character(0))
  unique(sprintf("%02d.%02d", d[ok], m[ok]))
}

# --- Make sure required column exists and add binary column ---
if (file.exists(federal_holidays_csv)) {
  tbl <- readr::read_csv(federal_holidays_csv, show_col_types = FALSE)
  if (!listed_col_name %in% names(tbl)) {
    warning("CSV '", federal_holidays_csv, "' lacks required column '", listed_col_name,
            "'. Setting federal_holiday = 0.")
    master_with_weather$federal_holiday <- 0L
  } else {
    listed_dm <- normalize_ddmm(tbl[[listed_col_name]])
    # Derive calendar variables in UTC to match time-based joins.
    dm_rows <- format(master_with_weather$date_utc, "%d.%m")
    master_with_weather$federal_holiday <- as.integer(dm_rows %in% listed_dm)
  }
} else {
  warning("CSV not found at '", federal_holidays_csv, "'. Setting federal_holiday = 0.")
  master_with_weather$federal_holiday <- 0L
}

# (optional) move flag to the end
master_with_weather <- master_with_weather %>%
  relocate(federal_holiday, .after = last_col())

# ================== 4d) Add absolute fuel counts by date (CSV: dd.mm.yyyy) ==================
# Use absolute national vehicle counts rather than shares.
# EV combines hybrid, plug-in hybrid and fully electric vehicles:
#   EV = HEV + PHEV + BEV

fuel_source_cols <- c("Gas", "Diesel", "HEV", "PHEV", "BEV")
fuel_cols        <- c("Gas", "Diesel", "EV")

# Remove obsolete share predictors if this section is re-run in an existing session.
obsolete_fuel_share_cols <- c(
  "Gas_share", "Diesel_share", "HEV_share", "PHEV_share", "BEV_share"
)

master_with_weather <- master_with_weather %>%
  dplyr::select(-dplyr::any_of(obsolete_fuel_share_cols))

# Default to NA columns. This is safe if the file is missing or parsing fails.
for (cc in fuel_cols) {
  master_with_weather[[cc]] <- NA_real_
}

if (file.exists(fuel_shares_csv)) {
  
  fuel_tbl <- readr::read_csv(
    fuel_shares_csv,
    show_col_types = FALSE
  )
  
  if (!"date" %in% names(fuel_tbl)) {
    
    warning(
      "Fuel CSV lacks required column 'date'. ",
      "Leaving Gas, Diesel and EV as NA."
    )
    
  } else {
    
    miss_fuel <- setdiff(
      fuel_source_cols,
      names(fuel_tbl)
    )
    
    if (length(miss_fuel)) {
      
      warning(
        "Fuel CSV missing required absolute-count columns: ",
        paste(miss_fuel, collapse = ", "),
        ". Missing values will remain NA."
      )
      
      for (cc in miss_fuel) {
        fuel_tbl[[cc]] <- NA_real_
      }
    }
    
    fuel_tbl <- fuel_tbl %>%
      mutate(
        date_utc = suppressWarnings(
          as.Date(.data$date, format = "%d.%m.%Y")
        ),
        
        # Interpolated national vehicle counts:
        # convert fractional estimates to whole vehicles.
        Gas    = as.integer(floor(as.numeric(Gas)    + 0.5)),
        Diesel = as.integer(floor(as.numeric(Diesel) + 0.5)),
        
        # Sum the interpolated electric categories first, then round the
        # combined total. This avoids accumulating rounding errors.
        EV = as.integer(
          floor(
            as.numeric(HEV) +
              as.numeric(PHEV) +
              as.numeric(BEV) +
              0.5
          )
        )
      ) %>%
      filter(!is.na(date_utc)) %>%
      select(date_utc, Gas, Diesel, EV) %>%
      distinct(date_utc, .keep_all = TRUE)
    
    if (!nrow(fuel_tbl)) {
      
      warning(
        "Fuel CSV parsed, but no valid rows remained after date parsing. ",
        "Leaving Gas, Diesel and EV as NA."
      )
      
    } else {
      
      # Join by date only: national counts apply to all grids on that day.
      master_with_weather <- master_with_weather %>%
        left_join(
          fuel_tbl,
          by = "date_utc",
          suffix = c("", ".csv")
        )
      
      # Prefer values freshly read from the CSV if this section is re-run.
      for (cc in fuel_cols) {
        
        cc_csv <- paste0(
          cc,
          ".csv"
        )
        
        if (cc_csv %in% names(master_with_weather)) {
          master_with_weather[[cc]] <- master_with_weather[[cc_csv]]
          master_with_weather[[cc_csv]] <- NULL
        }
      }
      
      message(
        "Absolute fuel counts joined by date. Non-NA counts: ",
        paste(
          sprintf(
            "%s=%d",
            fuel_cols,
            vapply(
              fuel_cols,
              function(x) {
                sum(
                  !is.na(
                    master_with_weather[[x]]
                  )
                )
              },
              integer(1)
            )
          ),
          collapse = ", "
        )
      )
    }
  }
  
} else {
  
  warning(
    "Fuel CSV not found at '",
    fuel_shares_csv,
    "'. Leaving Gas, Diesel and EV as NA."
  )
}

master_with_weather <- master_with_weather %>%
  relocate(
    any_of(fuel_cols),
    .after = last_col()
  )


# ============================================================
# ADD CALENDAR YEAR
# ============================================================

if (!"date_utc" %in% names(master_with_weather)) {
  stop("Cannot create year: date_utc is missing from master_with_weather.")
}

master_with_weather <- master_with_weather %>%
  dplyr::mutate(
    year = as.integer(format(as.Date(date_utc), "%Y"))
  )

if (anyNA(master_with_weather$year)) {
  warning(
    "Some year values are NA because date_utc could not be parsed. ",
    "Number of missing years: ",
    sum(is.na(master_with_weather$year))
  )
}

message(
  "Added year predictor. Range: ",
  min(master_with_weather$year, na.rm = TRUE),
  "–",
  max(master_with_weather$year, na.rm = TRUE)
)

# ===============================================
#
#   5. Point extract spatial data from premade rasters
#
# ===============================================

stopifnot(exists("master_with_weather"))
master <- as.data.table(master_with_weather)   # work in-memory
stopifnot(all(c("grid_id","centroid_x","centroid_y") %in% names(master)))

# ================== 5a) Column check defining helper functions ==================

# -------------------- Unique keys for sampling --------------------
dt_keys <- unique(master[, .(grid_id, centroid_x, centroid_y)])
dt_keys[, grid_id := as.character(grid_id)]
dt_keys <- dt_keys[is.finite(centroid_x) & is.finite(centroid_y)]
if (!nrow(dt_keys)) stop("No valid centroids found in master_with_weather.")

# -------------------- helper functions --------------------
# Turn centroid table into vector points and (if needed) reproject to the raster CRS
make_points_for_raster <- function(df_xy, r) {
  stopifnot(all(c("centroid_x","centroid_y") %in% names(df_xy)))
  v <- terra::vect(df_xy[, .(centroid_x, centroid_y)],
                   geom = c("centroid_x","centroid_y"),
                   crs  = "EPSG:2056")  # your centroids are LV95
  cr_r <- terra::crs(r, proj = TRUE)
  if (is.na(cr_r)) stop("Raster has no CRS.")
  if (!identical(terra::crs(v, proj = TRUE), cr_r)) {
    v <- terra::project(v, cr_r)  # <-- real reprojection (not assign)
  }
  v
}

# Project any input raster to EPSG:2056 (LV95) if it isn't already
ensure_lv95_raster <- function(r, method = "bilinear") {
  cr <- terra::crs(r, proj = TRUE)
  if (is.na(cr)) stop("Raster has no CRS.")
  if (cr != "EPSG:2056") {
    message("Reprojecting raster to EPSG:2056 …")
    r <- terra::project(r, "EPSG:2056", method = method)
  }
  r
}


# Generic point extraction function
extract_points <- function(r, vpts, method = "simple") { # <- extract_points function for our TIFs
  ex <- terra::extract(r, vpts, method = method, ID = FALSE)
  if (is.null(names(ex))) names(ex) <- names(r)
  ex
}

# Utility for setting multiple columns at once
assign_cols <- function(df, named_list) {
  for (nm in names(named_list)) df[[nm]] <- named_list[[nm]]
  df
}

# ================== 5b) Do point extraction on relevant rasters ==================

# -------------------- POPULATION (per radius, 1-band) --------------------
for (rad in names(pop_tifs)) {
  tif <- pop_tifs[[rad]]; if (!file.exists(tif)) stop("Population tif missing: ", tif)
  r <- terra::rast(tif); v <- make_points_for_raster(dt_keys, r) # r reads raster, v makes our points into a raster and ex does point extraction
  ex <- extract_points(r, v, method = "simple")
  dt_keys[[paste0("pop_buf", rad, "m")]] <- as.numeric(ex[[1]])
}

# -------------------- NDVI (per radius, 1-band) ---------------------------
for (rad in names(ndvi_tifs)) {
  tif <- ndvi_tifs[[rad]]; if (!file.exists(tif)) stop("NDVI tif missing: ", tif)
  r <- terra::rast(tif); v <- make_points_for_raster(dt_keys, r) # r reads raster, v makes our points into a raster and ex does point extraction
  ex <- extract_points(r, v, method = "simple")
  dt_keys[[paste0("NDVI_buf", rad, "m")]] <- as.numeric(ex[[1]])
}

# -------------------- LANDCOVER FRACTIONS: standard categories ----------
# Standard LC files contain all reclassified LC bands, but ports and airports
# are deliberately skipped here. They are extracted separately from dedicated
# larger-buffer rasters.
stopifnot(length(lc_band_names) >= 1L)

stopifnot(
  identical(
    lc_band_names[lc_standard_band_idx],
    lc_band_names[!(lc_band_names %in% lc_special_large_buffer_classes)]
  )
)

for (rad in names(lc_tifs)) {
  
  tif <- lc_tifs[[rad]]
  
  if (!file.exists(tif)) {
    stop("Landcover tif missing: ", tif)
  }
  
  r <- terra::rast(tif)
  
  expected_lc_bands <- length(lc_band_names)
  
  if (terra::nlyr(r) != expected_lc_bands) {
    stop(
      "Expected ", expected_lc_bands,
      " bands in standard LC raster, but found ",
      terra::nlyr(r),
      ": ",
      tif
    )
  }
  
  if (
    is.null(names(r)) ||
    anyNA(names(r)) ||
    length(unique(names(r))) != expected_lc_bands
  ) {
    names(r) <- paste0("lc_band", seq_len(expected_lc_bands))
  }
  
  v  <- make_points_for_raster(dt_keys, r)
  ex <- extract_points(r, v, method = "simple")
  
  for (i in lc_standard_band_idx) {
    dt_keys[[sprintf("%s_frac_r%s", lc_band_names[i], rad)]] <- as.numeric(ex[[i]])
  }
}

# -------------------- PORT LANDCOVER FRACTIONS: dedicated buffers ----------
# Each file must be a 1-band raster containing the port fraction (0..1).
for (rad in names(port_lc_tifs)) {
  
  tif <- port_lc_tifs[[rad]]
  
  if (!file.exists(tif)) {
    stop("Dedicated port landcover tif missing: ", tif)
  }
  
  r <- terra::rast(tif)
  
  if (terra::nlyr(r) != 1L) {
    stop("Expected 1 band in dedicated port landcover raster: ", tif)
  }
  
  v  <- make_points_for_raster(dt_keys, r)
  ex <- extract_points(r, v, method = "simple")
  
  dt_keys[[sprintf("ports_frac_r%s", rad)]] <- as.numeric(ex[[1]])
}

# -------------------- AIRPORT LANDCOVER FRACTIONS: dedicated buffers ----------
# Each file must be a 1-band raster containing the airport fraction (0..1).
for (rad in names(airport_lc_tifs)) {
  
  tif <- airport_lc_tifs[[rad]]
  
  if (!file.exists(tif)) {
    stop("Dedicated airport landcover tif missing: ", tif)
  }
  
  r <- terra::rast(tif)
  
  if (terra::nlyr(r) != 1L) {
    stop("Expected 1 band in dedicated airport landcover raster: ", tif)
  }
  
  v  <- make_points_for_raster(dt_keys, r)
  ex <- extract_points(r, v, method = "simple")
  
  dt_keys[[sprintf("airports_frac_r%s", rad)]] <- as.numeric(ex[[1]])
}

# -------------------- HEATING FRACTIONS / CLASSES (per radius, 4 bands) ----------
for (rad in names(heating_tifs)) {
  tif <- heating_tifs[[rad]]
  if (!file.exists(tif)) stop("Heating tif missing: ", tif)
  
  r <- terra::rast(tif)
  if (terra::nlyr(r) != 4L) stop("Expected 4 bands in heating raster: ", tif)
  
  if (is.null(names(r)) || anyNA(names(r)) || length(unique(names(r))) != 4L) {
    names(r) <- paste0("heating_band", seq_len(4L))
  }
  
  v  <- make_points_for_raster(dt_keys, r)
  ex <- extract_points(r, v, method = "simple")
  
  stopifnot(length(heating_band_names) == 4L)
  
  for (i in seq_len(4L)) {
    dt_keys[[sprintf("%s_r%s", heating_band_names[i], rad)]] <- as.numeric(ex[[i]])
  }
}

# -------------------- ROADS (auto-detect types & buffers) ------------------
# New filename pattern:
#   NoTunnel_roads_<type>_den_km2_r<buffer>.tif
#
# Output column names remain unchanged:
#   roads_<type>_den_km2_r<buffer>

road_files <- list.files(
  roads_dir,
  pattern = "^NoTunnel_roads_.*_den_km2_r(25|50|100)\\.tif$",
  full.names = TRUE
)

if (!length(road_files)) {
  
  warning("No NoTunnel road density TIFFs found in: ", roads_dir)
  
} else {
  
  rx <- "^NoTunnel_roads_(.+?)_den_km2_r(\\d+)\\.tif$"
  
  road_meta <- data.frame(
    file   = road_files,
    fname  = basename(road_files),
    type   = sub(rx, "\\1", basename(road_files)),
    buffer = sub(rx, "\\2", basename(road_files)),
    stringsAsFactors = FALSE
  )
  
  for (k in seq_len(nrow(road_meta))) {
    
    tif <- road_meta$file[k]
    typ <- road_meta$type[k]
    buf <- road_meta$buffer[k]
    
    if (!file.exists(tif)) {
      warning("Road density tif missing: ", tif)
      next
    }
    
    r <- terra::rast(tif)
    v <- make_points_for_raster(dt_keys, r)
    ex <- extract_points(r, v, method = "simple")
    
    # Column name stays exactly as before
    colname <- sprintf(
      "roads_%s_den_km2_r%s",
      typ,
      buf
    )
    
    dt_keys[[colname]] <- as.numeric(ex[[1]])
  }
}

# -------------------- HIGHWAY BINARY FLAG ------------------
# Binary flag from the 25 m no-tunnel motorway density raster:
#   highway_binary = 1 if NoTunnel_roads_motorway_den_km2_r25.tif > 0
#   highway_binary = 0 otherwise

if (!file.exists(highway_flag_tif)) {
  
  warning(
    "Highway flag raster missing: ",
    highway_flag_tif,
    ". Setting ",
    highway_flag_col,
    " to NA."
  )
  
  dt_keys[[highway_flag_col]] <- NA_integer_
  
} else {
  
  r_highway <- terra::rast(highway_flag_tif)
  v_highway <- make_points_for_raster(dt_keys, r_highway)
  ex_highway <- extract_points(r_highway, v_highway, method = "simple")
  
  highway_val <- suppressWarnings(as.numeric(ex_highway[[1]]))
  
  dt_keys[[highway_flag_col]] <- dplyr::case_when(
    is.na(highway_val) ~ 0L,
    highway_val > 0    ~ 1L,
    TRUE               ~ 0L
  )
  
  message(
    highway_flag_col,
    " added from ",
    basename(highway_flag_tif),
    ": ",
    format(
      sum(dt_keys[[highway_flag_col]] == 1L, na.rm = TRUE),
      big.mark = ","
    ),
    " / ",
    format(nrow(dt_keys), big.mark = ","),
    " unique grid(s) flagged as highway."
  )
  
  rm(r_highway, v_highway, ex_highway, highway_val)
  gc(FALSE)
}

# -------------------- MUNICIPALITY TYPE (1..3) --------------------
if (!file.exists(municipality_type_tif)) stop("municipality_type tif missing: ", municipality_type_tif)

r_muni <- terra::rast(municipality_type_tif) |> ensure_lv95_raster("near")  # categorical -> nearest neighbor
v_muni <- make_points_for_raster(dt_keys, r_muni)

ex_muni <- extract_points(r_muni, v_muni, method = "simple")
dt_keys[, municipality_type := as.integer(ex_muni[[1]])]

# optional: enforce only 1..3, set everything else to NA
dt_keys[!(municipality_type %in% 1:3), municipality_type := NA_integer_]

message("municipality_type non-NA: ", sum(!is.na(dt_keys$municipality_type)), " / ", nrow(dt_keys))

# -------------------- SKY-VIEW FACTOR --------------------

if (!file.exists(svf_brut_tif)) {
  stop("Sky-view-factor TIF missing: ", svf_brut_tif)
}

r_svf <- terra::rast(svf_brut_tif)

if (terra::nlyr(r_svf) != 1L) {
  stop(
    "Sky-view-factor raster must contain exactly one layer. Found: ",
    terra::nlyr(r_svf)
  )
}

# Reproject the raster to LV95 when required.
# SVF is continuous, so bilinear resampling is appropriate if reprojection occurs.
r_svf <- ensure_lv95_raster(
  r_svf,
  method = "bilinear"
)

# Create centroid points in the raster CRS.
v_svf <- make_points_for_raster(
  dt_keys,
  r_svf
)

# Extract the raster value at every unique grid centroid.
ex_svf <- extract_points(
  r_svf,
  v_svf,
  method = "simple"
)

# Exact predictor name expected by the RF model.
dt_keys[, svf_brut := as.numeric(ex_svf[[1]])]

message(
  "svf_brut non-NA: ",
  sum(is.finite(dt_keys$svf_brut)),
  " / ",
  nrow(dt_keys)
)

message(
  "svf_brut range: ",
  signif(min(dt_keys$svf_brut, na.rm = TRUE), 6),
  " to ",
  signif(max(dt_keys$svf_brut, na.rm = TRUE), 6)
)

rm(r_svf, v_svf, ex_svf)
gc(FALSE)

# # -------------------- CTM --------------------
# if (!file.exists(ctm_tif)) stop("CTM tif missing: ", ctm_tif)
# r_ctm <- terra::rast(ctm_tif)
# 
# # Construct vector points using the raster CRS.
# v_ctm <- make_points_for_raster(dt_keys, r_ctm)
# 
# # simple (pixel-at-point) extraction; take first band
# ex_ctm <- extract_points(r_ctm, v_ctm, method = "simple")
# dt_keys[ , ctm_ufp_pnc := as.numeric(ex_ctm[[1]]) ]
# 
# # -------------------- NABEL (fixed sites) --------------------
# if (!file.exists(nabel_tif)) stop("NABEL tif missing: ", nabel_tif)
# r_nab <- terra::rast(nabel_tif)
# 
# v_nab <- make_points_for_raster(dt_keys, r_nab)
# ex_nab <- extract_points(r_nab, v_nab, method = "simple")
# dt_keys[ , nabel_ufp_pnc := as.numeric(ex_nab[[1]]) ]

# -------------------- DISTANCE TO CHIASSO --------------------
if (!file.exists(distance_to_chiasso_tif)) stop("Distance-to-Chiasso tif missing: ", distance_to_chiasso_tif)
r_chiasso <- terra::rast(distance_to_chiasso_tif) |> ensure_lv95_raster("bilinear")
v_chiasso <- make_points_for_raster(dt_keys, r_chiasso)
ex_chiasso <- extract_points(r_chiasso, v_chiasso, method = "simple")
dt_keys[ , distance_to_chiasso := as.numeric(ex_chiasso[[1]]) ]
message("distance_to_chiasso non-NA: ", sum(is.finite(dt_keys$distance_to_chiasso)), " / ", nrow(dt_keys))

# -------------------- DISTANCE TO MAJOR ROAD --------------------
if (!file.exists(distance_to_major_road_tif)) stop("Distance-to-major-road tif missing: ", distance_to_major_road_tif)
r_major <- terra::rast(distance_to_major_road_tif) |> ensure_lv95_raster("bilinear")
v_major <- make_points_for_raster(dt_keys, r_major)
ex_major <- extract_points(r_major, v_major, method = "simple")
dt_keys[ , distance_to_major_road := as.numeric(ex_major[[1]]) ]
message("distance_to_major_road non-NA: ", sum(is.finite(dt_keys$distance_to_major_road)), " / ", nrow(dt_keys))

# -------------------- CAR METERS GAUSS --------------------
if (!file.exists(car_meters_gauss_tif)) stop("Car-meters-Gauss tif missing: ", car_meters_gauss_tif)
r_car <- terra::rast(car_meters_gauss_tif) |> ensure_lv95_raster("bilinear")
v_car <- make_points_for_raster(dt_keys, r_car)
ex_car <- extract_points(r_car, v_car, method = "simple")
dt_keys[ , car_meters_gauss := as.numeric(ex_car[[1]]) ]
message("car_meters_gauss non-NA: ", sum(is.finite(dt_keys$car_meters_gauss)), " / ", nrow(dt_keys))

# -------------------- WOOD BURNING --------------------
if (!file.exists(wood_burning_tif)) stop("Wood-burning tif missing: ", wood_burning_tif)
r_wood <- terra::rast(wood_burning_tif) |> ensure_lv95_raster("bilinear")
v_wood <- make_points_for_raster(dt_keys, r_wood)
ex_wood <- extract_points(r_wood, v_wood, method = "simple")
dt_keys[ , wood_burning := as.numeric(ex_wood[[1]]) ]
message("wood_burning non-NA: ", sum(is.finite(dt_keys$wood_burning)), " / ", nrow(dt_keys))

# # -------------------- Cyprus CTM --------------------
# if (!file.exists(cyprus_ctm_tif)) stop("Cyprus CTM tif missing: ", cyprus_ctm_tif)
# r_cyprus <- terra::rast(cyprus_ctm_tif) |> ensure_lv95_raster("bilinear")
# v_cyprus <- make_points_for_raster(dt_keys, r_cyprus)
# ex_cyprus <- extract_points(r_cyprus, v_cyprus, method = "simple")
# dt_keys[ , cyprus_ctm := as.numeric(ex_cyprus[[1]]) ]
# message("Cyprus CTM non-NA: ", sum(is.finite(dt_keys$cyprus_ctm)), " / ", nrow(dt_keys))


# (optional) quick sanity prints
cat("CTM extracted (non-NA): ", sum(is.finite(dt_keys$ctm_ufp_pnc)),  " / ", nrow(dt_keys), "\n")
cat("NABEL extracted (non-NA): ", sum(is.finite(dt_keys$nabel_ufp_pnc)), " / ", nrow(dt_keys), "\n")

# ================== 5c) Sanity check for raster joins ==================

# -------------------- SANITY CHECK SUMMARY --------------------
summarize_non_na <- function(cols) {
  cols <- intersect(cols, names(dt_keys))
  if (!length(cols)) return(NULL)
  data.frame(
    variable   = cols,
    n_non_na   = vapply(cols, function(cn) sum(!is.na(dt_keys[[cn]])), integer(1)),
    pct_non_na = vapply(cols, function(cn) mean(!is.na(dt_keys[[cn]]))*100, numeric(1)),
    stringsAsFactors = FALSE
  )
}
pop_cols  <- paste0("pop_buf",  names(pop_tifs),  "m")
ndvi_cols <- paste0("NDVI_buf", names(ndvi_tifs), "m")
standard_lc_cols <- as.vector(
  sapply(
    names(lc_tifs),
    function(rad) {
      sprintf(
        "%s_frac_r%s",
        lc_band_names[lc_standard_band_idx],
        rad
      )
    }
  )
)

port_lc_cols <- sprintf(
  "ports_frac_r%s",
  names(port_lc_tifs)
)

airport_lc_cols <- sprintf(
  "airports_frac_r%s",
  names(airport_lc_tifs)
)

lc_cols <- c(
  standard_lc_cols,
  airport_lc_cols
)
road_cols <- grep("^roads_.+_den_km2_r(25|50|100)$", names(dt_keys), value = TRUE)
heating_cols <- as.vector(
  sapply(names(heating_tifs), function(rad) sprintf("%s_r%s", heating_band_names, rad))
)
sanity_tab <- dplyr::bind_rows(
  list(
    Population = summarize_non_na(pop_cols),
    NDVI       = summarize_non_na(ndvi_cols),
    Landcover  = summarize_non_na(lc_cols),
    Heating    = summarize_non_na(heating_cols),
    Roads      = summarize_non_na(road_cols)
  ),
  .id = "group"
)
cat("\n=== Point-extraction sanity (non-NA counts) ===\n")
if (!is.null(sanity_tab) && nrow(sanity_tab)) {
  print(sanity_tab, row.names = FALSE)
  if (length(road_cols)) {
    buf_from_name <- function(x) sub("^.*_r(\\d+)$", "\\1", x)
    road_buf_summary <- sanity_tab %>%
      dplyr::filter(group == "Roads") %>%
      dplyr::mutate(buffer = buf_from_name(variable)) %>%
      dplyr::group_by(buffer) %>%
      dplyr::summarise(
        vars = dplyr::n(),
        total_non_na = sum(n_non_na),
        mean_pct_non_na = mean(pct_non_na),
        .groups = "drop"
      )
    cat("\n--- Roads per-buffer summary ---\n")
    print(road_buf_summary, row.names = FALSE)
  }
} else {
  cat("No variables summarized.\n")
}

# ================== 5d) Join new columns to master ==================

# Define add-on column groups
chem_cols <- c("ctm_ufp_pnc", "nabel_ufp_pnc")

# NEW: auxiliary/proximity/traffic columns extracted above
aux_cols  <- c("distance_to_chiasso",
               "distance_to_major_road",
               "car_meters_gauss",
               "wood_burning",
               "cyprus_ctm",
               "municipality_type")

# Columns that may be (re)created and should be dropped first on re-runs
drop_cols <- unique(c(pop_cols, ndvi_cols, lc_cols, heating_cols, road_cols, chem_cols, aux_cols))

# Drop any existing versions of those columns (safe for re-runs)
drop_cols <- intersect(drop_cols, names(master))
if (length(drop_cols)) master[, (drop_cols) := NULL]

# Prepare subset from dt_keys: grid_id + all add-on columns present
y_dt <- dt_keys[, 
                c("grid_id", setdiff(names(dt_keys), c("grid_id","centroid_x","centroid_y"))),
                with = FALSE
]

# Ensure matching types and join
master[, grid_id := as.character(grid_id)]
master <- dplyr::left_join(as.data.frame(master), as.data.frame(y_dt), by = "grid_id") %>%
  as.data.table()

# Quick feedback
n_pop  <- if (exists("pop_cols"))  length(pop_cols)  else 0L
n_ndvi <- if (exists("ndvi_cols")) length(ndvi_cols) else 0L
n_lc   <- if (exists("lc_cols"))   length(lc_cols)   else 0L
n_road <- if (exists("road_cols")) length(road_cols) else 0L
n_heat <- if (exists("heating_cols")) length(heating_cols) else 0L

cat("Joined back to master:\n")
cat("  - pop/NDVI/LC/roads cols:", n_pop + n_ndvi + n_lc + n_road, "\n")
cat("  - chem cols: ", paste(chem_cols, collapse = ", "), "\n", sep = "")
cat("  - aux  cols: ", paste(aux_cols,  collapse = ", "), "\n", sep = "")
cat("  - total rows:", nrow(master), "\n")


# ============================================================
# TRAFFIC buffered means — point extraction from prebuilt TIFFs
# Files: <road_type>_<traffic_type>_mean_r{250|500|1000}.tif
# ============================================================

road_types     <- c("motorway","primary","secondary","tertiary","residential")
traffic_types  <- c("asp","msp","dtv")
buffer_sizes   <- c(25L, 50L, 100L)
meas_xy        <- c("centroid_x","centroid_y")

# Points (LV95)
stopifnot(all(meas_xy %in% names(master)))
coords_df <- master[, ..meas_xy] |> as.data.frame()
vpts <- terra::vect(coords_df, geom = meas_xy, crs = "EPSG:2056")


# All expected raster names
grid <- expand.grid(road = road_types, traf = traffic_types, rad = buffer_sizes, 
                    stringsAsFactors = FALSE)
basenames <- sprintf("%s_%s_mean_r%d", grid$road, grid$traf, grid$rad)
files     <- file.path(traffic_root, paste0(basenames, ".tif"))

# Extract for each existing raster
for (k in seq_along(files)) {
  f  <- files[k]
  nm <- basenames[k]  # column name = tif basename
  if (!file.exists(f)) {
    warning("Missing traffic raster: ", basename(f))
    master[[nm]] <- NA_real_
    next
  }
  r <- terra::rast(f)
  vals <- as.numeric(terra::extract(r, vpts, ID = FALSE)[,1])
  master[[nm]] <- vals
  rm(r); gc()
}

message("Traffic point extraction complete. Added columns: ",
        paste(basenames, collapse = ", "))


# -------------------- WRITE MASTER --------------------
arrow::write_parquet(master, out_parquet)
message("\n✓ Wrote master with POP/NDVI/LC/ROADS and TRAFFIC buffers to: ", out_parquet)


# ============================================================
# 6) OPTIONAL: Aggregate finished grid-date master to 500 m road-segment-date master
# ============================================================
# This block keeps the full grid-date master intact and creates an additional
# segment-date master dataset for true segment-level RF modelling / CV.
# One output row = segment_id x date_utc.

create_segment_date_master_500m <- FALSE

segment_gpkg_500m <- file.path(input_root, "road_segments_500m.gpkg")
segment_layer_500m <- NA_character_  # NA = first layer in GPKG

# Existing grid-to-OSM-road assignment from the segment creation script.
# This is used only for optional QA; the actual grid -> segment assignment below
# is created by snapping grid centroids to the final 500 m segment geometries.
grid_to_segment_assignment_csv <- file.path(input_root, "grid_to_segment.csv")

segment_master_out_dir <- output_root
out_parquet_segment_500m <- file.path(segment_master_out_dir, "segment_daily_master.parquet")
out_grid_to_segment_assignment_500m <- file.path(segment_master_out_dir, "grid_to_500m_segment_assignment.csv")
out_segment_master_summary_500m <- file.path(segment_master_out_dir, "segment_daily_summary.csv")

# Maximum allowed distance from a measured grid centroid to the nearest final
# road segment. Because the segment network was built from measured roads, this
# should usually be small. If too many grids fail, try 100 m.
segment_snap_max_m <- 20
segment_snap_chunk_n <- 50000L

# Minimum data retained in the segment-date master.
min_unique_grids_per_segment_date <- 1L
min_raw_obs_per_segment_date <- 1L

if (isTRUE(create_segment_date_master_500m)) {
  
  dir.create(segment_master_out_dir, recursive = TRUE, showWarnings = FALSE)
  
  message("\n============================================================")
  message("Creating 500 m road-segment-date master dataset")
  message("============================================================")
  
  if (!exists("master")) {
    stop("Object 'master' not found. Run the grid-date master creation first.")
  }
  
  master <- as.data.table(master)
  
  required_master_cols <- c("grid_id", "centroid_x", "centroid_y", "date_utc")
  missing_master_cols <- setdiff(required_master_cols, names(master))
  if (length(missing_master_cols)) {
    stop("Master is missing required columns for segment aggregation: ", paste(missing_master_cols, collapse = ", "))
  }
  
  master[, grid_id := as.character(grid_id)]
  master[, date_utc := as.Date(date_utc)]
  
  # -------------------- Read final 500 m segment geometries as metadata only --------------------
  # We still read the GPKG, but only to attach segment metadata:
  # segment_x, segment_y, road_group, length_class, segment_length_m.
  # We do NOT snap grids here anymore; grid_id -> segment_id is read from CSV.
  
  if (!file.exists(segment_gpkg_500m)) {
    stop("500 m segment GPKG does not exist:\n", segment_gpkg_500m)
  }
  
  if (is.na(segment_layer_500m) || !nzchar(segment_layer_500m)) {
    seg_layers <- sf::st_layers(segment_gpkg_500m)$name
    if (!length(seg_layers)) stop("No layers found in segment GPKG: ", segment_gpkg_500m)
    segment_layer_500m <- seg_layers[1]
    message("segment_layer_500m is NA; using first layer: ", segment_layer_500m)
  }
  
  segments_sf <- sf::st_read(
    segment_gpkg_500m,
    layer = segment_layer_500m,
    quiet = TRUE
  )
  
  segments_sf <- sf::st_transform(segments_sf, 2056)
  
  if (!"segment_id" %in% names(segments_sf)) {
    stop("Segment GPKG must contain column 'segment_id'.")
  }
  
  segments_sf$segment_id <- as.character(segments_sf$segment_id)
  
  if (!"road_group" %in% names(segments_sf)) {
    segments_sf$road_group <- NA_character_
  }
  
  if (!"segment_length_m" %in% names(segments_sf)) {
    segments_sf$segment_length_m <- as.numeric(sf::st_length(segments_sf))
  }
  
  if (!"length_class" %in% names(segments_sf)) {
    segments_sf$length_class <- dplyr::case_when(
      segments_sf$segment_length_m < 250 ~ "<250 m",
      segments_sf$segment_length_m < 400 ~ "250-400 m",
      segments_sf$segment_length_m < 500 ~ "400-500 m",
      segments_sf$segment_length_m < 650 ~ "500-650 m",
      segments_sf$segment_length_m < 1000 ~ "650-1000 m",
      TRUE ~ ">=1000 m"
    )
  }
  
  message("Read 500 m road segments: ", format(nrow(segments_sf), big.mark = ","))
  
  # Segment representative coordinates for modelling folds / maps.
  # st_point_on_surface is safer than st_centroid for multiline geometries.
  seg_points <- suppressWarnings(sf::st_point_on_surface(segments_sf))
  seg_xy <- sf::st_coordinates(seg_points)
  
  segment_meta_dt <- as.data.table(sf::st_drop_geometry(segments_sf))
  
  segment_meta_dt[, segment_id := as.character(segment_id)]
  segment_meta_dt[, segment_x := as.numeric(seg_xy[, "X"])]
  segment_meta_dt[, segment_y := as.numeric(seg_xy[, "Y"])]
  segment_meta_dt[, segment_length_m := as.numeric(segment_length_m)]
  segment_meta_dt[, road_group := as.character(road_group)]
  segment_meta_dt[, length_class := as.character(length_class)]
  
  segment_meta_dt <- segment_meta_dt[
    ,
    .(
      segment_id,
      road_group,
      length_class,
      segment_length_m,
      segment_x,
      segment_y
    )
  ]
  
  rm(segments_sf, seg_points, seg_xy)
  gc(FALSE)
  
  # -------------------- Read existing grid-to-segment assignment CSV --------------------
  # This CSV was created by the validation script and already contains the final
  # grid_id -> segment_id assignment. Therefore no snapping is done here.
  
  if (!file.exists(grid_to_segment_assignment_csv)) {
    stop(
      "grid_to_segment_assignment_csv does not exist:\n",
      grid_to_segment_assignment_csv
    )
  }
  
  message("\nReading existing grid-to-500m-segment assignment:")
  message(grid_to_segment_assignment_csv)
  
  grid_to_segment_dt <- data.table::fread(
    grid_to_segment_assignment_csv,
    nThread = max(1L, parallel::detectCores() - 1L)
  )
  
  required_assignment_cols <- c("grid_id", "segment_id")
  missing_assignment_cols <- setdiff(required_assignment_cols, names(grid_to_segment_dt))
  
  if (length(missing_assignment_cols)) {
    stop(
      "Grid-to-segment assignment CSV is missing required column(s): ",
      paste(missing_assignment_cols, collapse = ", ")
    )
  }
  
  grid_to_segment_dt[, grid_id := as.character(grid_id)]
  grid_to_segment_dt[, segment_id := as.character(segment_id)]
  
  # If the validation assignment contains snapped_to_segment, respect it.
  # If not, assume rows in the CSV are valid assignments.
  if ("snapped_to_segment" %in% names(grid_to_segment_dt)) {
    
    # fread may read logicals as TRUE/FALSE, 1/0, or character depending on file.
    if (is.character(grid_to_segment_dt$snapped_to_segment)) {
      grid_to_segment_dt[
        ,
        snapped_to_segment := tolower(snapped_to_segment) %in% c("true", "t", "1", "yes", "y")
      ]
    } else {
      grid_to_segment_dt[
        ,
        snapped_to_segment := as.logical(snapped_to_segment)
      ]
    }
    
  } else {
    
    grid_to_segment_dt[, snapped_to_segment := TRUE]
  }
  
  # Optional extra safety: if snap distance exists, require it to be finite.
  # The actual thresholding was already done in the validation script.
  if ("snap_dist_to_segment_m" %in% names(grid_to_segment_dt)) {
    grid_to_segment_dt[, snap_dist_to_segment_m := as.numeric(snap_dist_to_segment_m)]
  } else {
    grid_to_segment_dt[, snap_dist_to_segment_m := NA_real_]
  }
  
  grid_to_segment_dt <- grid_to_segment_dt[
    snapped_to_segment == TRUE &
      !is.na(grid_id) &
      !is.na(segment_id)
  ]
  
  if (!nrow(grid_to_segment_dt)) {
    stop("No valid snapped grid-to-segment assignments remained after filtering.")
  }
  
  # Keep one assignment per grid_id.
  # If duplicates exist, keep the closest one if snap distance is available.
  setorder(grid_to_segment_dt, grid_id, snap_dist_to_segment_m)
  
  grid_to_segment_dt <- grid_to_segment_dt[
    ,
    .SD[1],
    by = grid_id
  ]
  
  message("Valid assigned grids from CSV: ", format(nrow(grid_to_segment_dt), big.mark = ","))
  message("Unique assigned segments from CSV: ", format(uniqueN(grid_to_segment_dt$segment_id), big.mark = ","))
  
  # Attach authoritative segment metadata from the GPKG.
  grid_to_segment_dt <- merge(
    grid_to_segment_dt,
    segment_meta_dt,
    by = "segment_id",
    all.x = TRUE,
    sort = FALSE,
    suffixes = c("", ".gpkg")
  )
  
  # If the CSV already had some of these metadata columns, prefer the GPKG version
  # where available because it is directly tied to the final segment layer.
  for (cc in c("road_group", "length_class", "segment_length_m", "segment_x", "segment_y")) {
    
    cc_gpkg <- paste0(cc, ".gpkg")
    
    if (cc_gpkg %in% names(grid_to_segment_dt)) {
      
      if (cc %in% names(grid_to_segment_dt)) {
        grid_to_segment_dt[
          !is.na(get(cc_gpkg)),
          (cc) := get(cc_gpkg)
        ]
        grid_to_segment_dt[, (cc_gpkg) := NULL]
      } else {
        data.table::setnames(grid_to_segment_dt, cc_gpkg, cc)
      }
    }
  }
  
  # If length_class was missing in both places, recreate it.
  if (!"length_class" %in% names(grid_to_segment_dt)) {
    grid_to_segment_dt[
      ,
      length_class := dplyr::case_when(
        segment_length_m < 250 ~ "<250 m",
        segment_length_m < 400 ~ "250-400 m",
        segment_length_m < 500 ~ "400-500 m",
        segment_length_m < 650 ~ "500-650 m",
        segment_length_m < 1000 ~ "650-1000 m",
        TRUE ~ ">=1000 m"
      )
    ]
  }
  
  # Final sanity checks.
  if (anyNA(grid_to_segment_dt$segment_x) || anyNA(grid_to_segment_dt$segment_y)) {
    warning(
      "Some assigned segments have missing segment_x/segment_y after joining GPKG metadata. ",
      "K-means folds may need these rows removed later."
    )
  }
  
  message("\nGrid-to-segment assignment by road group:")
  print(
    grid_to_segment_dt[
      ,
      .(
        n_grids = .N,
        n_segments = uniqueN(segment_id),
        median_snap_dist_m = median(snap_dist_to_segment_m, na.rm = TRUE),
        median_segment_length_m = median(segment_length_m, na.rm = TRUE)
      ),
      by = road_group
    ][order(road_group)]
  )
  
  data.table::fwrite(grid_to_segment_dt, out_grid_to_segment_assignment_500m)
  message("Saved cleaned/enriched grid-to-500m-segment assignment:")
  message(out_grid_to_segment_assignment_500m)
  
  # -------------------- Join accepted segment IDs back to the grid-date master --------------------
  grid_to_segment_keep <- grid_to_segment_dt[
    ,
    .(
      grid_id,
      segment_id,
      road_group,
      length_class,
      segment_length_m,
      segment_x,
      segment_y,
      snap_dist_to_segment_m
    )
  ]
  
  master_seg <- merge(
    master,
    grid_to_segment_keep,
    by = "grid_id",
    all.x = FALSE,
    all.y = FALSE,
    sort = FALSE
  )
  
  message("Grid-date master rows before segment join: ", format(nrow(master), big.mark = ","))
  message("Grid-date rows after segment join: ", format(nrow(master_seg), big.mark = ","))
  message("Unique retained segments: ", format(uniqueN(master_seg$segment_id), big.mark = ","))
  
  if (!nrow(master_seg)) {
    stop("No master rows remained after joining grid-to-segment assignment.")
  }
  
  # -------------------- Aggregation helpers --------------------
  safe_mean <- function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) return(NA_real_)
    mean(x)
  }
  
  safe_max <- function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) return(NA_real_)
    max(x)
  }
  
  first_mode_character <- function(x) {
    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(x)]
    if (!length(x)) return(NA_character_)
    names(sort(table(x), decreasing = TRUE))[1]
  }
  
  first_mode_numeric <- function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) return(NA_real_)
    as.numeric(names(sort(table(x), decreasing = TRUE))[1])
  }
  
  # Columns that should not be averaged as predictors.
  non_predictor_numeric_cols <- c(
    "centroid_x", "centroid_y",
    "segment_x", "segment_y",
    "segment_length_m",
    "snap_dist_to_segment_m",
    "row_index", "col_index",
    "n_obs", "n_obs_grid_total", "n_days_grid_total",
    "NABEL_site", "municipality_type",
    "covid_lockdown", "federal_holiday"
  )
  
  numeric_cols <- names(master_seg)[vapply(master_seg, is.numeric, logical(1))]
  mean_cols <- setdiff(numeric_cols, non_predictor_numeric_cols)
  
  binary_max_cols <- intersect(c("covid_lockdown", "federal_holiday", "NABEL_site"), names(master_seg))
  
  message("Numeric columns aggregated by mean: ", length(mean_cols))
  message("Binary/status columns aggregated by max: ", paste(binary_max_cols, collapse = ", "))
  
  # Base metadata / QA counts per segment-date.
  segment_base_dt <- master_seg[
    ,
    .(
      segment_x = segment_x[1],
      segment_y = segment_y[1],
      road_group = road_group[1],
      length_class = length_class[1],
      segment_length_m = segment_length_m[1],
      n_grid_rows_segment_date = .N,
      n_unique_grids_segment_date = uniqueN(grid_id),
      n_raw_obs_segment_date = if ("n_obs" %in% names(master_seg)) sum(n_obs, na.rm = TRUE) else .N,
      mean_snap_dist_to_segment_m = safe_mean(snap_dist_to_segment_m),
      max_snap_dist_to_segment_m = safe_max(snap_dist_to_segment_m),
      canton_name = if ("canton_name" %in% names(master_seg)) first_mode_character(canton_name) else NA_character_,
      municipality_type = if ("municipality_type" %in% names(master_seg)) first_mode_numeric(municipality_type) else NA_real_
    ),
    by = .(segment_id, date_utc)
  ]
  
  # Mean-aggregate continuous pollutants and predictors.
  agg_parts <- list(segment_base_dt)
  
  if (length(mean_cols)) {
    segment_mean_dt <- master_seg[
      ,
      lapply(.SD, safe_mean),
      by = .(segment_id, date_utc),
      .SDcols = mean_cols
    ]
    agg_parts[[length(agg_parts) + 1L]] <- segment_mean_dt
  }
  
  if (length(binary_max_cols)) {
    segment_binary_dt <- master_seg[
      ,
      lapply(.SD, safe_max),
      by = .(segment_id, date_utc),
      .SDcols = binary_max_cols
    ]
    agg_parts[[length(agg_parts) + 1L]] <- segment_binary_dt
  }
  
  segment_master <- Reduce(
    function(x, y) merge(x, y, by = c("segment_id", "date_utc"), all = TRUE, sort = FALSE),
    agg_parts
  )
  
  # Apply minimum data filters.
  n_before_filter <- nrow(segment_master)
  segment_master <- segment_master[
    n_unique_grids_segment_date >= min_unique_grids_per_segment_date &
      n_raw_obs_segment_date >= min_raw_obs_per_segment_date
  ]
  
  message("Segment-date rows before minimum filters: ", format(n_before_filter, big.mark = ","))
  message("Segment-date rows after minimum filters: ", format(nrow(segment_master), big.mark = ","))
  message("Unique segments in segment-date master: ", format(uniqueN(segment_master$segment_id), big.mark = ","))
  message("Date range: ", paste(range(segment_master$date_utc, na.rm = TRUE), collapse = " to "))
  
  # Put key columns first.
  key_first <- c(
    "segment_id", "date_utc",
    "segment_x", "segment_y",
    "road_group", "length_class", "segment_length_m",
    "canton_name", "municipality_type",
    "n_grid_rows_segment_date", "n_unique_grids_segment_date", "n_raw_obs_segment_date",
    "mean_snap_dist_to_segment_m", "max_snap_dist_to_segment_m"
  )
  
  setcolorder(segment_master, c(intersect(key_first, names(segment_master)), setdiff(names(segment_master), key_first)))
  
  # -------------------- Save segment-date master and summary --------------------
  arrow::write_parquet(segment_master, out_parquet_segment_500m, compression = "zstd")
  message("\n✓ Wrote 500 m segment-date master dataset to:")
  message(out_parquet_segment_500m)
  
  segment_summary <- segment_master[
    ,
    .(
      n_segment_dates = .N,
      n_unique_dates = uniqueN(date_utc),
      first_date = min(date_utc, na.rm = TRUE),
      last_date = max(date_utc, na.rm = TRUE),
      mean_UFP = if ("UFP" %in% names(segment_master)) mean(UFP, na.rm = TRUE) else NA_real_,
      median_UFP = if ("UFP" %in% names(segment_master)) median(UFP, na.rm = TRUE) else NA_real_,
      mean_n_unique_grids_segment_date = mean(n_unique_grids_segment_date, na.rm = TRUE),
      total_raw_obs = sum(n_raw_obs_segment_date, na.rm = TRUE),
      road_group = road_group[1],
      length_class = length_class[1],
      segment_length_m = segment_length_m[1],
      segment_x = segment_x[1],
      segment_y = segment_y[1]
    ),
    by = segment_id
  ]
  
  data.table::fwrite(segment_summary, out_segment_master_summary_500m)
  message("✓ Wrote 500 m segment master summary to:")
  message(out_segment_master_summary_500m)
  
  message("\nSegment-date rows by road group:")
  print(
    segment_master[
      ,
      .(
        n_segment_dates = .N,
        n_segments = uniqueN(segment_id),
        mean_UFP = if ("UFP" %in% names(segment_master)) mean(UFP, na.rm = TRUE) else NA_real_,
        mean_unique_grids_per_segment_date = mean(n_unique_grids_segment_date, na.rm = TRUE)
      ),
      by = road_group
    ][order(road_group)]
  )
  
  message("\n============================================================")
  message("500 m segment-date master creation complete")
  message("============================================================")
}
