#!/usr/bin/env Rscript
# Run from repository root: Rscript run_demo.R
# Requires install.packages("ranger")

if (!file.exists("R/01_synthetic_data.R")) {
  stop("Please run this script from the repository root directory")
}
if (!requireNamespace("ranger", quietly = TRUE)) {
  stop("Package 'ranger' is missing. Run install.packages('ranger') first.")
}

source("R/01_synthetic_data.R", local = FALSE)
source("R/02_model_validation.R", local = FALSE)
source("R/03_figures.R", local = FALSE)

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
observations <- make_synthetic_ufp()
results <- run_validation_suite(observations)

write.csv(results$metrics, "outputs/validation_metrics_SYNTHETIC.csv", row.names = FALSE)
write.csv(results$predictions, "outputs/heldout_predictions_SYNTHETIC.csv", row.names = FALSE)
write_demo_figures(results$predictions, results$metrics, "outputs")

writeLines(c(
  "SYNTHETIC demonstration only: not original MARKOPOLO data or results.",
  "", capture.output(sessionInfo())
), "outputs/session_information.txt")

cat("\n--- SYNTHETIC demonstration: evaluation table ---\n")
print(results$metrics, row.names = FALSE)
cat("\nSynthetic outputs written to outputs/\n")
