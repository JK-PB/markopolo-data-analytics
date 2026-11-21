# MARKOPOLO portfolio demonstration
# Synthetic-data figures ONLY, not research results.

write_demo_figures <- function(predictions, metrics, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  method_order <- metrics$method
  labels <- c("Random 80/20", "Spatial k-means", "Later dates", "Held-out sites")

  png(file.path(output_dir, "validation_rmse_SYNTHETIC.png"),
      width = 1700, height = 1150, res = 220)
  old <- par(no.readonly = TRUE)
  par(mar = c(8, 5, 4, 1), family = "sans", fg = "black", col.axis = "black")
  barplot(metrics$rmse, names.arg = labels, las = 2,
          col = "#527A99", border = NA, cex.names = 0.85,
          ylab = "RMSE (synthetic UFP units)",
          main = "Validation error by evaluation design (synthetic data)")
  mtext("Different holdouts test different generalisation questions.",
        side = 1, line = 6, cex = 0.75)
  par(old)
  dev.off()

  for (method in method_order) {
    sub <- predictions[predictions$method == method, , drop = FALSE]
    suffix <- switch(method,
      "Random 80/20" = "random",
      "Spatial 5-fold k-means" = "spatial",
      "Future-date holdout" = "temporal",
      "Leave-one-site-out demo" = "site",
      "other"
    )
    png(file.path(output_dir, paste0("observed_vs_predicted_", suffix, "_SYNTHETIC.png")),
        width = 1500, height = 1300, res = 200)
    old <- par(no.readonly = TRUE)
    par(mar = c(5, 5, 4, 2), family = "sans", fg = "black", col.axis = "black")
    lim <- range(c(sub$observed_ufp, sub$predicted_ufp), finite = TRUE)
    plot(sub$observed_ufp, sub$predicted_ufp,
         xlab = "Observed UFP (synthetic units)",
         ylab = "Predicted UFP (synthetic units)",
         main = paste0("Observed vs. predicted - ", method, " (synthetic)"),
         pch = 16, cex = 0.6,
         col = grDevices::adjustcolor("#295F84", alpha.f = 0.23),
         xlim = lim, ylim = lim)
    abline(0, 1, lwd = 2, lty = 2, col = "black")
    row <- metrics[metrics$method == method, , drop = FALSE]
    legend("topleft", bty = "n", cex = 0.85,
           legend = sprintf("Predictive R² = %.3f | RMSE = %.0f | n = %d",
                            row$r_squared, row$rmse, row$n_holdout))
    par(old)
    dev.off()
  }
  invisible(NULL)
}
