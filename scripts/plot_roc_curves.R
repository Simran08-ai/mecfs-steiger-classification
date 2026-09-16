# ============================================================
# Real ROC Curves from LOOCV Predicted Probabilities
# Run from: D:/New folder (4)/MECFS Steiger/MECFS_Final
# ============================================================

library(pROC)

# ── Load prediction files ────────────────────────────────────
k1 <- read.csv("results/k1/loocv_predictions_k1.csv")
k2 <- read.csv("results/k2/loocv_predictions_k2.csv")
k3 <- read.csv("results/k3/loocv_predictions_k3.csv")
k5 <- read.csv("results/k5/loocv_predictions_k5.csv")

# ── Compute ROC objects ──────────────────────────────────────
roc_k1 <- roc(k1$True_Label, k1$Predicted_Prob_CFSR, quiet = TRUE)
roc_k2 <- roc(k2$True_Label, k2$Predicted_Prob_CFSR, quiet = TRUE)
roc_k3 <- roc(k3$True_Label, k3$Predicted_Prob_CFSR, quiet = TRUE)
roc_k5 <- roc(k5$True_Label, k5$Predicted_Prob_CFSR, quiet = TRUE)

cat("AUC values:\n")
cat(sprintf("  k=1: %.4f\n", as.numeric(auc(roc_k1))))
cat(sprintf("  k=2: %.4f\n", as.numeric(auc(roc_k2))))
cat(sprintf("  k=3: %.4f\n", as.numeric(auc(roc_k3))))
cat(sprintf("  k=5: %.4f\n", as.numeric(auc(roc_k5))))

# ── Figure 1: Combined ROC plot — all four k values ──────────
png("figures/ROC_curves_combined.png", width = 900, height = 750, res = 130)

par(mar = c(5, 5, 4, 2))

# Plot k=1 first to set up the axes
plot(roc_k1,
     legacy.axes = TRUE,
     col  = "#1f77b4",
     lwd  = 2.5,
     main = "ROC Curves — Multistage Logistic Regression\n(75-Participant Dataset, LOOCV)",
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)",
     cex.main = 1.1,
     cex.lab  = 1.0)

# Add k=2, k=3, k=5
plot(roc_k2, legacy.axes = TRUE, col = "#2ca02c", lwd = 2.5, add = TRUE)
plot(roc_k3, legacy.axes = TRUE, col = "#ff7f0e", lwd = 2.5, add = TRUE)
plot(roc_k5, legacy.axes = TRUE, col = "#d62728", lwd = 2.5, lty = 2, add = TRUE)

# Reference diagonal
abline(0, 1, col = "gray50", lty = 2, lwd = 1)

# Legend
legend("bottomright",
       legend = c(
         sprintf("k = 1  (AUC = %.4f)", as.numeric(auc(roc_k1))),
         sprintf("k = 2  (AUC = %.4f)", as.numeric(auc(roc_k2))),
         sprintf("k = 3  (AUC = %.4f)", as.numeric(auc(roc_k3))),
         sprintf("k = 5  (AUC = %.4f)", as.numeric(auc(roc_k5))),
         "Random (AUC = 0.50)"
       ),
       col = c("#1f77b4","#2ca02c","#ff7f0e","#d62728","gray50"),
       lty = c(1, 1, 1, 2, 2),
       lwd = c(2.5, 2.5, 2.5, 2.5, 1),
       cex = 0.9,
       bg  = "white")

dev.off()
cat("Saved: figures/ROC_curves_combined.png\n")

# ── Figure 2: Individual ROC plots — 2x2 panel ───────────────
png("figures/ROC_curves_individual.png", width = 1200, height = 1000, res = 130)

par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3.5, 1.5))

roc_list   <- list(roc_k1, roc_k2, roc_k3, roc_k5)
k_labels   <- c("k = 1", "k = 2", "k = 3", "k = 5")
k_colors   <- c("#1f77b4","#2ca02c","#ff7f0e","#d62728")
k_lty      <- c(1, 1, 1, 2)

for (i in 1:4) {
  plot(roc_list[[i]],
       legacy.axes = TRUE,
       col  = k_colors[i],
       lty  = k_lty[i],
       lwd  = 2.5,
       main = paste("ROC Curve —", k_labels[i]),
       xlab = "False Positive Rate",
       ylab = "True Positive Rate",
       cex.main = 1.1,
       cex.lab  = 1.0)
  abline(0, 1, col = "gray50", lty = 2, lwd = 1)
  legend("bottomright",
         legend = sprintf("AUC = %.4f", as.numeric(auc(roc_list[[i]]))),
         col = k_colors[i],
         lty = k_lty[i],
         lwd = 2.5,
         cex = 0.95,
         bg  = "white")
}

dev.off()
cat("Saved: figures/ROC_curves_individual.png\n")

cat("\nAll done. Check your figures/ folder.\n")
