# ============================================================
# ME/CFS Multistage Logistic Regression Analysis
# Dataset: 75 participants (58 Controls, 17 CFS-R)
# Features: 4 Steiger chi-square values per participant
# Evaluation: Leave-One-Out Cross-Validation (LOOCV)
# ============================================================

library(pROC)

# ── Set your working directory to MECFS_Final ───────────────
# setwd("D:/New folder (4)/MECFS Steiger/MECFS_Final")

# ── Load data ────────────────────────────────────────────────
k1 <- read.csv("data/correlations_k1.csv")
k2 <- read.csv("data/correlations_k2.csv")
k3 <- read.csv("data/correlations_k3.csv")

k5 <- read.csv("data/correlations_k5.csv")
datasets <- list(k1 = k1, k2 = k2, k3 = k3, k5 = k5)

# ── Features and label ───────────────────────────────────────
features <- c("Steiger_Control_Stage1",
              "Steiger_MECFS_Stage1",
              "Steiger_Control_Stage2",
              "Steiger_MECFS_Stage2")

# ── Function: run full LOOCV for one dataset ─────────────────
run_loocv <- function(df, k_label) {

  df$label <- ifelse(df$Original_CFS_Status == "CFS.R", 1, 0)
  n <- nrow(df)
  preds <- numeric(n)
  probs <- numeric(n)

  for (i in 1:n) {
    train <- df[-i, ]
    test  <- df[i,  ]

    model <- glm(label ~ Steiger_Control_Stage1 +
                          Steiger_MECFS_Stage1 +
                          Steiger_Control_Stage2 +
                          Steiger_MECFS_Stage2,
                 data   = train,
                 family = binomial)

    prob      <- predict(model, newdata = test, type = "response")
    probs[i]  <- prob
    preds[i]  <- ifelse(prob >= 0.5, 1, 0)
  }

  true <- df$label

  TP <- sum(preds == 1 & true == 1)
  TN <- sum(preds == 0 & true == 0)
  FP <- sum(preds == 1 & true == 0)
  FN <- sum(preds == 0 & true == 1)

  accuracy         <- (TP + TN) / n
  sensitivity      <- TP / (TP + FN)
  specificity      <- TN / (TN + FP)
  precision        <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)
  balanced_acc     <- (sensitivity + specificity) / 2
  roc_obj          <- roc(true, probs, quiet = TRUE)
  auc_val          <- as.numeric(auc(roc_obj))

  cat("\n========================================\n")
  cat("Results for", k_label, "\n")
  cat("========================================\n")
  cat(sprintf("Accuracy:         %.2f%%\n", accuracy * 100))
  cat(sprintf("Sensitivity:      %.2f%%\n", sensitivity * 100))
  cat(sprintf("Specificity:      %.2f%%\n", specificity * 100))
  cat(sprintf("Precision:        %.2f%%\n", precision * 100))
  cat(sprintf("Balanced Acc:     %.2f%%\n", balanced_acc * 100))
  cat(sprintf("AUC:              %.4f\n",   auc_val))
  cat(sprintf("CFS-R found:      %d / %d\n", TP, sum(true == 1)))
  cat(sprintf("Confusion matrix: TN=%d  FP=%d  FN=%d  TP=%d\n", TN, FP, FN, TP))

  # Save metrics
  metrics <- data.frame(
    k          = k_label,
    Accuracy   = round(accuracy * 100, 2),
    Sensitivity= round(sensitivity * 100, 2),
    Specificity= round(specificity * 100, 2),
    Precision  = round(precision * 100, 2),
    Balanced_Acc = round(balanced_acc * 100, 2),
    AUC        = round(auc_val, 4),
    CFSR_Found = TP,
    CFSR_Total = sum(true == 1),
    TN = TN, FP = FP, FN = FN, TP = TP
  )

  write.csv(metrics,
            file      = paste0("results/", k_label, "/loocv_metrics_", k_label, ".csv"),
            row.names = FALSE)

  # ROC curve — manual plot so axis is clean
  roc_df <- data.frame(
    fpr = 1 - roc_obj$specificities,
    tpr = roc_obj$sensitivities
  )
  roc_df <- roc_df[order(roc_df$fpr), ]
  
  png(paste0("figures/", k_label, "/ROC_curve_", k_label, ".png"),
      width = 800, height = 600)
  plot(roc_df$fpr, roc_df$tpr,
       type = "l",
       col  = "steelblue",
       lwd  = 2,
       xlim = c(0, 1),
       ylim = c(0, 1),
       xlab = "False Positive Rate (1 - Specificity)",
       ylab = "True Positive Rate (Sensitivity)",
       main = paste("ROC Curve \u2014", k_label))
  abline(0, 1, col = "gray", lty = 2)
  legend("bottomright",
         legend = paste("AUC =", round(auc_val, 4)),
         col = "steelblue", lwd = 2)
  dev.off()

  # Boxplots of features by group
  png(paste0("figures/", k_label, "/Feature_Boxplots_", k_label, ".png"),
      width = 1000, height = 700)
  par(mfrow = c(2, 2))
  for (feat in features) {
    boxplot(df[[feat]] ~ df$Original_CFS_Status,
            main   = paste(feat, "-", k_label),
            xlab   = "Group",
            ylab   = "Chi-square value",
            col    = c("lightblue", "salmon"))
  }
  dev.off()

  return(invisible(metrics))
}

# ── Run for k=1, k=2, k=3 ────────────────────────────────────
results_list <- list()
for (k_label in names(datasets)) {
  results_list[[k_label]] <- run_loocv(datasets[[k_label]], k_label)
}

# ── Welch t-tests on k=2 features ───────────────────────────
cat("\n========================================\n")
cat("Welch t-tests (k=2)\n")
cat("========================================\n")

df2 <- k2
df2$label <- ifelse(df2$Original_CFS_Status == "CFS.R", 1, 0)

for (feat in features) {
  ctrl  <- df2[[feat]][df2$label == 0]
  cfsr  <- df2[[feat]][df2$label == 1]
  ttest <- t.test(ctrl, cfsr)
  cat(sprintf("%-40s p = %.4e\n", feat, ttest$p.value))
}

# ── Odds ratios from k=2 full model ─────────────────────────
cat("\n========================================\n")
cat("Odds Ratios (k=2, full model)\n")
cat("========================================\n")

df2$label <- ifelse(df2$Original_CFS_Status == "CFS.R", 1, 0)
full_model <- glm(label ~ Steiger_Control_Stage1 +
                           Steiger_MECFS_Stage1 +
                           Steiger_Control_Stage2 +
                           Steiger_MECFS_Stage2,
                  data   = df2,
                  family = binomial)

or_table <- exp(cbind(OR = coef(full_model),
                      confint(full_model)))
print(round(or_table, 4))

or_df <- as.data.frame(or_table)
or_df$feature <- rownames(or_df)
or_df$pvalue  <- summary(full_model)$coefficients[, 4]
write.csv(or_df, "results/k2/odds_ratios_k2.csv", row.names = FALSE)

# ── Summary comparison table ─────────────────────────────────
all_metrics <- do.call(rbind, results_list)
write.csv(all_metrics, "results/k_comparison_metrics.csv", row.names = FALSE)

cat("\n========================================\n")
cat("Summary across k values\n")
cat("========================================\n")
print(all_metrics[, c("k","Accuracy","Sensitivity","Specificity",
                       "AUC","CFSR_Found","CFSR_Total")])

cat("\nAll done! Check results/ and figures/ folders.\n")
