# ── Run this in your MECFS_Final folder ─────────────────────
# setwd("D:/New folder (4)/MECFS Steiger/MECFS_Final")
#
# This script saves the predicted probabilities from LOOCV
# so we can plot the real ROC curves.
# Run this AFTER you have already run analysis.R

library(pROC)

k1 <- read.csv("data/correlations_k1.csv")
k2 <- read.csv("data/correlations_k2.csv")
k3 <- read.csv("data/correlations_k3.csv")
k5 <- read.csv("data/correlations_k5.csv")

datasets <- list(k1 = k1, k2 = k2, k3 = k3, k5 = k5)

features <- c("Steiger_Control_Stage1",
              "Steiger_MECFS_Stage1",
              "Steiger_Control_Stage2",
              "Steiger_MECFS_Stage2")

for (k_label in names(datasets)) {
  
  df <- datasets[[k_label]]
  df$label <- ifelse(df$Original_CFS_Status == "CFS.R", 1, 0)
  n <- nrow(df)
  probs <- numeric(n)
  
  cat("Running LOOCV for", k_label, "...\n")
  
  for (i in 1:n) {
    train <- df[-i, ]
    test  <- df[i,  ]
    
    model <- glm(label ~ Steiger_Control_Stage1 +
                          Steiger_MECFS_Stage1 +
                          Steiger_Control_Stage2 +
                          Steiger_MECFS_Stage2,
                 data = train, family = binomial)
    
    probs[i] <- predict(model, newdata = test, type = "response")
  }
  
  # Save predictions
  out <- data.frame(
    ParticipantID       = df$ParticipantID,
    True_Label          = df$label,
    True_Status         = df$Original_CFS_Status,
    Predicted_Prob_CFSR = round(probs, 6)
  )
  
  write.csv(out,
            paste0("results/", k_label, "/loocv_predictions_", k_label, ".csv"),
            row.names = FALSE)
  
  cat("  Saved results/", k_label, "/loocv_predictions_", k_label, ".csv\n", sep="")
}

cat("\nDone. Upload the four prediction CSV files.\n")
