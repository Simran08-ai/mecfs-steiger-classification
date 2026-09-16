# ME/CFS Classification with Steiger Feature Engineering

Machine learning classification of **Myalgic Encephalomyelitis / Chronic Fatigue Syndrome (ME/CFS)** using a Steiger-based feature engineering approach. This project builds and compares classifiers across multiple datasets, evaluating them with leave-one-out cross-validation (LOOCV).

This is my Master's dissertation work in health informatics.

---

## Approach

The pipeline engineers features from **Steiger correlation structure** and trains three classifiers to distinguish ME/CFS cases from controls:

- **Logistic regression**
- **Random forest**
- **Support vector machine (SVM)**

Models are evaluated with **leave-one-out cross-validation (LOOCV)** across the K1, K2, K3, and K5 datasets, with ROC/AUC as the primary metric.

---

## Key result

The best-performing model reached **AUC ≈ 0.90** (Steiger Stage 1+2 model on the K1 dataset). ROC curves and per-dataset comparisons are in `figures/` and `results/`.

---

## Repo structure

| Folder | Contents |
|--------|----------|
| `scripts/` | The R pipeline — `analysis.R` (main modeling), `plot_roc_curves.R` (ROC visualisation), `save_predictions.R` (LOOCV output) |
| `figures/` | ROC curves and feature boxplots for each dataset (K1–K5), plus combined comparisons |
| `results/` | LOOCV performance metrics per dataset and an overall comparison table |

---

## A note on data

The underlying ME/CFS datasets are **not included** in this repository. They contain sensitive clinical data and are subject to research-use restrictions, so they cannot be shared publicly. The scripts document the expected data structure; the figures and metrics reflect results produced from the full pipeline.

---

## Tools

`R` · logistic regression · random forest · SVM · LOOCV · ROC/AUC analysis · Steiger feature engineering

---

## Notes

This is dissertation research. Results are from the analysis pipeline described above and reflect work in progress toward the final dissertation.
