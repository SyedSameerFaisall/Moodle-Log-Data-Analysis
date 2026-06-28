# =============================================================================
# 13_indicator_importance.R   (WP3: which indicator matters, under collinearity)
#
# Frequency, Immediacy and Diversity are correlated, so partial regression
# slopes are unstable. We quantify each indicator's contribution properly:
#   - collinearity diagnostics (correlation, VIF)
#   - LMG relative-importance decomposition of model R2 (relaimpo) - averages
#     each predictor's contribution over all orderings (a commonality-style
#     fair share)
#   - a one-component PCA summary of the three indicators as a sanity check
# Computed per cohort at week 11.
# =============================================================================

source("R/00_setup.R")
suppressMessages({library(relaimpo); library(car)})

model_df <- readRDS(file.path(TAB_DIR, "model_df.rds"))
d <- model_df |> filter(week == 11)
preds <- c("z_cum_freq", "z_cum_imm", "z_cum_div")
nice <- c(z_cum_freq = "Frequency", z_cum_imm = "Immediacy", z_cum_div = "Diversity")

cohorts <- unique(d$cohort_id)

# ---- Collinearity: mean pairwise correlation and VIF -----------------------
collin <- purrr::map_dfr(cohorts, function(ci) {
  sub <- d |> filter(cohort_id == ci)
  cm  <- cor(sub[preds])
  v   <- car::vif(lm(final_grade ~ z_cum_freq + z_cum_imm + z_cum_div, sub))
  tibble(cohort_id = ci,
         mean_abs_cor = mean(abs(cm[lower.tri(cm)])),
         vif_freq = v["z_cum_freq"], vif_imm = v["z_cum_imm"],
         vif_div = v["z_cum_div"])
})
save_tab(collin, "13_collinearity.csv")

cat("\n=========== COLLINEARITY (week 11) ===========\n")
print(as.data.frame(collin |> mutate(across(where(is.numeric), \(x) round(x, 2)))),
      row.names = FALSE)

# ---- LMG relative importance -----------------------------------------------
lmg <- purrr::map_dfr(cohorts, function(ci) {
  sub <- d |> filter(cohort_id == ci)
  m <- lm(final_grade ~ z_cum_freq + z_cum_imm + z_cum_div, data = sub)
  ri <- relaimpo::calc.relimp(m, type = "lmg", rela = FALSE)
  tibble(cohort_id = ci, indicator = nice[names(ri$lmg)],
         lmg_r2 = as.numeric(ri$lmg), model_r2 = ri$R2)
}) |>
  left_join(COHORTS |> dplyr::select(cohort_id, condition, module, year_label),
            by = "cohort_id") |>
  group_by(cohort_id) |>
  mutate(lmg_share = lmg_r2 / sum(lmg_r2)) |>
  ungroup() |>
  mutate(indicator = factor(indicator, levels = c("Frequency", "Immediacy", "Diversity")))
save_tab(lmg, "13_lmg_importance.csv")

cat("\n=========== LMG RELATIVE IMPORTANCE (share of explained R2) ===========\n")
print(as.data.frame(
  lmg |> mutate(lmg_share = round(100 * lmg_share, 0)) |>
    dplyr::select(cohort_id, indicator, lmg_share) |>
    pivot_wider(names_from = indicator, values_from = lmg_share)
), row.names = FALSE)

# ---- PCA sanity check -------------------------------------------------------
pca_tbl <- purrr::map_dfr(cohorts, function(ci) {
  sub <- d |> filter(cohort_id == ci)
  pc <- prcomp(sub[preds], scale. = TRUE)
  pc1 <- pc$x[, 1]
  tibble(cohort_id = ci,
         pc1_var = summary(pc)$importance[2, 1],
         r_pc1_grade = cor(pc1, sub$final_grade, method = "spearman"))
})
save_tab(pca_tbl, "13_pca_summary.csv")

# ---- Figure: stacked LMG importance ----------------------------------------
p_lmg <- lmg |>
  mutate(panel = paste0(module, "\n", year_label, " (", condition, ")")) |>
  ggplot(aes(panel, lmg_r2, fill = indicator)) +
  geom_col() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Relative importance (LMG) of engagement indicators for final grade",
       subtitle = "Week 11; bar height = explained variance (R2), split into fair shares per indicator",
       x = NULL, y = "Explained variance (LMG R2)", fill = "Indicator") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
save_fig(p_lmg, "13_lmg_importance.png", width = 9, height = 5)

cat("\n=========== PCA: PC1 variance and PC1-grade correlation ===========\n")
print(as.data.frame(pca_tbl |> mutate(across(where(is.numeric), \(x) round(x, 2)))),
      row.names = FALSE)

cat("\nSaved figure: 13_lmg_importance.png\n")
cat("Saved tables: 13_collinearity.csv, 13_lmg_importance.csv, 13_pca_summary.csv\n")
