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
# Computed per cohort at decision weeks 6 (mid-term) and 11 (end-of-term).
# =============================================================================

source("R/00_setup.R")
suppressMessages({library(relaimpo); library(car)})

model_df <- readRDS(file.path(TAB_DIR, "model_df.rds"))
DECISION_WEEKS <- c(6, 11)
preds <- c("z_cum_freq", "z_cum_imm", "z_cum_div")
nice <- c(z_cum_freq = "Frequency", z_cum_imm = "Immediacy", z_cum_div = "Diversity")
cohorts <- unique(model_df$cohort_id)

# ---- Collinearity: mean pairwise correlation and VIF -----------------------
collin <- purrr::pmap_dfr(
  tidyr::expand_grid(cohort_id = cohorts, week = DECISION_WEEKS),
  function(cohort_id, week) {
    sub <- model_df |> filter(cohort_id == !!cohort_id, week == !!week)
    cm  <- cor(sub[preds])
    v   <- car::vif(lm(final_grade ~ z_cum_freq + z_cum_imm + z_cum_div, sub))
    tibble(cohort_id = cohort_id, week = week,
           mean_abs_cor = mean(abs(cm[lower.tri(cm)])),
           vif_freq = v["z_cum_freq"], vif_imm = v["z_cum_imm"],
           vif_div = v["z_cum_div"])
  }
)
save_tab(collin, "13_collinearity.csv")

cat("\n=========== COLLINEARITY (weeks 6 and 11) ===========\n")
print(as.data.frame(collin |> mutate(across(where(is.numeric), \(x) round(x, 2)))),
      row.names = FALSE)

# ---- LMG relative importance -----------------------------------------------
lmg <- purrr::pmap_dfr(
  tidyr::expand_grid(cohort_id = cohorts, week = DECISION_WEEKS),
  function(cohort_id, week) {
    sub <- model_df |> filter(cohort_id == !!cohort_id, week == !!week)
    m <- lm(final_grade ~ z_cum_freq + z_cum_imm + z_cum_div, data = sub)
    ri <- relaimpo::calc.relimp(m, type = "lmg", rela = FALSE)
    tibble(cohort_id = cohort_id, week = week, indicator = nice[names(ri$lmg)],
           lmg_r2 = as.numeric(ri$lmg), model_r2 = ri$R2)
  }
) |>
  left_join(COHORTS |> dplyr::select(cohort_id, condition, module, year_label),
            by = "cohort_id") |>
  group_by(cohort_id, week) |>
  mutate(lmg_share = lmg_r2 / sum(lmg_r2)) |>
  ungroup() |>
  mutate(indicator = factor(indicator, levels = c("Frequency", "Immediacy", "Diversity")))
save_tab(lmg, "13_lmg_importance.csv")

cat("\n=========== LMG RELATIVE IMPORTANCE (share of explained R2) ===========\n")
for (wk in DECISION_WEEKS) {
  cat(sprintf("\n--- Week %d ---\n", wk))
  print(as.data.frame(
    lmg |> filter(week == wk) |>
      mutate(lmg_share = round(100 * lmg_share, 0)) |>
      dplyr::select(cohort_id, indicator, lmg_share) |>
      pivot_wider(names_from = indicator, values_from = lmg_share)
  ), row.names = FALSE)
}

# ---- Pooled mean shares with bootstrap 95% CIs (resample cohorts) ----------
set.seed(42)
B <- 2000
boot_pooled <- purrr::map_dfr(DECISION_WEEKS, function(wk) {
  sub <- lmg |> filter(week == wk)
  cohort_ids <- unique(sub$cohort_id)
  purrr::map_dfr(1:B, function(b) {
    samp_ids <- sample(cohort_ids, replace = TRUE)
    samp_w <- tibble(cohort_id = samp_ids) |> count(cohort_id, name = "w")
    sub |>
      inner_join(samp_w, by = "cohort_id") |>
      group_by(indicator) |>
      summarise(mean_share = weighted.mean(lmg_share, w), .groups = "drop") |>
      mutate(week = wk, boot = b)
  })
})

lmg_pooled <- boot_pooled |>
  group_by(week, indicator) |>
  summarise(
    mean_share = mean(mean_share),
    ci_lo = quantile(mean_share, 0.025),
    ci_hi = quantile(mean_share, 0.975),
    .groups = "drop"
  ) |>
  mutate(indicator = factor(indicator, levels = c("Frequency", "Immediacy", "Diversity")))
save_tab(lmg_pooled, "13_lmg_pooled_bootstrap.csv")

cat("\n=========== POOLED LMG SHARES (bootstrap 95% CI across cohorts) ===========\n")
print(as.data.frame(
  lmg_pooled |>
    mutate(across(c(mean_share, ci_lo, ci_hi), \(x) round(100 * x, 1))) |>
    rename(share_pct = mean_share, ci_lo_pct = ci_lo, ci_hi_pct = ci_hi)
), row.names = FALSE)

# ---- PCA sanity check -------------------------------------------------------
pca_tbl <- purrr::pmap_dfr(
  tidyr::expand_grid(cohort_id = cohorts, week = DECISION_WEEKS),
  function(cohort_id, week) {
    sub <- model_df |> filter(cohort_id == !!cohort_id, week == !!week)
    pc <- prcomp(sub[preds], scale. = TRUE)
    pc1 <- pc$x[, 1]
    tibble(cohort_id = cohort_id, week = week,
           pc1_var = summary(pc)$importance[2, 1],
           r_pc1_grade = cor(pc1, sub$final_grade, method = "spearman"))
  }
)
save_tab(pca_tbl, "13_pca_summary.csv")

# ---- Figure: stacked LMG importance by week --------------------------------
p_lmg <- lmg |>
  mutate(panel = paste0(module, "\n", year_label, " (", condition, ")"),
         week_label = paste("Week", week)) |>
  ggplot(aes(panel, lmg_r2, fill = indicator)) +
  geom_col() +
  facet_wrap(~ week_label) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Relative importance (LMG) of engagement indicators for final grade",
       subtitle = "Bar height = explained variance (R2), split into fair shares per indicator",
       x = NULL, y = "Explained variance (LMG R2)", fill = "Indicator") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
save_fig(p_lmg, "13_lmg_importance.png", width = 11, height = 5)

# ---- Figure: pooled mean shares with bootstrap CIs -------------------------
p_pooled <- lmg_pooled |>
  mutate(week_label = paste("Week", week)) |>
  ggplot(aes(indicator, 100 * mean_share, fill = indicator)) +
  geom_col(alpha = 0.85) +
  geom_errorbar(aes(ymin = 100 * ci_lo, ymax = 100 * ci_hi), width = 0.2) +
  facet_wrap(~ week_label) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Pooled LMG importance shares across cohorts",
       subtitle = "Bootstrap 95% CIs (resample cohorts, B = 2000)",
       x = NULL, y = "Mean LMG share (%)", fill = "Indicator") +
  theme_bw(base_size = 11) +
  theme(legend.position = "none")
save_fig(p_pooled, "13_lmg_pooled_bootstrap.png", width = 8, height = 4)

cat("\n=========== PCA: PC1 variance and PC1-grade correlation ===========\n")
print(as.data.frame(pca_tbl |> mutate(across(where(is.numeric), \(x) round(x, 2)))),
      row.names = FALSE)

cat("\nSaved figures: 13_lmg_importance.png, 13_lmg_pooled_bootstrap.png\n")
cat("Saved tables: 13_collinearity.csv, 13_lmg_importance.csv,",
    "13_lmg_pooled_bootstrap.csv, 13_pca_summary.csv\n")
