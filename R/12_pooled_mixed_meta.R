# =============================================================================
# 12_pooled_mixed_meta.R   (WP2: cross-cohort synthesis)
#
# Replace five piecewise comparisons with two principled syntheses at week 11:
#   (A) Mixed-effects model with a random engagement slope across cohorts,
#       giving the average (pooled) slope and the between-cohort slope SD.
#   (B) Random-effects meta-analysis (metafor) over the 5 cohorts using
#       Fisher-z transformed engagement-grade correlations: pooled effect,
#       Q-test, I-squared, forest plots, and a delivery-condition moderator.
#
# Confounding note: condition = year = cohort. Cohort is the unit of synthesis;
# the condition moderator has only 5 "studies" so it is under-powered - reported
# as descriptive heterogeneity, not a causal delivery-mode test.
# =============================================================================

source("R/00_setup.R")
suppressMessages({library(lme4); library(lmerTest); library(metafor)})

model_df <- readRDS(file.path(TAB_DIR, "model_df.rds"))
wk <- 11
d <- model_df |> filter(week == wk)

indicators <- c(Frequency = "cum_freq", Immediacy = "cum_imm",
                Diversity = "cum_div", IDF = "cum_IDF")
z_ind <- c(Frequency = "z_cum_freq", Immediacy = "z_cum_imm",
           Diversity = "z_cum_div", IDF = "z_cum_IDF")

# ---- (A) Mixed-effects models: random slope across cohorts ------------------
mixed_tbl <- purrr::imap_dfr(z_ind, function(zc, nm) {
  f <- as.formula(paste("final_grade ~", zc, "+ (", zc, "| cohort_id)"))
  m <- suppressMessages(lmer(f, data = d, REML = TRUE))
  fe <- summary(m)$coefficients
  vc <- as.data.frame(lme4::VarCorr(m))
  slope_sd <- vc$sdcor[vc$grp == "cohort_id" & vc$var1 == zc & is.na(vc$var2)]
  tibble(indicator = nm,
         fixed_slope = fe[zc, "Estimate"],
         se = fe[zc, "Std. Error"],
         p = fe[zc, "Pr(>|t|)"],
         between_cohort_slope_sd = ifelse(length(slope_sd) == 0, NA, slope_sd),
         singular = isSingular(m))
})
save_tab(mixed_tbl, "12_mixed_models.csv")

cat("\n=========== MIXED-EFFECTS: pooled slope + between-cohort slope SD (week 11) ===========\n")
print(as.data.frame(mixed_tbl |>
  mutate(across(c(fixed_slope, se, between_cohort_slope_sd), \(x) round(x, 2)),
         p = signif(p, 2))), row.names = FALSE)

# ---- (B) Random-effects meta-analysis of correlations ----------------------
es <- purrr::imap_dfr(indicators, function(col, nm) {
  d |> group_by(cohort_id) |>
    summarise(r = cor(.data[[col]], final_grade, method = "pearson"),
              n = n(), .groups = "drop") |>
    mutate(indicator = nm)
}) |>
  left_join(COHORTS |> select(cohort_id, condition, year_label, module),
            by = "cohort_id")

es <- metafor::escalc(measure = "ZCOR", ri = es$r, ni = es$n, data = es)
save_tab(es |> mutate(r = round(r, 3), yi = round(yi, 3), vi = round(vi, 4)),
         "12_meta_effect_sizes.csv")

pooled <- purrr::imap_dfr(indicators, function(col, nm) {
  sub <- es |> filter(indicator == nm)
  m <- metafor::rma(yi, vi, data = sub, method = "REML")
  mod <- tryCatch(metafor::rma(yi, vi, mods = ~ condition, data = sub,
                               method = "REML"),
                  error = function(e) NULL)
  tibble(indicator = nm,
         pooled_r = transf.ztor(m$b[1]),
         ci_lo = transf.ztor(m$ci.lb), ci_hi = transf.ztor(m$ci.ub),
         Q = m$QE, Q_p = m$QEp, I2 = m$I2,
         mod_condition_p = if (is.null(mod)) NA else mod$QMp)
})
save_tab(pooled, "12_meta_pooled.csv")

cat("\n=========== RANDOM-EFFECTS META-ANALYSIS (week 11) ===========\n")
print(as.data.frame(pooled |>
  mutate(across(c(pooled_r, ci_lo, ci_hi), \(x) round(x, 3)),
         Q = round(Q, 1), I2 = round(I2, 1),
         across(c(Q_p, mod_condition_p), \(x) signif(x, 2)))), row.names = FALSE)

# ---- Forest plot for IDF ----------------------------------------------------
sub_idf <- es |> filter(indicator == "IDF") |>
  arrange(condition) |>
  mutate(slab = paste0(module, " ", year_label, " (", condition, ")"))
m_idf <- metafor::rma(yi, vi, data = sub_idf, method = "REML")

png(file.path(FIG_DIR, "12_forest_IDF.png"), width = 1000, height = 520, res = 120)
metafor::forest(m_idf, slab = sub_idf$slab,
                transf = transf.ztor, refline = 0,
                xlab = "Engagement-grade correlation (r)",
                header = c("Cohort", "r [95% CI]"),
                mlab = sprintf("RE pooled (I2 = %.0f%%)", m_idf$I2))
title("Cumulative IDF engagement vs final grade (week 11)", cex.main = 1)
dev.off()

# ---- Pooled-r-by-indicator summary figure ----------------------------------
p_pool <- pooled |>
  mutate(indicator = factor(indicator, levels = names(indicators))) |>
  ggplot(aes(indicator, pooled_r)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi), linewidth = 0.8) +
  geom_text(aes(label = sprintf("I2=%.0f%%", I2)), vjust = -1.2, size = 3.2) +
  labs(title = "Pooled engagement-grade correlation across 5 cohorts (random-effects meta-analysis)",
       subtitle = "Week 11; whiskers = 95% CI of pooled r; I-squared = between-cohort heterogeneity",
       x = NULL, y = "Pooled Pearson r") +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_bw(base_size = 11)
save_fig(p_pool, "12_meta_pooled_summary.png", width = 8, height = 5)

cat("\nSaved figures: 12_forest_IDF.png, 12_meta_pooled_summary.png\n")
cat("Saved tables: 12_mixed_models.csv, 12_meta_effect_sizes.csv, 12_meta_pooled.csv\n")
