# =============================================================================
# 11_outcome_models.R   (WP1: correct outcome models + tail effects)
#
# Final grades are bounded [0,100] and ceiling-skewed, so OLS is mis-specified.
# We (a) compare OLS with fractional logit and beta regression to show the
# engagement->grade conclusions are robust to specification, and (b) use
# quantile regression to test the substantive question: does engagement matter
# MORE for weaker students (lower tail) than for the average student?
#
# Predictors: within-cohort z-scored CUMULATIVE indicators (from model_df).
# Headline decision week = 6 (mid-term); week 11 reported as end benchmark.
# =============================================================================

source("R/00_setup.R")
source("R/features.R")
suppressMessages({library(betareg); library(quantreg); library(broom)})

model_df <- readRDS(file.path(TAB_DIR, "model_df.rds"))

DECISION_WEEKS <- c(6, 11)
taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
preds <- c("z_cum_freq", "z_cum_imm", "z_cum_div")
nice <- c(z_cum_freq = "Frequency", z_cum_imm = "Immediacy", z_cum_div = "Diversity")

# Smithson & Verkuilen (2006) squeeze of (0,1) endpoints for beta regression.
squeeze01 <- function(y) {
  n <- length(y); (y * (n - 1) + 0.5) / n
}

# ---- (A) Outcome-model comparison: OLS vs fractional logit vs beta ----------
compare_models <- function(df) {
  df <- df |> mutate(y01 = pmin(pmax(final_grade / 100, 0), 1))
  f  <- as.formula(paste("y01 ~", paste(preds, collapse = " + ")))
  fg <- as.formula(paste("final_grade ~", paste(preds, collapse = " + ")))

  ols  <- lm(fg, data = df)
  frac <- glm(f, data = df, family = quasibinomial(link = "logit"))
  beta <- tryCatch(betareg(update(f, squeeze01(y01) ~ .), data = df),
                   error = function(e) NULL)

  bind_rows(
    broom::tidy(ols)  |> mutate(model = "OLS (grade points)",
                                fit = summary(ols)$r.squared),
    broom::tidy(frac) |> mutate(model = "Fractional logit (log-odds)",
                                fit = 1 - frac$deviance / frac$null.deviance),
    if (!is.null(beta))
      broom::tidy(beta) |> filter(component == "mean") |>
        mutate(model = "Beta regression (log-odds)", fit = beta$pseudo.r.squared)
  ) |>
    filter(term %in% preds) |>
    select(model, term, estimate, std.error, statistic, p.value, fit)
}

comp <- purrr::pmap_dfr(
  tidyr::expand_grid(cohort = unique(model_df$cohort_id), wk = DECISION_WEEKS),
  function(cohort, wk) {
    d <- model_df |> filter(cohort_id == cohort, week == wk)
    compare_models(d) |> mutate(cohort_id = cohort, week = wk, .before = 1)
  }
)
save_tab(comp, "11_outcome_model_comparison.csv")

cat("\n=========== OUTCOME-MODEL COMPARISON (week 6, Diversity coef) ===========\n")
print(as.data.frame(
  comp |> filter(week == 6, term == "z_cum_div") |>
    transmute(cohort_id, model, estimate = round(estimate, 3),
              p = signif(p.value, 2), pseudoR2 = round(fit, 3))
), row.names = FALSE)

# ---- OLS diagnostics on one cohort (motivates the alternatives) -------------
d_demo <- model_df |> filter(cohort_id == "STAT0004_2021", week == 11)
ols_demo <- lm(final_grade ~ z_cum_freq + z_cum_imm + z_cum_div, data = d_demo)
png(file.path(FIG_DIR, "11_ols_diagnostics.png"), width = 900, height = 700, res = 110)
par(mfrow = c(2, 2)); plot(ols_demo); dev.off()

# ---- (B) Quantile regression: tail effects ---------------------------------
fit_rq_tau <- function(df, rhs, tau) {
  m <- quantreg::rq(as.formula(paste("final_grade ~", rhs)), tau = tau, data = df)
  s <- tryCatch(summary(m, se = "boot", R = 500),
                error = function(e) summary(m, se = "nid"))
  co <- as.data.frame(s$coefficients)
  co$term <- rownames(co); co$tau <- tau
  tibble(term = co$term, tau = tau,
         estimate = co[, "Value"], std.error = co[, "Std. Error"],
         p.value = co[, "Pr(>|t|)"])
}

# Headline: single-predictor cumulative IDF (clean tail interpretation)
qr_idf <- purrr::pmap_dfr(
  tidyr::expand_grid(cohort = unique(model_df$cohort_id), week = DECISION_WEEKS, tau = taus),
  function(cohort, week, tau) {
    d <- model_df |> filter(cohort_id == cohort, week == !!week)
    fit_rq_tau(d, "z_cum_IDF", tau) |>
      filter(term == "z_cum_IDF") |>
      mutate(cohort_id = cohort, week = week)
  }
) |>
  left_join(COHORTS |> select(cohort_id, condition, module), by = "cohort_id") |>
  mutate(lo = estimate - 1.96 * std.error, hi = estimate + 1.96 * std.error)
save_tab(qr_idf, "11_quantile_idf.csv")

# Per-indicator joint model across taus
qr_ind <- purrr::pmap_dfr(
  tidyr::expand_grid(cohort = unique(model_df$cohort_id), week = DECISION_WEEKS, tau = taus),
  function(cohort, week, tau) {
    d <- model_df |> filter(cohort_id == cohort, week == !!week)
    fit_rq_tau(d, paste(preds, collapse = " + "), tau) |>
      filter(term %in% preds) |>
      mutate(cohort_id = cohort, week = week)
  }
) |>
  left_join(COHORTS |> select(cohort_id, condition, module), by = "cohort_id") |>
  mutate(indicator = recode(term, z_cum_freq = "Frequency",
                            z_cum_imm = "Immediacy", z_cum_div = "Diversity"),
         lo = estimate - 1.96 * std.error, hi = estimate + 1.96 * std.error)
save_tab(qr_ind, "11_quantile_indicators.csv")

# Univariate quantile models (one predictor at a time)
qr_uni <- purrr::pmap_dfr(
  tidyr::expand_grid(
    cohort = unique(model_df$cohort_id), week = DECISION_WEEKS,
    pred = preds, tau = taus
  ),
  function(cohort, week, pred, tau) {
    d <- model_df |> filter(cohort_id == cohort, week == !!week)
    fit_rq_tau(d, pred, tau) |>
      filter(term == pred) |>
      mutate(cohort_id = cohort, week = week, predictor = pred)
  }
) |>
  left_join(COHORTS |> select(cohort_id, condition, module), by = "cohort_id") |>
  mutate(indicator = nice[predictor],
         lo = estimate - 1.96 * std.error, hi = estimate + 1.96 * std.error)
save_tab(qr_uni, "11_quantile_univariate.csv")

# ---- Bootstrap CI on tau_0.1 - tau_0.5 per cohort --------------------------
tail_boot <- purrr::pmap_dfr(
  tidyr::expand_grid(cohort = unique(model_df$cohort_id), week = DECISION_WEEKS),
  function(cohort, week) {
    d <- model_df |> filter(cohort_id == cohort, week == !!week)
    set.seed(42)
    B <- 500
    n <- nrow(d)
    diffs <- vapply(seq_len(B), function(b) {
      idx <- sample.int(n, n, replace = TRUE)
      sub <- d[idx, ]
      b1 <- coef(quantreg::rq(final_grade ~ z_cum_IDF, tau = 0.1, data = sub))["z_cum_IDF"]
      b5 <- coef(quantreg::rq(final_grade ~ z_cum_IDF, tau = 0.5, data = sub))["z_cum_IDF"]
      b1 - b5
    }, numeric(1))
    b1 <- coef(quantreg::rq(final_grade ~ z_cum_IDF, tau = 0.1, data = d))["z_cum_IDF"]
    b5 <- coef(quantreg::rq(final_grade ~ z_cum_IDF, tau = 0.5, data = d))["z_cum_IDF"]
    tibble(
      cohort_id = cohort, week = week,
      contrast = b1 - b5,
      ci_lo = quantile(diffs, 0.025),
      ci_hi = quantile(diffs, 0.975),
      sig_positive = ci_lo > 0
    )
  }
) |>
  left_join(COHORTS |> select(cohort_id, condition, module), by = "cohort_id")
save_tab(tail_boot, "11_tail_contrast_bootstrap.csv")

# ---- Tail vs median summary (descriptive) ----------------------------------
tail_summary <- qr_idf |>
  filter(tau %in% c(0.1, 0.5, 0.9)) |>
  select(cohort_id, condition, week, tau, estimate) |>
  pivot_wider(names_from = tau, values_from = estimate,
              names_prefix = "tau_") |>
  left_join(
    tail_boot |> select(cohort_id, week, contrast_boot = contrast,
                        ci_lo, ci_hi, sig_positive),
    by = c("cohort_id", "week")
  ) |>
  mutate(low_minus_median = round(tau_0.1 - tau_0.5, 2),
         across(starts_with("tau_"), \(x) round(x, 2)),
         contrast_boot = round(contrast_boot, 2),
         ci_lo = round(ci_lo, 2), ci_hi = round(ci_hi, 2))
save_tab(tail_summary, "11_tail_vs_median.csv")

# ---- Figures ---------------------------------------------------------------
# IDF quantile: coefficient + CI at each tau (week 11, cleaner for report)
p_idf <- qr_idf |>
  filter(week == 11) |>
  ggplot(aes(tau, estimate, colour = condition)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = condition), alpha = 0.15,
              colour = NA) +
  geom_line(linewidth = 0.9) + geom_point(size = 2) +
  facet_wrap(~ cohort_id, ncol = 3, scales = "free_y") +
  scale_colour_manual(values = CONDITION_COLOURS, drop = FALSE) +
  scale_fill_manual(values = CONDITION_COLOURS, drop = FALSE) +
  scale_x_continuous(breaks = taus) +
  labs(title = "Quantile regression: cumulative engagement (IDF) and final grade",
       subtitle = "Week 11. Steeper slopes at low tau = engagement matters more for weaker students",
       x = expression(Quantile~tau~of~final~grade),
       y = "Grade points per +1 SD of cumulative IDF",
       colour = "Delivery condition", fill = "Delivery condition") +
  theme_bw(base_size = 10) + theme(legend.position = "bottom")
save_fig(p_idf, "11_quantile_idf.png", width = 10, height = 6)

# Week 6 version
p_idf_w6 <- qr_idf |>
  filter(week == 6) |>
  ggplot(aes(tau, estimate, colour = condition)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = condition), alpha = 0.15,
              colour = NA) +
  geom_line(linewidth = 0.9) + geom_point(size = 2) +
  facet_wrap(~ cohort_id, ncol = 3, scales = "free_y") +
  scale_colour_manual(values = CONDITION_COLOURS, drop = FALSE) +
  scale_fill_manual(values = CONDITION_COLOURS, drop = FALSE) +
  scale_x_continuous(breaks = taus) +
  labs(title = "Quantile regression: cumulative engagement (IDF) and final grade",
       subtitle = "Week 6 (mid-term decision point)",
       x = expression(Quantile~tau~of~final~grade),
       y = "Grade points per +1 SD of cumulative IDF",
       colour = "Delivery condition", fill = "Delivery condition") +
  theme_bw(base_size = 10) + theme(legend.position = "bottom")
save_fig(p_idf_w6, "11_quantile_idf_wk6.png", width = 10, height = 6)

p_ind <- qr_ind |>
  filter(week == 11) |>
  ggplot(aes(factor(tau), estimate, colour = condition, group = cohort_id)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_line(linewidth = 0.7) + geom_point(size = 1.2) +
  facet_grid(indicator ~ module) +
  scale_colour_manual(values = CONDITION_COLOURS, drop = FALSE) +
  labs(title = "Quantile regression by indicator (week 11, joint model)",
       x = expression(Quantile~tau), y = "Grade points per +1 SD",
       colour = "Delivery condition") +
  theme_bw(base_size = 10) + theme(legend.position = "bottom")
save_fig(p_ind, "11_quantile_indicators.png", width = 9, height = 8)

# Univariate quantile: pooled mean slope across cohorts at each tau
p_uni <- qr_uni |>
  group_by(week, indicator, tau) |>
  summarise(mean_est = mean(estimate), .groups = "drop") |>
  mutate(week_label = paste("Week", week),
         indicator = factor(indicator, levels = c("Frequency", "Immediacy", "Diversity"))) |>
  ggplot(aes(tau, mean_est, colour = indicator, group = indicator)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  facet_wrap(~ week_label) +
  scale_colour_brewer(palette = "Set2") +
  scale_x_continuous(breaks = taus) +
  labs(title = "Univariate quantile regression: mean slope by indicator",
       subtitle = "One predictor at a time; mean across five cohorts",
       x = expression(Quantile~tau~of~final~grade),
       y = "Mean grade points per +1 SD", colour = "Indicator") +
  theme_bw(base_size = 11) + theme(legend.position = "bottom")
save_fig(p_uni, "11_quantile_univariate.png", width = 9, height = 4.5)

cat("\n=========== TAIL EFFECT: IDF slope at tau=0.1 vs 0.5 vs 0.9 ===========\n")
for (wk in DECISION_WEEKS) {
  cat(sprintf("\n--- Week %d ---\n", wk))
  print(as.data.frame(tail_summary |> filter(week == wk)), row.names = FALSE)
}

n_sig <- sum(tail_boot$sig_positive)
cat(sprintf("\nBootstrap tail contrast (tau_0.1 - tau_0.5): %d/%d cohort-weeks with CI > 0\n",
            n_sig, nrow(tail_boot)))

cat("\nSaved figures: 11_quantile_idf.png, 11_quantile_idf_wk6.png,",
    "11_quantile_indicators.png, 11_quantile_univariate.png, 11_ols_diagnostics.png\n")
cat("Saved tables: 11_outcome_model_comparison.csv, 11_quantile_idf.csv,",
    "11_quantile_indicators.csv, 11_quantile_univariate.csv,",
    "11_tail_vs_median.csv, 11_tail_contrast_bootstrap.csv\n")
