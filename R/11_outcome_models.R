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
preds <- c("z_cum_freq", "z_cum_imm", "z_cum_div")

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
taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)

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
  tidyr::expand_grid(cohort = unique(model_df$cohort_id), tau = taus),
  function(cohort, tau) {
    d <- model_df |> filter(cohort_id == cohort, week == 11)
    fit_rq_tau(d, "z_cum_IDF", tau) |>
      filter(term == "z_cum_IDF") |>
      mutate(cohort_id = cohort)
  }
) |>
  left_join(COHORTS |> select(cohort_id, condition, module), by = "cohort_id") |>
  mutate(lo = estimate - 1.96 * std.error, hi = estimate + 1.96 * std.error)
save_tab(qr_idf, "11_quantile_idf.csv")

# Per-indicator joint model across taus
qr_ind <- purrr::pmap_dfr(
  tidyr::expand_grid(cohort = unique(model_df$cohort_id), tau = taus),
  function(cohort, tau) {
    d <- model_df |> filter(cohort_id == cohort, week == 11)
    fit_rq_tau(d, paste(preds, collapse = " + "), tau) |>
      filter(term %in% preds) |>
      mutate(cohort_id = cohort)
  }
) |>
  left_join(COHORTS |> select(cohort_id, condition, module), by = "cohort_id") |>
  mutate(indicator = recode(term, z_cum_freq = "Frequency",
                            z_cum_imm = "Immediacy", z_cum_div = "Diversity"),
         lo = estimate - 1.96 * std.error, hi = estimate + 1.96 * std.error)
save_tab(qr_ind, "11_quantile_indicators.csv")

# ---- Figures ---------------------------------------------------------------
p_idf <- qr_idf |>
  ggplot(aes(factor(tau), estimate, colour = condition, group = cohort_id)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = condition), alpha = 0.12,
              colour = NA) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.6) +
  facet_wrap(~ module) +
  scale_colour_manual(values = CONDITION_COLOURS, drop = FALSE) +
  scale_fill_manual(values = CONDITION_COLOURS, drop = FALSE) +
  labs(title = "Quantile regression: effect of cumulative engagement (IDF) across the grade distribution",
       subtitle = "Week 11. Larger coefficients at low tau = engagement matters more for weaker students",
       x = expression(Quantile~tau~of~final~grade),
       y = "Grade points per +1 SD of cumulative IDF",
       colour = "Delivery condition", fill = "Delivery condition") +
  theme_bw(base_size = 11) + theme(legend.position = "bottom")
save_fig(p_idf, "11_quantile_idf.png", width = 9, height = 5)

p_ind <- qr_ind |>
  ggplot(aes(factor(tau), estimate, colour = condition, group = cohort_id)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_line(linewidth = 0.7) + geom_point(size = 1.2) +
  facet_grid(indicator ~ module) +
  scale_colour_manual(values = CONDITION_COLOURS, drop = FALSE) +
  labs(title = "Quantile regression by indicator (week 11)",
       x = expression(Quantile~tau), y = "Grade points per +1 SD",
       colour = "Delivery condition") +
  theme_bw(base_size = 10) + theme(legend.position = "bottom")
save_fig(p_ind, "11_quantile_indicators.png", width = 9, height = 8)

# ---- Tail vs median test: pooled low-tail (0.1) vs median (0.5) slope -------
tail_summary <- qr_idf |>
  filter(tau %in% c(0.1, 0.5, 0.9)) |>
  select(cohort_id, condition, tau, estimate) |>
  pivot_wider(names_from = tau, values_from = estimate,
              names_prefix = "tau_") |>
  mutate(low_minus_median = round(tau_0.1 - tau_0.5, 2),
         across(starts_with("tau_"), \(x) round(x, 2)))
save_tab(tail_summary, "11_tail_vs_median.csv")

cat("\n=========== TAIL EFFECT: IDF slope at tau=0.1 vs 0.5 vs 0.9 (week 11) ===========\n")
print(as.data.frame(tail_summary), row.names = FALSE)

cat("\nSaved figures: 11_quantile_idf.png, 11_quantile_indicators.png, 11_ols_diagnostics.png\n")
cat("Saved tables: 11_outcome_model_comparison.csv, 11_quantile_idf.csv,",
    "11_quantile_indicators.csv, 11_tail_vs_median.csv\n")
