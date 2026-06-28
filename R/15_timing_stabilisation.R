# =============================================================================
# 15_timing_stabilisation.R   (WP5: rigorous early-prediction timing)
#
# The paper claims the metric "stabilises" around week 3-5 informally. We make
# this precise with a paired bootstrap: for each cohort and week w, resample
# students and compute the difference between the week-11 correlation and the
# week-w correlation on the SAME resample. The stabilisation week is the first
# week whose 95% bootstrap CI for (rho_11 - rho_w) includes 0 - i.e. engagement
# measured that early is statistically indistinguishable from end-of-term.
# We then compare the stabilisation week across delivery conditions.
# =============================================================================

source("R/00_setup.R")
set.seed(2024)
R_BOOT <- 2000

model_df <- readRDS(file.path(TAB_DIR, "model_df.rds"))

boot_cohort <- function(ci) {
  w <- model_df |>
    filter(cohort_id == ci) |>
    dplyr::select(User, week, cum_IDF, final_grade) |>
    tidyr::pivot_wider(names_from = week, values_from = cum_IDF,
                       names_prefix = "w")
  wk_cols <- grep("^w\\d+$", names(w), value = TRUE)
  weeks <- as.integer(sub("w", "", wk_cols))
  ord <- order(weeks); wk_cols <- wk_cols[ord]; weeks <- weeks[ord]
  fg <- w$final_grade
  M  <- as.matrix(w[wk_cols])
  last <- which.max(weeks)               # week 11 column index

  point <- apply(M, 2, \(x) cor(x, fg, method = "spearman"))

  n <- nrow(M)
  diffs <- matrix(NA_real_, nrow = R_BOOT, ncol = length(weeks))
  for (b in seq_len(R_BOOT)) {
    idx <- sample.int(n, n, replace = TRUE)
    rb  <- suppressWarnings(apply(M[idx, , drop = FALSE], 2,
                                  \(x) cor(x, fg[idx], method = "spearman")))
    diffs[b, ] <- rb[last] - rb            # rho_11 - rho_w per week
  }

  tibble(cohort_id = ci, week = weeks,
         rho = point,
         rho_lo = apply(M, 2, \(x) NA)[seq_along(weeks)],  # placeholder
         diff_med = apply(diffs, 2, median),
         diff_lo  = apply(diffs, 2, quantile, 0.025),
         diff_hi  = apply(diffs, 2, quantile, 0.975))
}

# Per-week rho bootstrap CI (separate, for the ribbon) -----------------------
boot_rho_ci <- function(ci) {
  w <- model_df |> filter(cohort_id == ci) |>
    dplyr::select(User, week, cum_IDF, final_grade) |>
    tidyr::pivot_wider(names_from = week, values_from = cum_IDF, names_prefix = "w")
  wk_cols <- grep("^w\\d+$", names(w), value = TRUE)
  weeks <- as.integer(sub("w", "", wk_cols)); ord <- order(weeks)
  wk_cols <- wk_cols[ord]; weeks <- weeks[ord]
  fg <- w$final_grade; M <- as.matrix(w[wk_cols]); n <- nrow(M)
  boot <- replicate(R_BOOT, {
    idx <- sample.int(n, n, replace = TRUE)
    apply(M[idx, , drop = FALSE], 2, \(x) cor(x, fg[idx], method = "spearman"))
  })
  tibble(cohort_id = ci, week = weeks,
         rho_lo = apply(boot, 1, quantile, 0.025),
         rho_hi = apply(boot, 1, quantile, 0.975))
}

cohorts <- unique(model_df$cohort_id)
diff_tbl <- purrr::map_dfr(cohorts, boot_cohort) |> dplyr::select(-rho_lo)
ci_tbl   <- purrr::map_dfr(cohorts, boot_rho_ci)

res <- diff_tbl |>
  left_join(ci_tbl, by = c("cohort_id", "week")) |>
  left_join(COHORTS |> dplyr::select(cohort_id, condition, module, year_label),
            by = "cohort_id")
save_tab(res, "15_stabilisation_bootstrap.csv")

# Stabilisation week: first week whose (rho_11 - rho_w) CI includes 0.
stab <- res |>
  group_by(cohort_id, condition, module, year_label) |>
  arrange(week, .by_group = TRUE) |>
  summarise(rho_final = rho[which.max(week)],
            stab_week = {
              ok <- diff_lo <= 0 & diff_hi >= 0
              w_ok <- week[ok]
              if (length(w_ok)) min(w_ok) else NA_integer_
            }, .groups = "drop")
save_tab(stab, "15_stabilisation_week.csv")

cat("\n=========== STABILISATION WEEK (first week indistinguishable from wk11) ===========\n")
print(as.data.frame(stab |> mutate(rho_final = round(rho_final, 2))), row.names = FALSE)

# ---- Figure: weekly rho with bootstrap ribbon + stabilisation marker --------
p <- res |>
  ggplot(aes(week, rho, colour = condition, fill = condition)) +
  geom_ribbon(aes(ymin = rho_lo, ymax = rho_hi), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.3) +
  geom_vline(data = stab |> filter(!is.na(stab_week)),
             aes(xintercept = stab_week, colour = condition),
             linetype = "dashed", linewidth = 0.6, show.legend = FALSE) +
  facet_wrap(~ paste0(module, " ", year_label, " (", condition, ")"), ncol = 5) +
  scale_colour_manual(values = CONDITION_COLOURS, drop = FALSE) +
  scale_fill_manual(values = CONDITION_COLOURS, drop = FALSE) +
  scale_x_continuous(breaks = seq(1, 11, 2)) +
  labs(title = "When does cumulative engagement stabilise as a grade signal?",
       subtitle = "Weekly Spearman rho with bootstrap 95% band; dashed line = stabilisation week (CI of rho_11 - rho_w includes 0)",
       x = "University week", y = expression(Spearman~rho), colour = "Condition") +
  theme_bw(base_size = 10) + theme(legend.position = "bottom")
save_fig(p, "15_stabilisation.png", width = 13, height = 5)

cat("\nSaved figure: 15_stabilisation.png\n")
cat("Saved tables: 15_stabilisation_bootstrap.csv, 15_stabilisation_week.csv\n")
