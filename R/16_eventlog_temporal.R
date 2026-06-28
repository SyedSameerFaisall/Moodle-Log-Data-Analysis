# =============================================================================
# 16_eventlog_temporal.R   (WP6: richer temporal features from the event log)
#
# The provided F/I/D metric captures volume, promptness and breadth but not the
# REGULARITY/TIMING of study (which Hoffman et al. and others find adds value).
# We engineer timing features directly from `dat` (cumulatively up to the
# decision week 6) and test whether they add predictive value beyond F/I/D:
#   - week_entropy   : evenness of activity across weeks (regularity)
#   - gap_sd         : SD of gaps between active days (spacing irregularity)
#   - late_night_frac: share of activity 00:00-06:00
#   - breadth_comp   : distinct Moodle Components touched
#   - time_proxy_min : capped time-on-task proxy from inter-event gaps
#   - cramming_frac  : share of chapter events accessed >2 weeks after release
#
# Tests: nested-model F-test for incremental R2 on final grade, and change in
# early-warning AUC for predicting low performance (<50). In-sample AUC is
# reported with the usual optimism caveat; the nested F-test is the rigorous
# incremental-value test.
# =============================================================================

source("R/00_setup.R")
suppressMessages(library(pROC))

W <- 6  # decision week (real-time, mid-term)

entropy_norm <- function(counts) {
  p <- counts[counts > 0]; if (length(p) <= 1) return(0)
  p <- p / sum(p); -sum(p * log(p)) / log(length(counts))
}

temporal_features <- function(module, file_tag) {
  dat <- load_cohort(module, file_tag)$dat |>
    mutate(univ_week = as.integer(univ_week),
           hour = suppressWarnings(as.integer(substr(as.character(time), 1, 2))),
           chap = num_na(session_chap))

  # chapter release week (earliest access across all students, full term)
  release <- dat |> filter(!is.na(chap)) |>
    group_by(chap) |> summarise(rel = min(univ_week, na.rm = TRUE), .groups = "drop")

  d <- dat |> filter(!is.na(univ_week), univ_week >= 1, univ_week <= W)

  by_user <- d |>
    group_by(User) |>
    summarise(
      n_events     = n(),
      n_sessions   = n_distinct(Session_ID),
      n_active_days= n_distinct(date),
      time_proxy_min = sum(pmin(timediff, 1800), na.rm = TRUE) / 60,
      late_night_frac = mean(hour < 6, na.rm = TRUE),
      breadth_comp = n_distinct(Component),
      week_entropy = entropy_norm(tabulate(univ_week, nbins = W)),
      gap_sd = {
        ds <- sort(unique(date))
        if (length(ds) < 3) 0 else sd(as.numeric(diff(ds)))
      },
      .groups = "drop"
    )

  cram <- d |> filter(!is.na(chap)) |>
    left_join(release, by = "chap") |>
    group_by(User) |>
    summarise(cramming_frac = mean((univ_week - rel) > 2, na.rm = TRUE),
              .groups = "drop")

  by_user |>
    left_join(cram, by = "User") |>
    mutate(cramming_frac = tidyr::replace_na(cramming_frac, 0),
           cohort_id = paste(module, file_tag, sep = "_"))
}

feats <- purrr::pmap_dfr(list(COHORTS$module, COHORTS$file_tag), temporal_features)
saveRDS(feats, file.path(TAB_DIR, "eventlog_features.rds"))

new_feats <- c("week_entropy", "gap_sd", "late_night_frac",
               "breadth_comp", "time_proxy_min", "cramming_frac")

# ---- Merge with week-6 engagement + outcome --------------------------------
base6 <- readRDS(file.path(TAB_DIR, "model_df.rds")) |>
  filter(week == W) |>
  dplyr::select(cohort_id, condition, User, final_grade, low,
                z_cum_freq, z_cum_imm, z_cum_div)

dd <- base6 |>
  inner_join(feats, by = c("cohort_id", "User")) |>
  group_by(cohort_id) |>
  mutate(across(all_of(new_feats), \(x) {
    s <- sd(x, na.rm = TRUE); if (is.na(s) || s == 0) 0 else as.numeric(scale(x))
  }, .names = "z_{.col}")) |>
  ungroup()

z_new <- paste0("z_", new_feats)

# ---- Incremental value: nested F-test per cohort + pooled ------------------
incr_one <- function(df) {
  base <- lm(final_grade ~ z_cum_freq + z_cum_imm + z_cum_div, df)
  full <- lm(reformulate(c("z_cum_freq", "z_cum_imm", "z_cum_div", z_new),
                         "final_grade"), df)
  a <- anova(base, full)
  bm <- glm(low ~ z_cum_freq + z_cum_imm + z_cum_div, df, family = binomial)
  fm <- glm(reformulate(c("z_cum_freq", "z_cum_imm", "z_cum_div", z_new), "low"),
            df, family = binomial)
  auc_b <- as.numeric(pROC::auc(pROC::roc(df$low, predict(bm), quiet = TRUE)))
  auc_f <- as.numeric(pROC::auc(pROC::roc(df$low, predict(fm), quiet = TRUE)))
  tibble(r2_base = summary(base)$r.squared,
         r2_full = summary(full)$r.squared,
         delta_r2 = summary(full)$r.squared - summary(base)$r.squared,
         F = a$F[2], p_incremental = a$`Pr(>F)`[2],
         auc_base = auc_b, auc_full = auc_f, auc_delta = auc_f - auc_b)
}

incr <- dd |> group_by(cohort_id, condition) |>
  group_modify(~ incr_one(.x)) |> ungroup()

# pooled with cohort fixed effects
pool_base <- lm(reformulate(c("cohort_id", "z_cum_freq", "z_cum_imm", "z_cum_div"),
                            "final_grade"), dd)
pool_full <- lm(reformulate(c("cohort_id", "z_cum_freq", "z_cum_imm", "z_cum_div", z_new),
                            "final_grade"), dd)
pool_a <- anova(pool_base, pool_full)

save_tab(incr, "16_incremental_value.csv")

cat("\n=========== INCREMENTAL VALUE of temporal features beyond F/I/D (week 6) ===========\n")
print(as.data.frame(incr |>
  mutate(across(c(delta_r2, auc_base, auc_full, auc_delta), \(x) round(x, 3)),
         F = round(F, 2), p_incremental = signif(p_incremental, 2))), row.names = FALSE)
cat(sprintf("\nPooled (cohort fixed effects): delta R2 = %.3f, F = %.2f, p = %.3g\n",
            summary(pool_full)$r.squared - summary(pool_base)$r.squared,
            pool_a$F[2], pool_a$`Pr(>F)`[2]))

# ---- Which temporal features correlate with grade (pooled, within-cohort) ---
feat_corr <- dd |>
  dplyr::select(cohort_id, final_grade, all_of(z_new)) |>
  tidyr::pivot_longer(all_of(z_new), names_to = "feature", values_to = "z") |>
  group_by(feature) |>
  summarise(r = cor(z, final_grade, method = "spearman"), .groups = "drop") |>
  mutate(feature = sub("^z_", "", feature))
save_tab(feat_corr, "16_temporal_feature_corr.csv")

p_feat <- feat_corr |>
  mutate(feature = forcats::fct_reorder(feature, r)) |>
  ggplot(aes(r, feature, fill = r > 0)) +
  geom_col() + geom_vline(xintercept = 0, colour = "grey50") +
  scale_fill_manual(values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02"),
                    guide = "none") +
  labs(title = "Temporal study-behaviour features vs final grade (pooled, within-cohort z)",
       subtitle = "Cumulative to week 6; positive = associated with higher grades",
       x = "Spearman correlation with final grade", y = NULL) +
  theme_bw(base_size = 11)
save_fig(p_feat, "16_temporal_feature_corr.png", width = 8, height = 5)

cat("\nSaved figure: 16_temporal_feature_corr.png\n")
cat("Saved tables: 16_incremental_value.csv, 16_temporal_feature_corr.csv\n")
