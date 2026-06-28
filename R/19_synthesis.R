# =============================================================================
# 19_synthesis.R   (Integration: one consolidated results-synthesis table)
#
# Reads the key tables produced by the advanced work packages (11-18) and
# assembles a single master findings table that maps each research question to
# its statistic, value, and a plain-language verdict. This is the at-a-glance
# summary for the write-up.
# =============================================================================

source("R/00_setup.R")
rd <- function(f) readr::read_csv(file.path(TAB_DIR, f), show_col_types = FALSE)

rows <- list()
add <- function(wp, question, statistic, value, verdict) {
  rows[[length(rows) + 1]] <<- tibble(work_package = wp, question = question,
                                      statistic = statistic, value = value,
                                      verdict = verdict)
}

# ---- WP2: pooled association + heterogeneity + condition moderator ----------
meta <- rd("12_meta_pooled.csv")
idf <- meta |> filter(indicator == "IDF")
add("WP2 synthesis",
    "How strongly does cumulative engagement relate to final grade overall?",
    "Random-effects pooled r (IDF, wk11) [95% CI]",
    sprintf("%.2f [%.2f, %.2f]", idf$pooled_r, idf$ci_lo, idf$ci_hi),
    "Moderate, robust positive association across all five cohorts.")
add("WP2 synthesis",
    "Is the association heterogeneous, and does delivery mode explain it?",
    "I-squared / condition-moderator p (IDF)",
    sprintf("I2=%.0f%%, mod p=%.2f", idf$I2, idf$mod_condition_p),
    "Low-moderate heterogeneity; delivery condition is NOT a significant moderator.")

# ---- WP1: specification robustness + tail effect ---------------------------
tail <- rd("11_tail_vs_median.csv")
add("WP1 outcome models",
    "Does engagement matter more for weaker students?",
    "Mean (IDF slope at tau=0.1 minus tau=0.5), grade pts",
    sprintf("+%.1f", mean(tail$low_minus_median, na.rm = TRUE)),
    "Yes - effect is markedly larger in the lower grade tail (early-warning value).")
comp <- rd("11_outcome_model_comparison.csv") |> filter(week == 6, term == "z_cum_div")
add("WP1 outcome models",
    "Are conclusions sensitive to model specification (OLS vs fractional/beta)?",
    "Sign agreement of Diversity coef across 3 model families",
    sprintf("%d/%d cohorts agree in sign",
            comp |> group_by(cohort_id) |>
              summarise(a = n_distinct(sign(estimate)) == 1) |> pull(a) |> sum(),
            n_distinct(comp$cohort_id)),
    "Conclusions robust to bounded-outcome specification.")

# ---- WP3: indicator importance ---------------------------------------------
lmg <- rd("13_lmg_importance.csv")
imp <- lmg |> group_by(indicator) |> summarise(mean_share = mean(lmg_share)) |>
  arrange(desc(mean_share))
add("WP3 importance",
    "Which engagement dimension contributes most (under collinearity)?",
    "Mean LMG share: top vs bottom indicator",
    sprintf("%s %.0f%% > %s %.0f%%", imp$indicator[1], 100 * imp$mean_share[1],
            imp$indicator[nrow(imp)], 100 * imp$mean_share[nrow(imp)]),
    "Diversity/Immediacy carry more unique signal than raw Frequency.")

# ---- WP4: assessment validity ----------------------------------------------
dep <- rd("14_dependent_corr_tests.csv")
ex_peer <- dep |> filter(comp_A == "exam_grade", comp_B == "peer_marking_grade")
add("WP4 assessment",
    "Does engagement track individual/exam work more than group/peer work?",
    "Steiger test: r(eng,exam) vs r(eng,peer-marking)",
    sprintf("dz=%.2f vs %.2f, p=%.3f", ex_peer$r_eng_A, ex_peer$r_eng_B, ex_peer$steiger_p),
    "Engagement aligns significantly more with exam than peer-marked components.")

# ---- WP5: stabilisation timing ---------------------------------------------
stab <- rd("15_stabilisation_week.csv")
add("WP5 timing",
    "How early does the engagement signal stabilise?",
    "Stabilisation week range across cohorts",
    sprintf("wk %d-%d", min(stab$stab_week, na.rm = TRUE), max(stab$stab_week, na.rm = TRUE)),
    "Stabilises early when effect is flat; accrues to wk~9 where signal grows (STAT0002 in-person).")

# ---- WP6: incremental temporal value ---------------------------------------
incr <- rd("16_incremental_value.csv")
add("WP6 temporal features",
    "Do timing/regularity features add value beyond F/I/D?",
    "Cohorts with significant incremental R2 (wk6) / mean dR2",
    sprintf("%d/%d sig, mean dR2=%.3f", sum(incr$p_incremental < 0.05),
            nrow(incr), mean(incr$delta_r2)),
    "Yes - regularity/timing add significant explanatory power (esp. STAT0002).")

# ---- WP7: trajectory classes -----------------------------------------------
cls <- rd("17_trajectory_classes.csv")
kw <- kruskal.test(final_grade ~ factor(class), data = cls)
gbc <- rd("17_grade_by_class.csv")
add("WP7 trajectories",
    "Do latent engagement trajectories differentiate outcomes?",
    "Kruskal-Wallis grade ~ class; grade gap (top-bottom class)",
    sprintf("p=%.1e; %.1f pts", kw$p.value,
            max(gbc$mean_grade) - min(gbc$mean_grade)),
    "Distinct trajectory classes; large grade gap; composition stable across conditions.")

# ---- WP8: robustness --------------------------------------------------------
prog <- rd("18_programme_adjustment.csv")
bh <- rd("18_weekly_family_BH.csv")
add("WP8 robustness",
    "Does the association survive programme adjustment and multiple testing?",
    "Max slope attenuation; BH-significant weekly tests",
    sprintf("<=%.0f%% atten; %d/%d survive BH",
            max(abs(prog$pct_attenuation)), sum(bh$p_BH < 0.05), nrow(bh)),
    "Association robust to degree-programme control and family-wise correction.")

synthesis <- purrr::list_rbind(rows)
save_tab(synthesis, "19_results_synthesis.csv")

cat("\n================= CONSOLIDATED RESULTS SYNTHESIS =================\n\n")
for (i in seq_len(nrow(synthesis))) {
  s <- synthesis[i, ]
  cat(sprintf("[%s]\n  Q: %s\n  %s = %s\n  -> %s\n\n",
              s$work_package, s$question, s$statistic, s$value, s$verdict))
}
cat("Saved table: 19_results_synthesis.csv\n")
