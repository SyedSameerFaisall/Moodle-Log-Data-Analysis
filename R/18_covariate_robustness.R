# =============================================================================
# 18_covariate_robustness.R   (WP8: partial de-confounding + robustness)
#
# Four checks that strengthen internal validity:
#  (1) Programme adjustment - does the engagement-grade slope survive
#      controlling for degree programme (cohort composition)? (cohorts where
#      Programme.Code is available in dat).
#  (2) Extenuating-circumstances sensitivity - re-estimate excluding students
#      with EC.Minutes > 0.
#  (3) Multiple-comparison control - Benjamini-Hochberg across the full weekly
#      correlation family.
#  (4) Threshold robustness - early-warning recall/precision at 10/20/30%
#      flags and cumulative vs per-week engagement.
# =============================================================================

source("R/00_setup.R")
suppressMessages(library(pROC))

model_df <- readRDS(file.path(TAB_DIR, "model_df.rds"))

# ---- Covariates from dat (present only in some cohorts) ---------------------
get_covars <- function(module, file_tag) {
  dat <- load_cohort(module, file_tag)$dat
  have <- intersect(c("Programme.Code", "Route.Code", "EC.Minutes"), names(dat))
  if (length(have) == 0) return(NULL)
  dat |>
    dplyr::select(User, dplyr::all_of(have)) |>
    dplyr::group_by(User) |>
    dplyr::summarise(dplyr::across(dplyr::everything(),
                                   \(x) x[which(!is.na(x) & x != "NA")][1]),
                     .groups = "drop") |>
    dplyr::mutate(cohort_id = paste(module, file_tag, sep = "_"),
                  EC.Minutes = if ("EC.Minutes" %in% have) num_na(EC.Minutes) else NA)
}
covars <- purrr::pmap(list(COHORTS$module, COHORTS$file_tag), get_covars) |>
  purrr::compact() |> purrr::list_rbind()
cohorts_with_cov <- unique(covars$cohort_id)
cat("Cohorts with programme/EC covariates:", paste(cohorts_with_cov, collapse = ", "), "\n")

d11 <- model_df |> filter(week == 11) |>
  left_join(covars, by = c("cohort_id", "User"))

# ---- (1) Programme adjustment ----------------------------------------------
prog_adj <- purrr::map_dfr(cohorts_with_cov, function(ci) {
  d <- d11 |> filter(cohort_id == ci, !is.na(Programme.Code)) |>
    mutate(prog = forcats::fct_lump_min(factor(Programme.Code), 10))
  if (n_distinct(d$prog) < 2) return(NULL)
  m0 <- lm(final_grade ~ z_cum_div, d)
  m1 <- lm(final_grade ~ z_cum_div + prog, d)
  tibble(cohort_id = ci,
         slope_unadj = coef(m0)["z_cum_div"],
         slope_adj   = coef(m1)["z_cum_div"],
         pct_attenuation = 100 * (coef(m0)["z_cum_div"] - coef(m1)["z_cum_div"]) /
           coef(m0)["z_cum_div"],
         n_programmes = n_distinct(d$prog), n = nrow(d))
})
save_tab(prog_adj, "18_programme_adjustment.csv")
cat("\n=========== (1) PROGRAMME-ADJUSTED DIVERSITY SLOPE (week 11) ===========\n")
print(as.data.frame(prog_adj |> mutate(across(where(is.numeric), \(x) round(x, 2)))),
      row.names = FALSE)

# ---- (2) EC sensitivity -----------------------------------------------------
ec_sens <- purrr::map_dfr(cohorts_with_cov, function(ci) {
  d <- d11 |> filter(cohort_id == ci)
  full <- cor(d$cum_IDF, d$final_grade, method = "spearman")
  d2 <- d |> filter(is.na(EC.Minutes) | EC.Minutes == 0)
  excl <- cor(d2$cum_IDF, d2$final_grade, method = "spearman")
  tibble(cohort_id = ci, n_all = nrow(d), n_excl_EC = nrow(d2),
         rho_all = full, rho_excl_EC = excl)
})
save_tab(ec_sens, "18_ec_sensitivity.csv")
cat("\n=========== (2) EC.Minutes>0 EXCLUSION SENSITIVITY ===========\n")
print(as.data.frame(ec_sens |> mutate(across(c(rho_all, rho_excl_EC), \(x) round(x, 3)))),
      row.names = FALSE)

# ---- (3) BH across the full weekly correlation family ----------------------
ind_map <- c(Frequency = "cum_freq", Immediacy = "cum_imm",
             Diversity = "cum_div", IDF = "cum_IDF")
fam <- purrr::imap_dfr(ind_map, function(col, nm) {
  model_df |> group_by(cohort_id, week) |>
    summarise(p = suppressWarnings(cor.test(.data[[col]], final_grade,
                                            method = "spearman")$p.value),
              rho = cor(.data[[col]], final_grade, method = "spearman"),
              .groups = "drop") |>
    mutate(indicator = nm)
}) |>
  mutate(p_BH = p.adjust(p, method = "BH"))
save_tab(fam, "18_weekly_family_BH.csv")
cat("\n=========== (3) MULTIPLE-COMPARISON CONTROL (whole weekly family) ===========\n")
cat(sprintf("Tests in family: %d | significant raw p<.05: %d | significant BH<.05: %d\n",
            nrow(fam), sum(fam$p < 0.05), sum(fam$p_BH < 0.05)))

# ---- (4) Threshold robustness of the early-warning flag --------------------
thr_rob <- purrr::map_dfr(c(0.10, 0.20, 0.30), function(thr) {
  model_df |> filter(week == 6) |>
    group_by(cohort_id, condition) |>
    mutate(flag_cum = cum_IDF <= quantile(cum_IDF, thr),
           flag_wk  = IDF_week <= quantile(IDF_week, thr)) |>
    summarise(threshold = thr,
              recall_cum = ifelse(sum(low) > 0, mean(flag_cum[low]), NA),
              recall_wk  = ifelse(sum(low) > 0, mean(flag_wk[low]),  NA),
              prec_cum   = ifelse(sum(flag_cum) > 0, mean(low[flag_cum]), NA),
              .groups = "drop")
})
save_tab(thr_rob, "18_threshold_robustness.csv")
cat("\n=========== (4) THRESHOLD ROBUSTNESS (week 6 recall, cumulative flag) ===========\n")
print(as.data.frame(
  thr_rob |> dplyr::select(cohort_id, threshold, recall_cum) |>
    mutate(recall_cum = round(recall_cum, 2)) |>
    tidyr::pivot_wider(names_from = threshold, values_from = recall_cum,
                       names_prefix = "thr_")
), row.names = FALSE)

cat("\nSaved tables: 18_programme_adjustment.csv, 18_ec_sensitivity.csv,",
    "18_weekly_family_BH.csv, 18_threshold_robustness.csv\n")
