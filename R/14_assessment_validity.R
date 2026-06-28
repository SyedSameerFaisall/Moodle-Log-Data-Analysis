# =============================================================================
# 14_assessment_validity.R   (WP4: engagement vs assessment components)
#
# The paper hypothesises that behavioural (VLE) engagement tracks performance
# better on individual, exam-type assessment than on coursework/group work
# (where individual effort is obscured). We test this within cohorts that have
# a clear component split:
#   - STAT0002 2022-23: exam_grade vs participation_grade / peer_marking_grade
#   - STAT0004 2022-23: quiz_grade (individual) vs group_grade (group)
# Other cohorts only expose C1/C2/C3 marks of unknown type -> reported
# descriptively. Dependent (overlapping) correlations are compared with
# Steiger's test via cocor.
# =============================================================================

source("R/00_setup.R")
suppressMessages(library(cocor))

model_df <- readRDS(file.path(TAB_DIR, "model_df.rds"))
eng <- model_df |>
  filter(week == 11) |>
  dplyr::select(cohort_id, User, cum_IDF)

META <- c("module", "file_tag", "cohort_id", "condition")

# Pull each cohort's component marks and join engagement.
component_long <- purrr::pmap_dfr(list(COHORTS$module, COHORTS$file_tag),
  function(m, ft) {
    g <- load_cohort(m, ft)$grades
    comp_cols <- setdiff(names(g), c(META, "User", "final_grade"))
    comp_cols <- comp_cols[!grepl("Assessment\\.Name", comp_cols)]
    g |>
      dplyr::mutate(dplyr::across(dplyr::all_of(comp_cols), num_na)) |>
      dplyr::select(cohort_id, User, dplyr::all_of(comp_cols)) |>
      tidyr::pivot_longer(dplyr::all_of(comp_cols),
                          names_to = "component", values_to = "mark")
  }) |>
  inner_join(eng, by = c("cohort_id", "User")) |>
  filter(!is.na(mark), !is.na(cum_IDF))

# ---- Engagement-component correlations -------------------------------------
comp_corr <- component_long |>
  group_by(cohort_id, component) |>
  filter(n() >= 10, sd(mark) > 0) |>
  summarise(r_spearman = cor(cum_IDF, mark, method = "spearman"),
            r_pearson  = cor(cum_IDF, mark, method = "pearson"),
            n = n(), .groups = "drop") |>
  left_join(COHORTS |> dplyr::select(cohort_id, condition), by = "cohort_id")
save_tab(comp_corr, "14_component_correlations.csv")

cat("\n=========== ENGAGEMENT (cum IDF, wk11) vs COMPONENT MARKS ===========\n")
print(as.data.frame(comp_corr |>
  mutate(r_spearman = round(r_spearman, 2)) |>
  dplyr::select(cohort_id, component, r_spearman, n)), row.names = FALSE)

# ---- Dependent-correlation tests for the labelled contrasts ----------------
# Compare r(engagement, A) vs r(engagement, B) on the same students (Steiger).
test_contrast <- function(cohort, compA, compB) {
  w <- component_long |>
    filter(cohort_id == cohort, component %in% c(compA, compB)) |>
    tidyr::pivot_wider(names_from = component, values_from = mark) |>
    filter(!is.na(.data[[compA]]), !is.na(.data[[compB]]))
  if (nrow(w) < 15) return(NULL)
  rjk <- cor(w$cum_IDF, w[[compA]])          # engagement vs A
  rjh <- cor(w$cum_IDF, w[[compB]])          # engagement vs B
  rkh <- cor(w[[compA]], w[[compB]])         # A vs B
  ct <- cocor::cocor.dep.groups.overlap(rjk, rjh, rkh, n = nrow(w))
  st <- ct@steiger1980
  tibble(cohort_id = cohort, comp_A = compA, comp_B = compB,
         r_eng_A = rjk, r_eng_B = rjh, r_AB = rkh, n = nrow(w),
         steiger_z = st$statistic, steiger_p = st$p.value)
}

contrasts <- bind_rows(
  test_contrast("STAT0002_2223", "exam_grade", "participation_grade"),
  test_contrast("STAT0002_2223", "exam_grade", "peer_marking_grade"),
  test_contrast("STAT0004_2223", "quiz_grade", "group_grade")
)
if (!is.null(contrasts) && nrow(contrasts) > 0) {
  save_tab(contrasts, "14_dependent_corr_tests.csv")
  cat("\n=========== DEPENDENT-CORRELATION TESTS (Steiger 1980) ===========\n")
  print(as.data.frame(contrasts |>
    mutate(across(c(r_eng_A, r_eng_B, r_AB, steiger_z), \(x) round(x, 2)),
           steiger_p = signif(steiger_p, 2))), row.names = FALSE)
}

# ---- Figure ----------------------------------------------------------------
p_comp <- comp_corr |>
  mutate(component = forcats::fct_reorder(paste0(cohort_id, ": ", component), r_spearman)) |>
  ggplot(aes(r_spearman, component, fill = condition)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "grey60") +
  scale_fill_manual(values = CONDITION_COLOURS, drop = FALSE) +
  labs(title = "Engagement (cumulative IDF, week 11) vs assessment-component marks",
       subtitle = "Spearman correlation; tests whether engagement aligns more with exam/individual components",
       x = "Spearman correlation with cumulative IDF", y = NULL,
       fill = "Delivery condition") +
  theme_bw(base_size = 10) + theme(legend.position = "bottom")
save_fig(p_comp, "14_component_correlations.png", width = 9, height = 6)

cat("\nSaved figure: 14_component_correlations.png\n")
cat("Saved tables: 14_component_correlations.csv, 14_dependent_corr_tests.csv\n")
