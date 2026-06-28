# =============================================================================
# 17_trajectory_models.R   (WP7: longitudinal trajectory modelling)
#
# Instead of clustering static summary features (script 10), we model the SHAPE
# of each student's weekly engagement trajectory with a latent-class mixed model
# (group-based trajectory modelling, lcmm::hlme). The outcome is within-cohort
# weekly z-scored IDF so trajectories describe relative engagement over time.
# Number of classes chosen by BIC; classes are then related to final grade
# (Kruskal-Wallis) and delivery condition (chi-square).
# =============================================================================

source("R/00_setup.R")
suppressMessages(library(lcmm))
set.seed(7)

model_df <- readRDS(file.path(TAB_DIR, "model_df.rds"))

traj <- model_df |>
  dplyr::select(cohort_id, condition, User, week, z_IDF_week, final_grade) |>
  mutate(week_c = week - 6) |>
  arrange(cohort_id, User, week)

# numeric subject id required by lcmm
ids <- traj |> distinct(cohort_id, User) |> mutate(id = dplyr::row_number())
traj <- traj |> left_join(ids, by = c("cohort_id", "User"))

# ---- Fit ng = 1..4 (quadratic group trajectories); pick by BIC -------------
m1 <- hlme(z_IDF_week ~ week_c + I(week_c^2), subject = "id", ng = 1, data = traj)

fit_k <- function(k) {
  if (k == 1) return(m1)
  suppressWarnings(
    gridsearch(rep = 6, maxiter = 15, minit = m1,
               hlme(z_IDF_week ~ week_c + I(week_c^2),
                    mixture = ~ week_c + I(week_c^2),
                    subject = "id", ng = k, data = traj))
  )
}

models <- list(m1)
for (k in 2:3) models[[k]] <- tryCatch(fit_k(k), error = function(e) NULL)

bic_tbl <- purrr::imap_dfr(models, function(m, k) {
  if (is.null(m) || m$conv != 1) return(tibble(ng = k, BIC = NA, converged = FALSE))
  tibble(ng = k, BIC = m$BIC, converged = TRUE)
})
save_tab(bic_tbl, "17_trajectory_bic.csv")
cat("\n=========== TRAJECTORY MODEL SELECTION (BIC) ===========\n")
print(as.data.frame(bic_tbl), row.names = FALSE)

best_k <- bic_tbl |> filter(converged) |> slice_min(BIC, n = 1) |> pull(ng)
best_k <- max(best_k, 2)             # prefer >=2 classes for interpretation
best <- models[[best_k]]
cat(sprintf("Chosen number of classes: %d\n", best_k))

# ---- Assign classes and label by mean level/slope --------------------------
cls <- best$pprob |> dplyr::select(id, class) |>
  left_join(ids, by = "id") |>
  left_join(traj |> distinct(cohort_id, User, condition, final_grade),
            by = c("cohort_id", "User"))

shape <- traj |>
  left_join(best$pprob |> dplyr::select(id, class), by = "id") |>
  group_by(class, week) |>
  summarise(mean_z = mean(z_IDF_week), .groups = "drop")

# human-readable labels from early vs late mean level
lab_tbl <- shape |>
  group_by(class) |>
  summarise(early = mean(mean_z[week <= 4]),
            late  = mean(mean_z[week >= 8]),
            overall = mean(mean_z), .groups = "drop") |>
  mutate(label = dplyr::case_when(
    overall > 0.3 ~ "High-steady",
    overall < -0.3 ~ "Low-steady",
    late - early > 0.3 ~ "Rising",
    early - late > 0.3 ~ "Declining",
    TRUE ~ "Mid-variable"),
    label = paste0("C", class, ": ", label))

cls <- cls |> left_join(lab_tbl |> dplyr::select(class, label), by = "class")
shape <- shape |> left_join(lab_tbl |> dplyr::select(class, label), by = "class")
save_tab(cls |> dplyr::select(cohort_id, User, condition, class, label, final_grade),
         "17_trajectory_classes.csv")

# ---- Relate classes to grade and condition ---------------------------------
kw <- kruskal.test(final_grade ~ factor(class), data = cls)
grade_by_class <- cls |> group_by(label) |>
  summarise(n = n(), mean_grade = round(mean(final_grade), 1),
            pct_low = round(100 * mean(final_grade < 50), 1), .groups = "drop")
save_tab(grade_by_class, "17_grade_by_class.csv")

tab_cc <- table(cls$label, cls$condition)
chisq <- suppressWarnings(chisq.test(tab_cc))
comp <- as.data.frame(prop.table(tab_cc, 2)) |>
  setNames(c("class", "condition", "prop"))
save_tab(comp, "17_class_by_condition.csv")

cat("\n=========== GRADE BY TRAJECTORY CLASS ===========\n")
print(as.data.frame(grade_by_class), row.names = FALSE)
cat(sprintf("\nKruskal-Wallis grade ~ class: chi2 = %.1f, df = %d, p = %.3g\n",
            kw$statistic, kw$parameter, kw$p.value))
cat(sprintf("Chi-square class ~ condition: chi2 = %.1f, df = %d, p = %.3g\n",
            chisq$statistic, chisq$parameter, chisq$p.value))

# ---- Figures ---------------------------------------------------------------
p_tr <- shape |>
  ggplot(aes(week, mean_z, colour = label)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_line(linewidth = 1) + geom_point(size = 1.4) +
  scale_x_continuous(breaks = seq(1, 11, 2)) +
  labs(title = sprintf("Latent engagement-trajectory classes (k = %d)", best_k),
       subtitle = "Mean within-cohort z-scored weekly IDF per class",
       x = "University week", y = "Mean standardised weekly engagement",
       colour = "Class") +
  theme_bw(base_size = 11) + theme(legend.position = "bottom")
save_fig(p_tr, "17_trajectory_shapes.png", width = 9, height = 5)

p_g <- cls |>
  ggplot(aes(label, final_grade, fill = label)) +
  geom_hline(yintercept = 50, linetype = "dashed", colour = "grey30") +
  geom_boxplot(alpha = 0.8, outlier.size = 0.5) +
  labs(title = "Final grade by engagement-trajectory class",
       x = NULL, y = "Final grade (%)") +
  theme_bw(base_size = 10) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 1))
save_fig(p_g, "17_trajectory_grade.png", width = 8, height = 5)

p_c <- comp |>
  ggplot(aes(condition, prop, fill = class)) +
  geom_col() + scale_y_continuous(labels = scales::percent) +
  labs(title = "Trajectory-class composition by delivery condition",
       x = "Delivery condition", y = "Share of students", fill = "Class") +
  theme_bw(base_size = 10)
save_fig(p_c, "17_class_composition.png", width = 8, height = 5)

cat("\nSaved figures: 17_trajectory_shapes.png, 17_trajectory_grade.png, 17_class_composition.png\n")
cat("Saved tables: 17_trajectory_bic.csv, 17_trajectory_classes.csv,",
    "17_grade_by_class.csv, 17_class_by_condition.csv\n")
