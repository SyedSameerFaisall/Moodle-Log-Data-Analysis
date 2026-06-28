# =============================================================================
# run_all.R
# Run the full engagement-analysis pipeline end to end. From the project root:
#   "C:/Program Files/R/R-4.4.3/bin/Rscript.exe" R/run_all.R
# Each step writes figures to outputs/figures and tables to outputs/tables.
# =============================================================================

steps <- c(
  "R/01_verify_metric.R",       # confirm sem contributions are per-week
  "R/02_check_schema.R",        # loaders + schema consistency
  "R/03_data_inventory.R",      # cohort sizes, grades, overlap
  "R/04_build_features.R",      # weekly / cumulative / phase / profile features
  "R/05_build_model_df.R",      # join grades, exclude absences, standardise
  "R/06_replicate_paper.R",     # weekly correlations + quintile boxplots
  "R/07_interaction.R",         # engagement x condition interaction (STAT0004)
  "R/08_early_warning.R",       # bottom-quintile recall / precision / AUC
  "R/09_stat0002_replication.R",# STAT0002 + cross-module replication
  "R/10_profiles_clustering.R", # behaviour profiles + PAM clustering
  # ---- Advanced statistical extensions (WP1-WP8) ----
  "R/11_outcome_models.R",      # WP1 bounded-outcome models + quantile tail effects
  "R/12_pooled_mixed_meta.R",   # WP2 mixed-effects + random-effects meta-analysis
  "R/13_indicator_importance.R",# WP3 collinearity + LMG relative importance
  "R/14_assessment_validity.R", # WP4 engagement vs assessment components (cocor)
  "R/15_timing_stabilisation.R",# WP5 bootstrap stabilisation week
  "R/16_eventlog_temporal.R",   # WP6 timing/regularity features + incremental value
  "R/17_trajectory_models.R",   # WP7 latent-class engagement trajectories (lcmm)
  "R/18_covariate_robustness.R",# WP8 programme adjustment + BH + robustness
  "R/19_synthesis.R"            # consolidated results-synthesis table
)

for (s in steps) {
  message("\n========================= RUNNING: ", s, " =========================")
  source(s, echo = FALSE)
}
message("\nPipeline complete. See outputs/figures and outputs/tables.")
