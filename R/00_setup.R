# =============================================================================
# 00_setup.R
# Shared configuration, libraries, paths and loaders for the
# UCL Moodle Engagement Research Masterplan.
#
# Every analysis script begins with:  source("R/00_setup.R")
# =============================================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(purrr)
  library(readr)
  library(tibble)
  library(forcats)
})

# ---- Paths -----------------------------------------------------------------
# Scripts are run from the project root. The raw data live in the Data/ folder
# (the *_data.RData files); adjust DATA_DIR if the data live elsewhere.
DATA_DIR <- file.path(getwd(), "Data")
FIG_DIR  <- file.path("outputs", "figures")
TAB_DIR  <- file.path("outputs", "tables")
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TAB_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- Registry of the available module-years --------------------------------
# NOTE: STAT0002 has NO 2021-22 (hybrid) year. Only STAT0004 spans all three
# delivery conditions. file_tag is the string used in the *_data.RData names.
COHORTS <- tibble::tribble(
  ~module,     ~file_tag, ~condition,   ~year_label,
  "STAT0002",  "2021",    "online",     "2020-21",
  "STAT0002",  "2223",    "in_person",  "2022-23",
  "STAT0004",  "2021",    "online",     "2020-21",
  "STAT0004",  "2122",    "hybrid",     "2021-22",
  "STAT0004",  "2223",    "in_person",  "2022-23"
) |>
  dplyr::mutate(
    cohort_id = paste(module, file_tag, sep = "_"),
    condition = factor(condition, levels = c("online", "hybrid", "in_person"))
  )

CONDITION_LEVELS <- c("online", "hybrid", "in_person")
CONDITION_COLOURS <- c(online = "#1b9e77", hybrid = "#d95f02", in_person = "#7570b3")

# ---- Loader ----------------------------------------------------------------
# Each *_data.RData contains three objects named dat, grades, sem.
# Load one cohort into an isolated environment and return a named list,
# tagging every table with module / file_tag / condition.
load_cohort <- function(module, file_tag, data_dir = DATA_DIR) {
  path <- file.path(data_dir, sprintf("%s_%s_data.RData", module, file_tag))
  if (!file.exists(path)) stop("File not found: ", path)

  e <- new.env()
  load(path, envir = e)
  meta <- COHORTS |> dplyr::filter(module == !!module, file_tag == !!file_tag)

  tag <- function(df) {
    df |>
      dplyr::mutate(
        module    = module,
        file_tag  = file_tag,
        cohort_id = meta$cohort_id,
        condition = factor(as.character(meta$condition), levels = CONDITION_LEVELS),
        .before = 1
      )
  }

  list(
    dat    = tag(tibble::as_tibble(e$dat)),
    grades = tag(tibble::as_tibble(e$grades)),
    sem    = tag(tibble::as_tibble(e$sem))
  )
}

# Convenience: load every cohort's `which` table and row-bind compatible columns.
load_all <- function(which = c("sem", "grades"), data_dir = DATA_DIR) {
  which <- match.arg(which)
  purrr::pmap(
    list(COHORTS$module, COHORTS$file_tag),
    function(m, ft) load_cohort(m, ft, data_dir)[[which]]
  ) |>
    purrr::list_rbind()
}

# ---- Small helpers ---------------------------------------------------------
# Moodle exports store missing values as the literal string "NA"; coerce safely.
num_na <- function(x) suppressWarnings(as.numeric(dplyr::na_if(as.character(x), "NA")))

save_tab <- function(df, name) {
  readr::write_csv(df, file.path(TAB_DIR, name))
  invisible(df)
}

save_fig <- function(plot, name, width = 9, height = 6, dpi = 150) {
  ggplot2::ggsave(file.path(FIG_DIR, name), plot, width = width,
                  height = height, dpi = dpi)
  invisible(plot)
}

message("00_setup.R loaded. Cohorts available:")
print(COHORTS[, c("cohort_id", "condition", "year_label")])
