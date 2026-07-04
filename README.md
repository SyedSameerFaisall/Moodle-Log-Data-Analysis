# Moodle Log Data Research

Secondary validation and extension of a **chapter-aligned Moodle engagement metric** (Johnston et al., 2025) across five UCL statistics cohorts: **STAT0002** and **STAT0004**, spanning online, hybrid, and in-person delivery periods.

The project builds a reproducible R pipeline from raw Moodle logs to a LaTeX research report, with two headline research questions:

1. **RQ1 — Which engagement components matter most?** Frequency, Immediacy, and Diversity are decomposed with LMG relative-importance analysis under collinearity.
2. **RQ2 — Does engagement affect all students equally?** Quantile regression tests whether the engagement–grade association is stronger among lower-performing students.

**Headline findings:** pooled engagement–grade *r* ≈ 0.32; Diversity and Immediacy carry more unique signal than Frequency; lower-tail grade slopes exceed median slopes in every cohort.

---

## Repository structure

```
Moodle Log Data Research/
├── Data/                    # Raw cohort files (*_data.RData, companion .xlsx)
├── R/                       # Analysis pipeline (run in order via run_all.R)
│   ├── 00_setup.R           # Paths, cohort registry, loaders
│   ├── features.R           # Feature-engineering functions
│   ├── 01–05                # Verify data → build model_df.rds
│   ├── 06–10                # Replication, interaction, early warning
│   ├── 11–19                # Advanced analyses + synthesis table
│   └── run_all.R              # End-to-end orchestrator
├── outputs/
│   ├── figures/             # Plots embedded in the report
│   └── tables/              # CSV/RDS outputs (model_df.rds is the hub)
├── report/
│   ├── report.tex           # LaTeX source
│   └── report.pdf           # Compiled report
├── docs/                    # Pipeline flowchart (PNG/SVG/Mermaid)
├── understanding.md         # Step-by-step guide to every R script
└── required_packages.R      # Core tidyverse packages
```

---

## Data

Each cohort is stored as `Data/{MODULE}_{YEAR}_data.RData` containing three objects:

| Object | Contents |
|--------|----------|
| `dat` | Event-level Moodle log |
| `grades` | Final grades and assessment components |
| `sem` | Weekly chapter-level Frequency / Immediacy / Diversity contributions |

| Cohort ID | Module | Year | Delivery |
|-----------|--------|------|----------|
| `STAT0002_2021` | STAT0002 | 2020–21 | online |
| `STAT0002_2223` | STAT0002 | 2022–23 | in_person |
| `STAT0004_2021` | STAT0004 | 2020–21 | online |
| `STAT0004_2122` | STAT0004 | 2021–22 | hybrid |
| `STAT0004_2223` | STAT0004 | 2022–23 | in_person |

Analysis sample: **N = 1,291** students with valid engagement records and `final_grade > 0`.

---

## Requirements

- **R** ≥ 4.2 (tested on 4.4.3)
- **LaTeX** (TeX Live) with `pdflatex` — to rebuild `report/report.pdf`
- **R packages:**

```r
# Core (see required_packages.R)
install.packages(c(
  "dplyr", "tidyr", "ggplot2", "stringr", "purrr", "readr",
  "tibble", "forcats", "lubridate"
))

# Analysis extensions
install.packages(c(
  "broom", "betareg", "quantreg", "relaimpo", "car", "emmeans",
  "lme4", "lmerTest", "metafor", "cocor", "pROC", "cluster", "lcmm"
))
```

---

## Quick start

Run the full pipeline from the **project root**:

```powershell
cd "path\to\Moodle Log Data Research"
& "C:\Program Files\R\R-4.4.3\bin\Rscript.exe" R/run_all.R
```

Or from inside R:

```r
setwd("path/to/Moodle Log Data Research")
source("R/run_all.R")
```

### Run headline analyses only

After `04_build_features.R` and `05_build_model_df.R` have produced `outputs/tables/model_df.rds`:

```powershell
Rscript R/13_indicator_importance.R   # RQ1: LMG component importance
Rscript R/11_outcome_models.R         # RQ2: quantile regression
Rscript R/19_synthesis.R              # Consolidated findings table
```

### Rebuild the report

```powershell
cd report
pdflatex -interaction=nonstopmode report.tex
pdflatex -interaction=nonstopmode report.tex
```

Figures are read from `../outputs/figures/`.

---

## Pipeline overview

![Pipeline flowchart](docs/pipeline-flow.png)

| Stage | Scripts | Purpose |
|-------|---------|---------|
| Verify | `01`–`03` | Metric semantics, schema checks, cohort inventory |
| Features | `04`–`05` | Weekly/cumulative F/I/D → **`model_df.rds`** |
| Replicate | `06`–`10` | Paper replication, interactions, early-warning flags |
| **RQ1** | **`13`** | LMG relative importance (weeks 6 & 11) |
| **RQ2** | **`11`** | Quantile regression + bootstrap tail contrasts |
| Pool | `12` | Mixed-effects models + random-effects meta-analysis |
| Diagnostics | `14`–`18` | Assessment validity, trajectories, robustness |
| Synthesis | `19` | `19_results_synthesis.csv` — one-row-per-question summary |

For a detailed walkthrough of every script, see **[understanding.md](understanding.md)**.

---

## Key outputs

| Output | Description |
|--------|-------------|
| `outputs/tables/model_df.rds` | Golden modelling table (student × week) |
| `outputs/tables/13_lmg_importance.csv` | RQ1: component importance shares |
| `outputs/tables/11_quantile_idf.csv` | RQ2: quantile-regression slopes |
| `outputs/tables/11_tail_contrast_bootstrap.csv` | RQ2: τ₀.₁ − τ₀.₅ bootstrap CIs |
| `outputs/tables/19_results_synthesis.csv` | Plain-language verdicts for all work packages |
| `report/report.pdf` | Full research report |

---

## Report

The written report is in [`report/report.tex`](report/report.tex) (compiled PDF: [`report/report.pdf`](report/report.pdf)).

Results are organised as:

- **§4.1** — Overall pooled engagement–grade association
- **§4.2** — RQ1: which components matter (LMG)
- **§4.3** — RQ2: heterogeneity across the grade distribution (quantile regression)
- **§4.4–4.6** — Trajectories, early-warning limits, robustness

See [`report/README.md`](report/README.md) for LaTeX build notes.

---

## Design notes

- **Delivery confounding:** online / hybrid / in-person labels align with academic years; cross-condition comparisons are descriptive, not causal.
- **Engagement metric:** F, I, D contributions in `sem` are per-week increments; cumulative totals are computed in the pipeline.
- **Exclusions:** students with `final_grade = 0` (typically exam absence) are dropped from modelling.

---

## References

- Johnston, L. J., Griffin, J. E., Manolopoulou, I., & Jendoubi, T. (2025). A real-time metric of online engagement monitoring. *arXiv:2507.12162*.
- Chong, S., & Wong, A. (2019). Proxy variables of online engagement on the learning management system.

---

## License and data

Moodle log data are subject to institutional access restrictions and are not redistributable without UCL approval. Code and report text in this repository are provided for academic research reproducibility within those constraints.
