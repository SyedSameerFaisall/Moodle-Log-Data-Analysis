# Moodle Engagement Research Report

**Validating and Extending a Chapter-Aligned Moodle Engagement Metric Across
STAT0002 and STAT0004**

A narrative research report (LaTeX), framed as secondary validation and
diagnostic extension of the metric in Johnston et al. (2025).

## Files

| File | Description |
|------|-------------|
| `report.tex` | LaTeX source |
| `report.pdf` | Compiled PDF |

## Rebuild

```powershell
cd report
pdflatex -interaction=nonstopmode report.tex
pdflatex -interaction=nonstopmode report.tex
```

Figures are embedded from `../outputs/figures`. Data live in `../Data/`.
