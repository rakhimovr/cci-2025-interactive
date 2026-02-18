# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Climate Culture Index (CCI) 2025 Interactive — an R-based data analysis pipeline that processes survey data on US public attitudes toward seven climate-mitigating behaviors and generates an interactive HTML report. Maintained by Rare.

## Build & Render

Render the main report (requires R and all packages installed):

```r
rmarkdown::render("cci-25-interactive.Rmd")
```

Or from the command line:

```bash
Rscript -e 'rmarkdown::render("cci-25-interactive.Rmd")'
```

There is no Makefile, test suite, or linter configured.

## R Package Dependencies

All packages are loaded in `source files/load-packages.R` and at the top of the Rmd. Key packages:

- **Data I/O:** `haven` (SPSS .sav files), `readr`, `readxl`, `labelled`
- **Data manipulation:** `tidyverse`, `magrittr` (`%<>%` used heavily), `data.table`, `fastDummies`, `forcats`
- **Survey weighting:** `survey`, `weights`
- **Visualization:** `ggplot2`, `plotly`, `patchwork`, `gridExtra`, `ggtext`, `ggrepel`
- **Tables:** `reactable`, `knitr`, `htmltools`
- **Analysis:** `jtools`, `broom`, `broom.mixed`, `effectsize`, `grf`

## Architecture & Data Pipeline

The Rmd file orchestrates the pipeline by sourcing R scripts sequentially:

```
1. source files 2025/load-data-2025.R    → Load SPSS data (data/spss.sav), merge with Lucid demographics
2. source files 2025/clean-data-2025.R   → QC: completion filter, attention checks, speeding detection
3. source files/table-color.R            → Color palette function for tables
4. [inline] sassy_2025 Segmentation.csv  → Join Yale SASSY psychographic segments
5. source files 2025/national-weights.R  → Multi-step raking (age×gender×race → geography → SES → ethnicity → politics+SASSY)
6. [inline Rmd code]                     → Build index table, generate trend plots, interactive reactable
```

Regional variants (Boston, Denver) follow the same pattern with their own clean/weight scripts.

### Key Data Objects

- **`data_2025`** — Main survey dataframe, progressively transformed through the pipeline. Ends with a `wt` column (survey weight).
- **`index_2025`** — Summary table of weighted means across all behaviors × indicators, used for the main interactive table.

### Survey Weighting

`national-weights.R` applies 5-step iterative raking via `survey::rake()` with weight trimming (bounds 0.2–5.0). Population targets are read from CSVs in `weights/US/`.

## Important Conventions

- **Variable naming:** Behavior prefixes (`ev_`, `solar_`, `community.solar_`, `offset_`, `hp_`, `beef_`, `fw_`) followed by indicator (`adoption`, `consider`, `intention`, `ee`, `pnb`, `ne_national`, `self.efficacy`, `difficulty`, `outcome.efficacy`, `personal.benefit`, `policy.support`, `policy.ee`, `policy.info`, `policy.contact`).
- **Behavior color palette:** Consistent hex colors used across all plots — defined in `plots/plot_setup.R` and reused in individual plot files.
- **`%<>%` operator:** Used extensively for in-place mutation of dataframes (magrittr compound assignment).
- **Paths are relative** to the project root (working directory must be set to repo root before rendering).

## Directory Structure

| Path | Purpose |
|------|---------|
| `cci-25-interactive.Rmd` | Main report (R Markdown → HTML) |
| `source files 2025/` | 2025 data loading, cleaning, weighting scripts |
| `source files/` | Shared/legacy utility scripts (packages, color palette) |
| `plots/` | Trend plot R scripts and their PNG outputs |
| `data/` | Input SPSS survey files (2024 and 2025) |
| `weights/` | Census/demographic CSVs for raking targets (US, Boston, Denver) |
| `_site.yml` | R Markdown site config (navbar, theme: flatly) |

## Seven Tracked Behaviors

Drive an EV, Install solar panels, Sign up for community solar, Buy carbon offsets, Install a heat pump, Eat less beef, Reduce food waste.

Each is measured across 14 psychosocial indicators (adoption, consideration, intention, empirical/normative expectations, self/outcome efficacy, difficulty, personal benefit, policy support, etc.).
