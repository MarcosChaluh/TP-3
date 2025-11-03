# TP-3 Analysis Pipeline

This project contains the scripts used to reproduce the labour market analyses for the TP-3 report. The repository is organised around a data ingestion step followed by a collection of analytical scripts that generate the output tables and plots.

## Prerequisites

1. **R 4.2+** with the ability to run `Rscript` from the command line.
2. The following R packages must be available:
   - Core tidyverse packages used across the scripts: `dplyr`, `readr`, `tibble`, `stringr`, `tidyr`, `purrr`, `ggplot2`, and `janitor`.
   - Additional dependencies: `readxl`, `expss`, and `eph`.
   Install them once inside R with:
   ```r
   install.packages(c(
     "dplyr", "readr", "tibble", "stringr", "tidyr", "purrr", "ggplot2",
     "janitor", "readxl", "expss", "eph"
   ))
   ```
3. Place the external crosswalk workbooks provided with the project inside `data/raw/` (see the sample files already present in the repository). These files are required by the remote work module.

## Project structure helpers

All scripts rely on the helper functions in [`R/project_paths.R`](R/project_paths.R) to locate the `data/` and `outputs/` directories. The first time you run a script, it will create any missing folders automatically via `ensure_project_structure()`.

## Step-by-step execution

1. **Download and standardise EPH microdata**
   ```bash
   Rscript get_data.R
   ```
   This command retrieves the individual microdata from `eph` for 2017–2025, standardises column names and types, and stores two `.rds` files under `data/processed/` (`eph_individual_2017_2025.rds` and `eph_individual_2017_2025_std.rds`).

2. **Run the full analysis suite**
   ```bash
   Rscript scripts/run_all.R
   ```
   The convenience script sequentially sources each analysis module (`1a` through `2`), generating tables and plots inside `outputs/`. The remote-work module (`scripts/2_remote_work.R`) consumes the processed EPH data along with the crosswalk spreadsheets in `data/raw/` to build its summary tables.

3. **Inspect results**
   - Generated tables will be written to `outputs/tables/` as CSV files. For example, the remote work analysis saves `tabla_1_AI_WFH_por_Ocupacion.csv`, `tabla_2_AI_WFH_por_Aglomerado.csv`, and `tabla_3_AI_WFH_por_Industria.csv`.
   - Plots produced by the earlier scripts are written to `outputs/plots/` via the helper in [`R/output_utils.R`](R/output_utils.R).

## Running individual modules

Each script under `scripts/` can be executed on its own if you only need a subset of the outputs. For instance, to rerun just the remote work module after updating crosswalk data:

```bash
Rscript scripts/2_remote_work.R
```

The module will reuse the processed EPH dataset generated in step 1.

## Troubleshooting tips

- If `Rscript get_data.R` fails because the `eph` package cannot download certain quarters, rerun the command after verifying your internet connection. The helper `safe_get_micro()` silently skips quarters that cannot be retrieved, so a transient failure may reduce coverage but will not crash the pipeline.
- Ensure the remote-work crosswalk files retain their original sheet names and column headers. The loader in [`R/remote_work.R`](R/remote_work.R) searches for an `ISCO-2` column whose header matches one of `2_isco`, `x2_isco`, `isco_2`, or `two_isco`.

With the data prepared and dependencies installed, the two commands above will reproduce all deliverables in a clean environment.
