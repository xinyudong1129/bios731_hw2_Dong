# BIOS 731 HW2 – Simulation study (Bootstrap Confidence intervals)

Author: Xinyu Dong

## Folder structure

-   `analysis/` : final reproducible report (`.rmd`)
-   `source/` : master scripts to execute the full simulation study
-   `simulations/` : step-by-step scripts and reusable functions to complete the simulation and provide visualization and tabular results
-   `data/` : saved intermediate results (scenario-wise `.rds` and combined `.rds`)
-   `figures/` : visualization plots saved for this report


## How to run (end-to-end)

1. Open the `.Rproj` in RStudio.
2. Install packages if needed:
   - `here`, `dplyr`, `tidyr`, `purrr`, `tibble`, `future.apply`, `parallelly`, `ggplot2`
3. Run the simulation:
   ```r
   source(here::here("simulations", "run_simulation.R"))
   ```
   This saves scenario-wise `.rds` files and a combined `data/combined_results.rds`.
4. Knit the report:
   - Open `analysis/final_report_hw2_Xinyu_Dong.Rmd`
   - Knit to PDF

## Notes on computation

Bootstrap-t uses a nested bootstrap (B=500 outer, B_inner=100 inner), so it can be slow,
especially for `n=500`. The simulation script parallelizes across the 18 scenarios.

*To accommendate for excessive computational time, I employed B=50 outer, B_inner=100 inner*
