# BIOS 731 HW2 – Simulation study (Bootstrap CIs)

Author: Xinyu Dong

## Folder structure

- `analysis/` : reproducible report (.Rmd)
- `source/` : reusable functions (DGM, CI methods, summaries, plots)
- `simulations/` : script to run the full simulation study
- `data/` : simulation outputs (`.rds`) — **ignored by git**
- `figures/` : optional saved figures

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
