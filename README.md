# BIOS 731 HW2 – Simulation study (Bootstrap Confidence intervals)

Author: Xinyu Dong

## Project Structure

This repository is organized to support a fully reproducible Monte Carlo simulation study of confidence interval methods for a linear regression treatment effect. The codebase separates simulation logic, reusable utilities, generated results, figures, and the final report.

- `analysis/` : Final reproducible report and knitted outputs  
  - `final_report_hw2_Xinyu_Dong.Rmd`  
    Main R Markdown file that loads saved simulation results from `data/`, sources plotting and summarization functions from `source/`, and produces all tables, figures, and written interpretations.  
  - `final_report_hw2_Xinyu_Dong.pdf`  
    Knitted PDF output of the final report.  
  - Auto-generated folders (e.g., `final_report_hw2_Xinyu_Dong_files/`) are created by R Markdown during knitting and are not part of the core workflow.

- `simulations/` : Master scripts and reusable plotting/summarization utilities
  - `simulation_code.R`  
    Defines the data-generating process, model fitting, and confidence interval construction methods (Wald CI, bootstrap percentile CI, bootstrap-\(t\) CI), as well as a single Monte Carlo replication wrapper.  
  - `make_plots.R`  
    Contains functions to generate all figures used in the report (coverage, bias, SE distributions, computation time).  
  - `summarize_results.R`  
    Contains functions to compute performance metrics across Monte Carlo replications (bias, empirical coverage, SE distributions, computation time summaries).  
  - These scripts are sourced by the final report to ensure all results and figures are generated reproducibly from the saved simulation outputs.

- `source/` : Core simulation code and data-generating mechanisms  
  - `run_simulations.R`  
    Master script that runs the full factorial simulation design across all combinations of sample size, true treatment effect, and error distribution. This script generates and saves scenario-wise results to `data/` and produces the combined results object used by the final report.

- `data/` : Saved simulation outputs (intermediate and combined results)  
  - `scenario_*.rds`  
    Scenario-wise simulation outputs for each combination of \((n, \beta_{\text{trt}}, \text{error type})\).  
  - `combined_results.rds`  
    Combined results across all scenarios and Monte Carlo replications, produced by `simulations/run_simulations.R`.  
  - These saved objects allow the final report to be rendered without re-running the full simulation study.

- `figures/` : Saved visualization outputs  
  - Contains all plots generated for the final report, including:  
    - Empirical coverage by method  
    - Bias of the treatment effect estimator  
    - Distribution of estimated standard errors  
    - Computation time comparisons across CI methods  
  - Figures are saved in high-resolution formats (e.g., `.png` or `.pdf`) by the plotting functions in `source/make_plots.R` and are included in the final report.

## Reproducibility

To reproduce the full simulation study from scratch:

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
