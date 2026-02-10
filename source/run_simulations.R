# source/run_simulation.R
# Runs the full factorial simulation study and saves per-scenario results to data/

# This script:
#  - Builds the scenario grid
#  - Parallelizes across scenarios
#  - For each scenario, runs nsim Monte Carlo reps
#  - Saves each scenario's raw results to data/scenario_*.rds
#  - Saves a combined summary object to data/combined_results.rds

library(here)
library(dplyr)
library(purrr)
library(tibble)
library(future.apply)
library(future)

# Reset any existing plan
future::plan(future::sequential)
future::plan(future::multisession, workers = 6)
source(here("simulations", "simulation_code.R"))

# --------------------------
# Design factors 
# --------------------------
n_vec <- c(10, 50, 500)
beta_vec <- c(0, 0.5, 2)
err_vec <- c("normal", "t3")

# Full factorial grid
scenario_grid <- tidyr::expand_grid(
  n = n_vec,
  beta_trt = beta_vec,
  err_type = err_vec
) %>%
  mutate(
    scenario_id = sprintf("n%03d_beta%0.1f_%s", n, beta_trt, err_type)
  )

# --------------------------
# Simulation parameters
# --------------------------
alpha <- 0.05
nsim <- 500    
B <- 50           # Outer bootstrap
B_inner <- 100     # Inner bootstrap for bootstrap-t

# Output folder
out_dir <- here("data")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Choose a parallel plan
future::plan(future::multisession, workers = max(1, parallelly::availableCores() - 1))

run_one_scenario <- function(n, beta_trt, err_type, scenario_id) {
  
  message("Running scenario: ", scenario_id)
  
  reps <- vector("list", nsim)
  for (s in seq_len(nsim)) {
    reps[[s]] <- run_one_rep(
      n = n,
      beta_treat = beta_trt,     
      error_distr = err_type, 
      B = B,
      B_inner = B_inner,
      alpha = alpha
    )
    if (s %% 50 == 0) message("  rep ", s, "/", nsim)
  }
  
  res <- dplyr::bind_rows(reps) %>%
    mutate(
      n = n,
      beta_trt = beta_trt,
      err_type = err_type,
      scenario_id = scenario_id
    )
  
  # Save raw scenario results
  saveRDS(res, file = file.path(out_dir, paste0("scenario_", scenario_id, ".rds")))
  res
}

# Run all scenarios in parallel
all_results <- future_lapply(
  seq_len(nrow(scenario_grid)),
  function(i) {
    sc <- scenario_grid[i, ]
    run_one_scenario(
      n = sc$n,
      beta_trt = sc$beta_trt,
      err_type = sc$err_type,
      scenario_id = sc$scenario_id
    )
  },
  future.seed = TRUE
)

all_results_df <- dplyr::bind_rows(all_results)
saveRDS(all_results_df, file = here("data", "combined_results.rds"))

message("Done. Saved combined results to: ", here("data", "combined_results.rds"))
