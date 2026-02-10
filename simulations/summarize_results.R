# simulations/summarize_results.R
# Summarize simulation outputs into bias/coverage/SE distribution/time metrics

library(dplyr)
library(tidyr)

# Add indicators for coverage for each CI method
# df: Raw simulation results (one row per Monte Carlo rep)
# output: df with coverage indicators
add_coverage_indicators <- function(df, beta_true_col = "beta_trt") {
  beta_true <- df[[beta_true_col]]
  
  df %>%
    mutate(
      cover_wald = (ci_wald_l <= beta_true) & (beta_true <= ci_wald_u),
      cover_pct  = (ci_pct_l  <= beta_true) & (beta_true <= ci_pct_u),
      cover_t    = (ci_t_l    <= beta_true) & (beta_true <= ci_t_u)
    )
}

# Summarize performance measures per scenario and method
# df: Raw results with coverage indicators
# output: long-format summary table
summarize_perf <- function(df) {
  
  df2 <- add_coverage_indicators(df)
  
  # Bias for beta_hat
  scen_summary <- df2 %>%
    group_by(scenario_id, n, beta_trt, err_type) %>%
    summarise(
      bias = mean(beta_hat - beta_trt, na.rm = TRUE),
      se_mean = mean(se_hat, na.rm = TRUE),
      se_sd   = sd(se_hat, na.rm = TRUE),
      cover_wald = mean(cover_wald, na.rm = TRUE),
      cover_pct  = mean(cover_pct,  na.rm = TRUE),
      cover_t    = mean(cover_t,    na.rm = TRUE),
      time_wald = mean(time_wald, na.rm = TRUE),
      time_pct  = mean(time_pct,  na.rm = TRUE),
      time_t    = mean(time_t,    na.rm = TRUE),
      .groups = "drop"
    )
  
  # Pivot coverage + time to long by method
  coverage_long <- scen_summary %>%
    select(scenario_id, n, beta_trt, err_type, cover_wald, cover_pct, cover_t) %>%
    pivot_longer(
      cols = starts_with("cover_"),
      names_to = "method",
      values_to = "coverage"
    ) %>%
    mutate(method = recode(method,
                           cover_wald = "Wald",
                           cover_pct = "Bootstrap percentile",
                           cover_t = "Bootstrap t"))
  
  time_long <- scen_summary %>%
    select(scenario_id, n, beta_trt, err_type, time_wald, time_pct, time_t) %>%
    pivot_longer(
      cols = starts_with("time_"),
      names_to = "method",
      values_to = "time_sec"
    ) %>%
    mutate(method = recode(method,
                           time_wald = "Wald",
                           time_pct = "Bootstrap percentile",
                           time_t = "Bootstrap t"))
  
  list(
    scenario_summary = scen_summary,
    coverage_long = coverage_long,
    time_long = time_long
  )
}
