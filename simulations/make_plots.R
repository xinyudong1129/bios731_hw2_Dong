# simulations/make_plots.R

  library(ggplot2)
  library(dplyr)

  plot_bias <- function(raw_df) {
    bias_df <- raw_df %>%
      dplyr::group_by(n, beta_trt, err_type) %>%
      dplyr::summarise(
        bias = mean(beta_hat - beta_trt, na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(bias_df, aes(x = factor(beta_trt), y = bias)) +
      geom_hline(yintercept = 0, linetype = 2) +
      geom_point() +
      facet_grid(err_type ~ n, labeller = label_both) +
      labs(
        x = "True treatment effect (beta_trt)",
        y = "Bias of beta_hat",
        title = "Bias of treatment effect estimator across scenarios",
        caption = "Bias = mean(beta_hat - beta_true) across nsim Monte Carlo reps"
      )
}

plot_coverage <- function(raw_df) {
  cov_df <- raw_df %>%
    mutate(
      cover_wald = (ci_wald_l <= beta_trt) & (beta_trt <= ci_wald_u),
      cover_pct  = (ci_pct_l  <= beta_trt) & (beta_trt <= ci_pct_u),
      cover_t    = (ci_t_l    <= beta_trt) & (beta_trt <= ci_t_u)
    ) %>%
    group_by(n, beta_trt, err_type) %>%
    summarise(
      cov_wald = mean(cover_wald, na.rm = TRUE),
      cov_pct  = mean(cover_pct,  na.rm = TRUE),
      cov_t    = mean(cover_t,    na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      starts_with("cov_"),
      names_to = "method",
      values_to = "coverage"
    )
  
  ggplot(cov_df, aes(x = factor(beta_trt), y = coverage, group = method, shape = method)) +
    geom_hline(yintercept = 0.95, linetype = 2) +
    geom_point(position = position_dodge(width = 0.4)) +
    facet_grid(err_type ~ factor(n), labeller = label_both) +
    ylim(0, 1) +
    labs(x = "True treatment effect", y = "Empirical coverage")
}

plot_se_dist <- function(raw_df) {
  ggplot(raw_df, aes(x = se_hat)) +
    geom_histogram(bins = 30) +
    facet_grid(err_type ~ n, labeller = label_both) +
    labs(
      x = "Estimated SE of beta_hat",
      y = "Count",
      title = "Distribution of SE(beta_hat) across Monte Carlo reps",
      caption = "Histogram combines all beta_trt values within each facet"
    )
}

make_time_long <- function(raw_df) {
  raw_df %>%
    dplyr::select(n, beta_trt, err_type, time_wald, time_pct, time_t) %>%
    tidyr::pivot_longer(
      cols = starts_with("time_"),
      names_to = "method",
      values_to = "time_sec"
    ) %>%
    dplyr::mutate(
      method = dplyr::recode(
        method,
        time_wald = "Wald",
        time_pct  = "Percentile",
        time_t    = "Bootstrap-t"
      )
    )
}


plot_time <- function(raw_df) {
  time_long <- make_time_long(raw_df)
  
  ggplot(time_long, aes(x = method, y = time_sec, color = method)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    facet_grid(err_type ~ n, labeller = label_both) +
    labs(
      x = "Method",
      y = "Mean time per Monte Carlo replication (seconds)",
      title = "Computation time across CI methods",
      caption = "Time measured with proc.time()[3] inside each replication"
    ) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}
