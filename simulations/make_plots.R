# source/make_plots.R

  library(ggplot2)
  library(dplyr)

plot_bias <- function(scen_summary) {
  ggplot(scen_summary, aes(x = factor(beta_trt), y = bias)) +
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

plot_coverage <- function(coverage_long) {
  ggplot(coverage_long, aes(x = factor(beta_trt), y = coverage, group = method, shape = method)) +
    geom_hline(yintercept = 0.95, linetype = 2) +
    geom_point(position = position_dodge(width = 0.4)) +
    facet_grid(err_type ~ n, labeller = label_both) +
    ylim(0, 1) +
    labs(
      x = "True treatment effect (beta_trt)",
      y = "Empirical coverage",
      title = "95% CI coverage across methods and scenarios",
      caption = "Dashed line marks nominal 0.95 coverage"
    )
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

plot_time <- function(time_long) {
  ggplot(time_long, aes(x = method, y = time_sec)) +
    geom_point() +
    facet_grid(err_type ~ n, labeller = label_both) +
    labs(
      x = "Method",
      y = "Mean time per Monte Carlo replication (seconds)",
      title = "Computation time across CI methods",
      caption = "Time measured with proc.time()[3] inside each replication"
    ) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}
