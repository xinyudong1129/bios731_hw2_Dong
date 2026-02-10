# simulations/simulation_code.R

# This file defines:
#  - Data-generating mechanism for linear regression with treatment indicator
#  - Estimation + Wald CI for beta_treatment
#  - Nonparametric bootstrap percentile CI
#  - Nonparametric bootstrap t CI (double bootstrap for SE*)

library(stats)
library(tibble)

# ------------------------------------------------
# Data-generating mechanism
# ------------------------------------------------
# n: Integer sample size
# beta_treat: True treatment effect
# error_distr: "normal" or "t3"
# output: tibble with columns y (outcome), x (binary treatment indicator)
get_simdata <- function(n, beta_treat, error_distr = c("normal", "t3")) {
  error_distr <- match.arg(error_distr)
  
  # Fixed parameters
  beta0  <- 1
  p_treat <- 0.5
  sigma2 <- 2
  v <- 3
  
  # Binary treatment indicator (0/1 numeric)
  x <- rbinom(n, size = 1, prob = p_treat)
  if (length(unique(x)) < 2) {
    # force at least one treated and one control
    x[1] <- 1 - x[1]
  }
  
  # Error distribution
  epsilon <- switch(
    error_distr,
    normal = rnorm(n, mean = 0, sd = sqrt(sigma2)),
    t3 = {
      u <- rt(n, df = v)
      u * sqrt(sigma2 * (v - 2) / v)
    }
  )
  
  # Outcome
  y <- beta0 + beta_treat * x + epsilon
  
  tibble::tibble(
    y = y,
    x = x
  )
}

# ------------------------------------------------
# Fit linear model and extract treatment estimate
# ------------------------------------------------
# dat: tibble with columns y, x
# output: list(beta_hat, se_hat)
fit_lm_trt <- function(dat) {
  fit <- lm(y ~ x, data = dat)
  coefs <- summary(fit)$coefficients
  get_beta_hat <- function(fit, term = "x") {
    cf <- coef(fit)
    unname(cf[term])
  }
  
  get_se_hat <- function(fit, term = "x") {
    V <- vcov(fit)
    sqrt(unname(V[term, term]))
  }
  list(beta_hat = get_beta_hat(fit, term = "x"), se_hat = get_se_hat(fit, term = "x"))
}

# ------------------------------------------------
# Wald CI
# ------------------------------------------------
wald_ci <- function(beta_hat, se_hat, alpha = 0.05) {
  z <- qnorm(1 - alpha / 2)
  c(beta_hat - z * se_hat,
    beta_hat + z * se_hat)
}

# ------------------------------------------------
# Bootstrap percentile CI
# ------------------------------------------------
boot_percentile_ci <- function(dat, B = 500, alpha = 0.05) {
  n <- nrow(dat)
  boot_est <- numeric(B)
  
  for (b in seq_len(B)) {
    idx <- c(
      sample(which(dat$x == 0), replace = TRUE),
      sample(which(dat$x == 1), replace = TRUE)
    )
    boot_est[b] <- fit_lm_trt(dat[idx, , drop = FALSE])$beta_hat
  }
  
  qs <- quantile(
    boot_est,
    probs = c(alpha/2, 1 - alpha/2),
    na.rm = TRUE,
    names = FALSE
  )
  
  list(ci = as.numeric(qs), boot_est = boot_est)
}

# ------------------------------------------------
# Bootstrap-t CI (double bootstrap)
# ------------------------------------------------
boot_t_ci <- function(dat, B = 500, B_inner = 100, alpha = 0.05) {
  orig <- fit_lm_trt(dat)
  beta_hat <- orig$beta_hat
  se_hat   <- orig$se_hat
  
  t_star <- numeric(B)
  
  for (b in seq_len(B)) {
    
    # Outer bootstrap (stratified to keep both groups)
    idx_outer <- c(
      sample(which(dat$x == 0), replace = TRUE),
      sample(which(dat$x == 1), replace = TRUE)
    )
    dat_outer <- dat[idx_outer, , drop = FALSE]
    
    out_fit <- fit_lm_trt(dat_outer)
    beta_hat_star <- out_fit$beta_hat
    
    # Inner bootstrap
    inner_est <- numeric(B_inner)
    for (j in seq_len(B_inner)) {
      idx_inner <- c(
        sample(which(dat_outer$x == 0), replace = TRUE),
        sample(which(dat_outer$x == 1), replace = TRUE)
      )
      dat_inner <- dat_outer[idx_inner, , drop = FALSE]
      inner_est[j] <- fit_lm_trt(dat_inner)$beta_hat
    }
    
    se_star <- sd(inner_est, na.rm = TRUE)
    
    if (!is.finite(se_star) || se_star <= 0 ||
        !is.finite(beta_hat_star) || !is.finite(beta_hat)) {
      t_star[b] <- NA_real_
    } else {
      t_star[b] <- (beta_hat_star - beta_hat) / se_star
    }
  }
  
  # ---- THIS is the line that prevents the crash ----
  t_star_clean <- t_star[is.finite(t_star)]
  if (length(t_star_clean) < 20) {
    return(list(ci = c(NA_real_, NA_real_), t_star = t_star))
  }
  
  tq <- quantile(t_star_clean,
                 probs = c(alpha/2, 1 - alpha/2),
                 names = FALSE, type = 6)
  
  ci <- c(beta_hat - tq[2] * se_hat,
          beta_hat - tq[1] * se_hat)
  
  list(ci = ci, t_star = t_star)
}

# ------------------------------------------------
# Run one Monte Carlo replication
# ------------------------------------------------
run_one_rep <- function(n, beta_treat, error_distr,
                        B = 500, B_inner = 100, alpha = 0.05) {
  
  dat <- get_simdata(n = n, beta_treat = beta_treat,
                     error_distr = error_distr)
  
  fit <- fit_lm_trt(dat)
  beta_hat <- fit$beta_hat
  se_hat   <- fit$se_hat
  
  # Wald
  t0 <- proc.time()[3]
  ci_wald <- wald_ci(beta_hat, se_hat, alpha)
  time_wald <- proc.time()[3] - t0
  
  # Bootstrap percentile
  t0 <- proc.time()[3]
  bp <- boot_percentile_ci(dat, B = B, alpha = alpha)
  ci_pct <- bp$ci
  time_pct <- proc.time()[3] - t0
  
  # Bootstrap-t
  t0 <- proc.time()[3]
  bt <- boot_t_ci(dat, B = B, B_inner = B_inner, alpha = alpha)
  ci_t <- bt$ci
  time_t <- proc.time()[3] - t0
  
  tibble::tibble(
    beta_hat = beta_hat,
    se_hat   = se_hat,
    ci_wald_l = ci_wald[1], ci_wald_u = ci_wald[2],
    ci_pct_l  = ci_pct[1],  ci_pct_u  = ci_pct[2],
    ci_t_l    = ci_t[1],    ci_t_u    = ci_t[2],
    time_wald = time_wald,
    time_pct  = time_pct,
    time_t    = time_t
  )
}
