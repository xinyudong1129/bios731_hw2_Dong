# source/sim_functions.R

# This file defines:
#  - Data-generating mechanism for linear regression with treatment indicator
#  - Estimation + Wald CI for beta_treatment
#  - Nonparametric bootstrap percentile CI
#  - Nonparametric bootstrap t CI (double bootstrap for SE*)


library(stats)
# n: Integer sample size.
# beta_trt: True treatment effect.
# beta0: Intercept (default 0).
# err_type: "normal" or "t3".
# sigma2: Target error variance (default 2).
# output: data.frame with columns y, trt
sim_one_dataset <- function(n,
                            beta_trt,
                            beta0 = 0,
                            err_type = c("normal", "t3"),
                            sigma2 = 2) {
  
  err_type <- match.arg(err_type)
  
  # Balanced treatment assignment for stability at small n
  trt <- rbinom(n, size = 1, prob = 0.5)
  
  if (err_type == "normal") {
    eps <- rnorm(n, mean = 0, sd = sqrt(sigma2))
  } else {
    # Heavy-tailed: t_{nu=3} scaled to have variance sigma2
    # If u ~ t_nu, Var(u)=nu/(nu-2). we want Var(eps)=sigma2 => eps = u * sqrt(sigma2 * (nu-2)/nu)
    nu <- 3
    u <- rt(n, df = nu)
    scale_fac <- sqrt(sigma2 * (nu - 2) / nu)
    eps <- u * scale_fac
  }
  
  y <- beta0 + beta_trt * trt + eps
  data.frame(y = y, trt = trt)
}

# Fit linear model and extract treatment estimate and SE

# dat: data.frame with y and trt
# output: list(beta_hat, se_hat)

fit_lm_trt <- function(dat) {
  fit <- lm(y ~ trt, data = dat)
  coefs <- summary(fit)$coefficients
  beta_hat <- unname(coefs["trt", "Estimate"])
  se_hat   <- unname(coefs["trt", "Std. Error"])
  list(beta_hat = beta_hat, se_hat = se_hat)
}

# Wald 95% CI for beta_trt
# beta_hat: Estimate
# se_hat: Standard error estimate
# alpha: 1 - confidence level (default 0.05)
# output: numeric vector c(lower, upper)
wald_ci <- function(beta_hat, se_hat, alpha = 0.05) {
  z <- qnorm(1 - alpha/2)
  c(beta_hat - z * se_hat, beta_hat + z * se_hat)
}

# Nonparametric bootstrap percentile CI

# dat: Original data
# B: Outer bootstrap reps (default 500)
# alpha: 1 - confidence level
# output: list(ci=c(lower, upper), boot_est=vector)
boot_percentile_ci <- function(dat, B = 500, alpha = 0.05) {
  n <- nrow(dat)
  boot_est <- numeric(B)
  for (b in seq_len(B)) {
    idx <- sample.int(n, size = n, replace = TRUE)
    boot_est[b] <- fit_lm_trt(dat[idx, , drop = FALSE])$beta_hat
  }
  qs <- quantile(boot_est, probs = c(alpha/2, 1 - alpha/2), names = FALSE, type = 6)
  list(ci = as.numeric(qs), boot_est = boot_est)
}

# Bootstrap-t CI (double bootstrap for SE*)

# For each outer bootstrap sample b:
#   1) compute beta_hat_star(b)
#   2) compute se_star(b) via INNER bootstrap on that outer sample
#   3) compute t_star(b) = (beta_hat_star(b) - beta_hat) / se_star(b)

# CI: [ beta_hat - t_{1-alpha/2}^* * se_hat , beta_hat - t_{alpha/2}^* * se_hat ]
# where t_q^* is the empirical quantile of t_star.

# dat: Original data
# B: Outer bootstrap reps (default 500)
# B_inner: Inner bootstrap reps (default 100)
# alpha: 1 - confidence level
# output: list(ci=c(lower, upper), t_star=vector, boot_est=vector)
boot_t_ci <- function(dat, B = 500, B_inner = 100, alpha = 0.05) {
  n <- nrow(dat)
  
  # Original sample estimates
  orig <- fit_lm_trt(dat)
  beta_hat <- orig$beta_hat
  se_hat   <- orig$se_hat
  
  boot_est <- numeric(B)
  t_star   <- numeric(B)
  
  for (b in seq_len(B)) {
    idx_outer <- sample.int(n, size = n, replace = TRUE)
    dat_outer <- dat[idx_outer, , drop = FALSE]
    
    # Outer estimate
    out_fit <- fit_lm_trt(dat_outer)
    beta_hat_star <- out_fit$beta_hat
    boot_est[b] <- beta_hat_star
    
    # Inner bootstrap to estimate SE* for this outer sample
    inner_est <- numeric(B_inner)
    for (j in seq_len(B_inner)) {
      idx_inner <- sample.int(n, size = n, replace = TRUE)
      dat_inner <- dat_outer[idx_inner, , drop = FALSE]
      inner_est[j] <- fit_lm_trt(dat_inner)$beta_hat
    }
    se_star <- sd(inner_est)
    
    # Guard against degenerate SE* (can happen when n is very small)
    if (is.na(se_star) || se_star <= 0) {
      t_star[b] <- NA_real_
    } else {
      t_star[b] <- (beta_hat_star - beta_hat) / se_star
    }
  }
  
  # Remove NA t* if any
  t_star_clean <- t_star[is.finite(t_star)]
  if (length(t_star_clean) < max(10, 0.8 * B)) {
    warning("Many NA/inf t* values; bootstrap-t CI may be unstable for this dataset.")
  }
  
  tq <- quantile(t_star_clean, probs = c(alpha/2, 1 - alpha/2), names = FALSE, type = 6)
  
  # Note the 'minus' form
  ci <- c(beta_hat - tq[2] * se_hat,
          beta_hat - tq[1] * se_hat)
  
  list(ci = as.numeric(ci), t_star = t_star, boot_est = boot_est)
}

