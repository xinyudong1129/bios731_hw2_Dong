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

