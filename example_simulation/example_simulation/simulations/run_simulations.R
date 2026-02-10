####################################################################
# BIOS 731: Advanced Statistical Computing
#
# This file produces simulations for linear regression under different data
# generating scenarios
####################################################################


library(tidyverse)
library(broom) # for 03_extract_estimates

###############################################################
## define or source functions used in code below
###############################################################

source(here::here("source", "01_simulate_data.R"))
source(here::here("source", "02_apply_methods.R"))
source(here::here("source", "03_extract_estimates.R"))

###############################################################
## set simulation design elements
###############################################################

# how are you justifying nsim?
nsim = 900


# Why NA showed up: because on a certain simulation,
# the values generated are entirely zeroes or ones

n = c(10, 50, 100)
beta_true = c(0.5, 2)
sigma2_true = c(1, 5)

params = expand.grid(n = n,
                     n_sim = nsim,
                     beta_true = beta_true,
                     sigma2_true = sigma2_true)

# define simulation scenario

for (case in 1:12){
  scenario = case
  params_ind = params[scenario,]

  ###############################################################
  ## start simulation code
  ###############################################################

  # generate a random seed for each simulated dataset
  set.seed(scenario)
  seed = floor(runif(nsim, 1, 10000))
  results = as.list(rep(NA, nsim))

  for(i in 1:nsim){

    set.seed(seed[i])

    ####################
    # simulate data
    simdata = get_simdata(n = params_ind$n,
                          beta_treat = params_ind$beta_true,
                          sigma2 = params_ind$sigma2_true)

    ####################
    # apply method(s)
    model_lm = fit_model(simdata)

    ####################
    # calculate estimates
    estimates = get_estimates(model_lm, true_beta = params_ind$beta_true)

    ####################
    # store results, including estimates, speed, parameter scenarios
    # store results, including estimates, speed, parameter scenarios
    estimates = estimates %>%
      mutate(true_beta = params_ind$beta_true,
             n = params_ind$n,
             sigma2_true = params_ind$sigma2_true,
             seed = seed[i],
             scenario = scenario)

    results[[i]] = estimates
  }

  ####################
  # save results
  # note that I am saving results outside of the for loop. For slow simulations,
  # you may want to save each iteration separately
  results_df = bind_rows(results)

  filename = paste0("scenario_", scenario, ".RDA")
  save(results_df,
       file = here::here("data", filename))

}
