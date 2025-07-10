# Playing around with simsurv R package to simulate some other survival data
#install.packages("simsurv")
# Here I am using the most conventional use case of the simsurv package
# (a proportional hazards assumption and time following well-known distribution, time-independent covariates)
# But the apckage allows for more complex scenarios

# Data simulation scenario:
# Proportional hazard scenario with time following Weibull distribution with parameters:
# shape (gamma) = 1.5 (controls how the risk changes over time, since shape > 1 the instantaneous hazard increases as time goes on)
# scale (lambda) = 0.1 (sets the “speed” at which risk accumulates over time)
# Covariates: 0-1 treatment with the effect beta = -0.5 --> subjects receiving
# trt have reduced risk of the event multiplicatively by exp(-0.5) as compared to untreated, c.p.
# Remianing 20 covariates are drawn from normal dist. but they have no influence on outcome (beta coefs == 0)
library(tidyverse)
library(simsurv)
set.seed(786)
covariates <- data.frame(id = 1:100,
                         trt = rbinom(100, 1, 0.5))

noise <- matrix(rnorm(n = 100*20), nrow = 100) %>%
  as.data.frame()

colnames(noise) <- paste0("noise", c(1:20))

data <- covariates %>% bind_cols(noise)

beta_coefs <- c(trt = -0.5, noise = rep(0, 20))
# Simulate data
# maxt -  specifies the maximum follow up time; individuals with a simulated event
# time larger than maxt will be right-censored and their event indicator di will be set to 0.
# Selected such that more subjects experiance event than censoring
data_sim <- simsurv(lambdas = 0.1, gammas = 1.5, betas = beta_coefs,
                    x = data, maxt = 7)

data_all_weibull <- data %>% inner_join(data_sim)
data_all_weibull %>% count(status)

shadow_weibul <- shadow_vimp_survival(alphas = c(0.3, 0.1, 0.05),
                                      data = data_all_weibull,
                                      time_column = "eventtime",
                                      censoring_column = "status",
                                      num.trees = 100)
