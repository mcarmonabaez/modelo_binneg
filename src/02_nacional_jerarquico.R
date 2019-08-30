library(rstan)
library(tidyverse)
source('lib/helpers.R')

nal_2012 <- load('data/nal_2012.rda')
data("nal_2012")
sm <- stan_model(file = "src/stan_files/neg_binomial_hier.stan",
                 save_dso = TRUE)
sm
set.seed(8382312)
nal_sample <- select_sample_prop(nal_2012, 
                                 stratum = estrato, 0.052) 
data_model <- nal_sample
####### 
party_name <- "pan"
x <- as.matrix(data_model %>% ungroup %>% select(rural, tamano_md, tamano_gd))

data_sample <- list(N = nrow(data_model), n = data_model$ln_total,
                    n_covariates = ncol(x),
                    n_strata = length(unique(data_model$estrato)),
                    y = data_model[, party_name][[1]],
                    stratum = dplyr::pull(data_model, estrato),
                    x = x
)
data <- nal_2012
x_full <- as.matrix(data %>% ungroup %>% select(rural, tamano_md, tamano_gd))
data_full <- list(N_f = nrow(data), n_f = data$ln_total,
                  n_covariates_f = ncol(x_full),
                  in_sample = as.numeric(data$casilla_id %in% data_model$casilla_id),
                  n_strata_f = length(unique(data$estrato)),
                  y_f = data[, party_name][[1]],
                  stratum_f = dplyr::pull(data, estrato),
                  x_f = x_full)

stan_fit <- sampling(sm, iter = 800, warmup = 200, 
                     chains = 3, data= c(data_sample, data_full), cores = 3)

y_sims <- rstan::extract(stan_fit, 'y_out')[[1]]
qplot(y_sims, binwidth=1000) + geom_vline(xintercept=sum(data_full$y_f), colour="red")
