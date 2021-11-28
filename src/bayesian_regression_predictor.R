library(pacman)
p_load(tidyverse)

colvalues <- function(df, column) df %>% dplyr::select(!!sym(column)) %>% pull()

param_mean <- function(model, param) get_posterior_mean(model, par=param)[4]

BayesianRegressionPredictor.from <- function(model, params, vars) {
  predictor <- params %>% 
    map(~ setNames(list(param_mean(model, .x)), .x)) %>%
    flatten()

  predictor$vars <- vars
  
  class(predictor) <- 'BayesianRegressionPredictor'
  predictor
}

get_params <- function(predictor, param_name) {
  names(predictor) %>%
    keep(~grepl(param_name,.x)) %>%
    map(~predictor[[.x]]) %>% 
    unlist()
}

predict_one <- function(predictor,  variables) {
  betas <- get_params(predictor, 'beta')

  betas[-1] %>% 
    map2(variables, ~ .x * .y) %>% 
    reduce(~.x+.y, .init=betas[1])
}

predict.BayesianRegressionPredictor <- function(predictor, df) {
  inputs <- df %>% dplyr::select(predictor$vars)
  apply(inputs, 1, function(row)  predict_one(predictor, row))
}

