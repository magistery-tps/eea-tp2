# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, tidyverse, tidymodels, compareGroups, MASS, caret, Metrics)
setwd(this.path::this.dir())
#
source('./bayesian_regression_predictor.R')
# ------------------------------------------------------------------------------

model_coefficients_summary <- function(df, p_value_threshold=0.05) {
  if(!("p.value" %in% colnames(df))) {
    print("WARN: p.value column is required to make model coefficients summary!\n");
    return(NULL)
  }
  if(!("conf.low" %in% colnames(df))) {
    print("WARN: conf.low column is required to make model coefficients summary!\n");
    return(NULL)
  }
  if(!("conf.high" %in% colnames(df))) {
    print("WARN: conf.high column is required to make model coefficients summary!\n");
    return(NULL)
  }
  
  df %>% 
    mutate(
      Signiticativo = ifelse(p.value < p_value_threshold, "Si", "No"),
      "IC incluye al cero" = ifelse(conf.low <= 0 & conf.high >=0, "Si", "No")
    ) %>%
    rename(Termino = term, Coeficiente = estimate) %>%
    dplyr::select(-std.error, -statistic, -p.value, -conf.low, -conf.high) 
}

plot_tidy_coefficients <- function(df) {
  if(!("p.value" %in% colnames(df))) {
    print("WARN: p.value column is required to plot tidy coefficients!\n");
    return(NULL)  
  }
  if(!("conf.low" %in% colnames(df))) {
    print("WARN: conf.low column is required to plot tidy coefficients!\n");
    return(NULL)
  }
  if(!("conf.high" %in% colnames(df))) {
    print("WARN: conf.high column is required to plot tidy coefficients!\n");
    return(NULL)
  }
  
  ggplot(
    df %>% arrange(p.value), 
    aes(estimate, term, xmin = conf.low, xmax = conf.high, height = 0)
  ) +
    geom_point(color = "forestgreen") +
    geom_vline(xintercept = 0, lty = 4, color = "black") +
    geom_errorbarh(color = "forestgreen") +
    theme_bw() +
    labs(y = "Coeficientes β", x = "Estimación") +
    ggtitle("Diferencia de coeficientes β contra el cero")
}

coefficients_summary <- function(model, show_tidy_sumamry = TRUE) {
  tidy_sumamry <- tidy(model, conf.int = TRUE)

  if(show_tidy_sumamry) {
    printTable(as.data.frame(tidy_sumamry))
  }

  model_summary <- model_coefficients_summary(tidy_sumamry)
  if(!is.null(model_summary)) {
    printTable(as.data.frame(model_summary))
  }
  
  plot_tidy_coefficients(tidy_sumamry)
}

anova_summary <- function(model, p_value_threshold=0.05) {
  tidy(anova(model)) %>% 
    mutate(
      signiticativa = ifelse(is.na(p.value), NA,  ifelse(p.value < p_value_threshold, "Si", "No"))
    )
}


models_validation <- function(
  lineal_model, 
  bayesion_model, 
  params, 
  vars,
  test_set_1,
  test_set_2 = NULL
) {
  bayesion_predictor <- BayesianRegressionPredictor.from(bayesion_model, params, vars)
  
  if(is.null(test_set_2)) test_set_2 = test_set_1
  
  lineal_test_pred   <- predict(lineal_model, test_set_1) 
  bayesion_test_pred <- predict(bayesion_predictor, test_set_2)
  
  test_true <- test_set %>% dplyr::select(body_mass_g) %>% pull()
  
  data.frame(
    model = c('Bayesian Regression', 'Lineal Regression'),
    rmse = c(
      rmse(test_true, bayesion_test_pred),
      rmse(test_true, lineal_test_pred)
    ),
    mae = c(
      mae(test_true, bayesion_test_pred),
      mae(test_true, lineal_test_pred)
    )
  ) %>% arrange(mae)
}

lm_vs_br_models_validation <- function(
  lineal_model, 
  bayesion_model, 
  params, 
  vars,
  test_set_1,
  test_set_2 = NULL
) {
  bayesion_predictor <- BayesianRegressionPredictor.from(bayesion_model, params, vars)
  
  if(is.null(test_set_2)) test_set_2 = test_set_1
  
  lineal_test_pred   <- predict(lineal_model, test_set_1) 
  bayesion_test_pred <- predict(bayesion_predictor, test_set_2)
  
  test_true <- test_set %>% dplyr::select(body_mass_g) %>% pull()
  
  data.frame(
    model = c('Bayesian Regression', 'Lineal Regression'),
    rmse = c(
      rmse(test_true, bayesion_test_pred),
      rmse(test_true, lineal_test_pred)
    ),
    mae = c(
      mae(test_true, bayesion_test_pred),
      mae(test_true, lineal_test_pred)
    )
  ) %>% arrange(mae)
}


lm_vs_lm_models_validation <- function(
  model_a, 
  model_b, 
  test_set_1,
  test_set_2 = NULL
) {
  if(is.null(test_set_2)) test_set_2 = test_set_1
  
  test_pred_a   <- predict(model_a, test_set_1) 
  test_pred_b <- predict(model_b, test_set_2)
  
  test_true <- test_set %>% dplyr::select(body_mass_g) %>% pull()

  data.frame(
    model = c('Modelo A', 'Modelo B'),
    rmse = c(
      rmse(test_true, test_pred_a),
      rmse(test_true, test_pred_b)
    ),
    mae = c(
      mae(test_true, test_pred_a),
      mae(test_true, test_pred_b)
    )
  ) %>% arrange(mae)
}




lm_vs_br_coeficients <- function(lineal_model, bayesion_model, params) {
  bayesian_coef <- params %>% 
    map(~get_posterior_mean(bayesion_model, par=.x)[4]) %>% 
    unlist()
  
  df <- cbind(as.data.frame(lineal_model$coefficients), bayesian_coef[1:(length(bayesian_coef)-1)])
  colnames(df) <- c('Regresión Lineal', 'Regresión Bayesiana')
  df  
}

lm_vs_lm_coeficients <- function(lineal_model_a, lineal_model_b) {
  df <- cbind(as.data.frame(lineal_model_a$coefficients), lineal_model_b$coefficients)
  colnames(df) <- c('Regresión Lineal A', 'Regresión Lineal B')
  df  
}

br_coeficients <- function(model, params) {
  data.frame(
    Coeficiente = params,
    Valor = params %>% 
      map(function(it) get_posterior_mean(model, par=it)[4]) %>%
      unlist()
  )
}


br_vs_br_coeficients <- function(model_1, mode_2, params) {
  coef_1 <- params %>% 
    map(~get_posterior_mean(model_1, par=.x)[4]) %>% 
    unlist()
  
  coef_2 <- params %>% 
    map(~get_posterior_mean(mode_2, par=.x)[4]) %>% 
    unlist()

  data.frame(
    Coeficiente = params,
    'Modelo A' = coef_1,
    'Modelo B' = coef_2
  )
}

train_test_split <- function (df, train_size = 0.7, shuffle = TRUE) 
{
  if (shuffle) {
    train_ind <- sample(seq_len(nrow(df)), size = (nrow(df) * train_size))
    train_set <- df[train_ind, ]
    test_set <- df[-train_ind, ]
  }
  else {
    train_set <- df[1:abs(nrow(df) * train_size), ]
    test_set <- df[abs(nrow(df) * train_size):nrow(df), 
    ]
  }
  print(paste("Train set size:", nrow(train_set)))
  print(paste("Test set size:", nrow(test_set)))
  list(train_set, test_set)
}
