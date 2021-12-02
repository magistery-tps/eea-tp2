# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, tidyverse, tidymodels, compareGroups, MASS, caret, Metrics)
setwd(this.path::this.dir())
p_load_gh('adrianmarino/commons')
#
import('./bayesian_regression_predictor.R')
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

models_validation <- function(lineal_model, bayesion_model, params, vars, test_set) {
  bayesion_predictor <- BayesianRegressionPredictor.from(bayesion_model, params, vars)
  
  bayesion_test_pred <- predict(bayesion_predictor, test_set)
  lineal_test_pred   <- predict(lineal_model, test_set) 
  
  test_true <- test_set %>% dplyr::select(body_mass_g) %>% pull()
  
  data.frame(
    model = c('Lineal Regression', 'Bayesian Regression'),
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
