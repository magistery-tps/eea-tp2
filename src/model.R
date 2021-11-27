library(pacman)
p_load(tidyverse, tidymodels, compareGroups, MASS, caret)


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

train_test_eval_metric_summary <- function(
  models, 
  test_set = NULL, 
  truth_column='peso', 
  metric_fn = rmse,
  include_r2 = TRUE
) {
  train_eval <- eval_metric_summary(
    models, 
    truth_column = truth_column, 
    metric_fn    = metric_fn
  ) %>% rename(train_error = .estimate)
  
  if(include_r2) {
    train_eval$r_2_adjusted <- models %>% 
      map(function(m) { glance(m)$adj.r.squared }) %>% 
      unlist()
  }
  
  test_eval <- eval_metric_summary(
    models, 
    truth_column = truth_column, 
    metric_fn    = metric_fn,
    test_set     = test_set
  ) %>% rename(test_error = .estimate)
  
  train_eval %>%
    inner_join(test_eval, by= 'model') %>%
    mutate(error_diff = abs(train_error - test_error)) 
}





