library(pacman)
p_load(tidyverse, tidymodels, compareGroups, sjmisc)


load_dataset <- function() {
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')
}


missings_summary <- function(
  df, 
  missings = c(NA, NULL, 'Dato perdido')
) {
  df %>% 
    gather(., key = Variable, value = value) %>%
    mutate(is_missing = if_else(value %in% missings, 1, 0)) %>%
    group_by(Variable) %>% 
    summarise(
      `Nª Categorias` = n_distinct(value),
      `Nª Faltantes`   = sum(is_missing),
      `% Faltantes`    = round(`Nª Faltantes` / nrow(df) * 100, 2)
    ) %>%
    arrange(desc(`% Faltantes`), `Nª Categorias`)
}

missings_columns <- function(ds, column_max_missings = 0.5) {
  missings_summary(ds) %>% 
    dplyr::filter(`% Faltantes` > (column_max_missings * 100)) %>%
    dplyr::select(Variable) %>%
    dplyr::pull()
}


show_values <- function(df , columns=c()) {
  if(is_empty(columns)) {
    columns <- df %>% colnames()
  }
  for(column in columns) {
    printTable(df %>% group_by(!!sym(column)) %>% tally())
  }
}

important_variables_set <- function(result, top=2, metrics=c("%IncMSE", "IncNodePurity")) {
  metrics %>% 
    map(function(metric) top_acc_features(result, top, metric) ) %>% 
    unlist() %>% 
    unique()
}

outliers <- function(df, column) {
  bp <- boxplot(df %>% pull(!!sym(column)))
  out_inf <- bp$stats[1]
  out_sup <- bp$stats[5]
  
  list(
    inf = df %>% filter(!!sym(column) <= out_inf) %>% pull(!!sym(column)) %>% unique() %>% sort(),
    sup = df %>% filter(!!sym(column) >= out_sup) %>% pull(!!sym(column)) %>% unique() %>% sort()
  )
}

dummify <- function(df, vars, suffix = 'label') {
  df %>%
    to_dummy(vars, suffix = suffix) %>%
    bind_cols(df) %>%
    dplyr::select(-vars) %>%
    rename_all(tolower)
}

cat_col_names <- function(df) df %>% select_if(negate(is.numeric)) %>% colnames()

