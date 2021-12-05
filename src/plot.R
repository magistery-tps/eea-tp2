library(pacman)
p_load(tidyverse, GGally, plotly, ggcorrplot, ggmosaic)

hist_plots <- function(df, columns=c(), bins = c()) {
  if(is_empty(columns)) {
    columns <- df %>% dplyr::select(is.numeric) %>% colnames()
  }
  if (is_empty(bins)) {
    bins <- rep(20, times = length(columns))
  }
  
  bins_index = 1
  for(col in columns) {
    bins_size <- bins[bins_index]
    p <- ggplot(df, aes(!!sym(col))) + 
      geom_histogram(
        col="red", 
        aes(fill=..count..), 
        alpha    = .80, 
        position = "dodge", 
        bins     = bins[bins_index]
      ) +
      scale_fill_gradient("Frecuencia", low="yellow", high="red") +
   
      theme(
        axis.text.x      = element_text(vjust = 1, hjust = 1), 
        legend.position  = "none"
      ) +
      ylab('Frecuencia')
    print(p)
    bins_index <- bins_index + 1
  }
}

box_plots <- function(df, title=NULL) {
  df %>% 
    pivot_longer(is.numeric, names_to = "Variables", values_to = "Frecuencia") %>%
    ggplot(aes(x = Variables, y = Frecuencia, fill = Variables)) +
    scale_y_continuous(limits = c(0, 180)) +
    geom_boxplot(width=0.7) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position = "none") +
    labs(title = title)
}

segmented_box_plot <- function(
  df, 
  column, 
  segmented_by,
  title    = NULL,
  y_label  = NULL,
  x_label  = NULL,
  y_limits = c(0, 1000)
) {
  ggplot(df, aes(x = fct_reorder(!!sym(segmented_by), !!sym(column), .desc = T), y = !!sym(column))) + 
    geom_boxplot(outlier.shape = NA, alpha = 0.75, aes(fill = !!sym(segmented_by)), position = "dodge") + 
    theme_minimal() + 
    theme(legend.position = 'none')+
    labs(y = y_label, x = x_label)  +
    scale_y_continuous(limits = y_limits) +
    ggtitle(title) +
    theme(
      axis.text.x = element_text(face="italic", colour="dark grey", size = 10, angle = 0),
      legend.position = "none"
    )
}

bar_plots <- function(
  df, 
  columns    = c(),
  width      = 0.9, 
  count_size = 2
) {
  if(is_empty(columns)) {
    columns <- df %>% dplyr::select(!is.numeric) %>% colnames()
  }
  for(column in columns) {
    p <- ggplot(
      df %>% 
        group_by(!!sym(column)) %>% 
        tally() %>%
        mutate(Frecuencia = n), 
      aes(x=!!sym(column), y=Frecuencia, fill=!!sym(column))
    ) +
      geom_col(stat="identity", width=width, position = "dodge") +
      geom_text(aes(label=Frecuencia), vjust=2, color="white", size=count_size) + 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position = "none")
    print(p)
  }
}

segmented_bar_plots <- function(df, columns, segmented_by) {
  for(column in columns) {
    r <- df %>%
      group_by(!!sym(segmented_by)) %>% 
      tally(!!sym(column)) %>%
      ggplot(aes(x=!!sym(segmented_by), y=n, fill = !!sym(segmented_by))) +
      geom_bar(position="dodge", stat="identity") +
      xlab(segmented_by) + 
      ylab(column) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position = "none")
    print(r)
  }
}

mosaic_plot <- function(df, column, segmented_by, title=NULL) {
  ggplot(df) +
  geom_mosaic(aes(x=product(!!sym(column)), fill=!!sym(segmented_by))) +
  theme(axis.text.x = element_text(angle=90, hjust=.1, vjust=-0.3), legend.position = "none") +
  labs(title = title)
}

segmented_pairs_plot <- function(df, segment_column) {
  df %>% 
    ggpairs(
      cardinality_threshold=100,
      aes(color = !!sym(segment_column)),
      progress = FALSE,
      upper = list(continuous = wrap("cor", size = 3, hjust=0.7)),
      legend = 3,
      labeller = label_wrap_gen(50)
    ) +
    theme(axis.text.x = element_text(angle=45, vjust=0.5), legend.position = "bottom")
}

pairs_plot <- function(df) {
  df %>% 
    ggpairs(
      cardinality_threshold=100,
      progress = FALSE,
      upper = list(continuous = wrap("cor", size = 3, hjust=0.7)),
      legend = 3,
      labeller = label_wrap_gen(50)
    ) +
    theme(axis.text.x = element_text(angle=45, vjust=0.5), legend.position = "bottom")
}

corr_plot <- function(df, title=NULL) {
  num_df <- df %>% select_if(is.numeric)
  ggcorrplot(
    cor(num_df),
    hc.order = TRUE,
    type     = "upper",
    insig    = "blank",
    # Compute a matrix of correlation p-values
    p.mat    = cor_pmat(num_df),
    lab      = TRUE,
    colors   = c("#6D9EC1", "white", "#E46726")
  ) +
  labs(title = title)
}

plot_fit <- function(
  model,
  test_set, 
  y_column = 'body_mass_g',  
  x_column = 'flipper_length_mm',
  title    = 'Largo de aleta vs. Masa corporal',
  xlabel   = 'Largo de aleta (mm)',
  ylabel   = 'Masa corporal (g)',
  se       = FALSE
) {
  df <- test_set %>%
    add_column(y_pre = predict(model, test_set))
  
  ggplot(df) + 
    geom_point(aes(x=!!sym(x_column), y=!!sym(y_column)), color='black') + 
    geom_smooth(aes(x=!!sym(x_column),  y=y_pre), method='lm', formula = y ~ x, se = se) +
    ggtitle(title) +
    xlab(xlabel) +
    ylab(ylabel)
}

plot_compare_fit <- function(
  model_1,
  model_2,
  df_inputs_1,
  df_inputs_2 = NULL, 
  x_column    = 'body_mass_g',
  y_column    = 'flipper_length_mm',
  label_1     = 'Modelo 1',
  label_2     = 'Modelo 2',
  title       = 'Masa corporal vs. Longitug de la aleta',
  xlabel      = 'Masa corporal (g)',
  ylabel      = 'longitud de la aleta (mm)',
  se          = TRUE
) {
  if(is.null(df_inputs_2)) {
    df_inputs_2 <- df_inputs_1
  }

  df <- df_inputs_1 %>%
    add_column(y_pre_1 = predict(model_1, df_inputs_1)) %>% 
    add_column(y_pre_2 = predict(model_2, df_inputs_2))
  
  Modelo <- label_1
  
  ggplot(df) + 
    geom_point(aes(x=!!sym(x_column), y=!!sym(y_column)), color='black') + 
    geom_smooth(aes(x=y_pre_1, y=!!sym(y_column), colour = Modelo), method='lm', formula = y ~ x, se = se) + 
    geom_smooth(aes(x=y_pre_2, y=!!sym(y_column), colour = label_2), method='lm', formula = y ~ x, se = se) +
    scale_colour_manual(values=c("blue","red")) +
    ggtitle(title) +
    xlab(xlabel) +
    ylab(ylabel)
  
}

plot_data <- function(
  data,
  x_column = 'body_mass_g',
  y_column = 'flipper_length_mm',
  title    = 'Masa corporal vs. Longitug de la aleta',
  xlabel   = 'Masa corporal (g)',
  ylabel   = 'longitud de la aleta (mm)',
  se       = TRUE
) {
  ggplot(data, aes(x=!!sym(x_column), y=!!sym(y_column))) + 
    geom_point() +
    geom_smooth(method=lm, formula = y ~ x, se = se) +
    ggtitle(title) +
    xlab(xlabel) +
    ylab(ylabel)
}
