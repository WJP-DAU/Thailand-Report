thailand_bars <- function(
    data,               # Data frame
    target,             # Column name for y-axis values
    grouping,           # Column name for x-axis groups
    labels   = NULL,    # Optional column for text labels
    colors   = NULL,    # Column name to determine colors by group
    color_palette = c("2020" = "#A90099", "2024" = "#2A2A94") # Colors to map for fill
) {
  
  # Rename columns in the data frame for simplicity
  data <- data %>%
    rename(
      target_var   = all_of(target),
      grouping_var = all_of(grouping)
    )
  
  # Optional label column renaming
  if (!is.null(labels) && labels %in% names(data)) {
    data <- data %>% rename(labels_var = all_of(labels))
  } else {
    data <- data %>% mutate(labels_var = "")
  }
  
  # # Optional colors column renaming or default assignment
  # if (!is.null(colors) && colors %in% names(data)) {
  #   data <- data %>% rename(colors_var = all_of(colors))
  # } else {
  #   data <- data %>% mutate(colors_var = "default")
  # }
  
  # Create the bar plot
  plt <- ggplot(data, aes(x = grouping_var, y = target_var, fill = grouping_var, label = labels_var)) +
    geom_bar(stat = "identity", width = 0.8, show.legend = FALSE) +
    geom_text(aes(y = target_var), vjust = -0.5, color = "black", fontface = "bold") +
    scale_fill_manual(values = color_palette) +
    labs(y = NULL, x = NULL) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "#D0D1D3"))
  
  return(plt)
}


