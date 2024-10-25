thailand_slope <- function(
    data,                    
    target,             
    grouping,
    ngroups,                 
    labels         = NULL,
    colors,
    cvec           = NULL,
    repel          = FALSE,
    ptheme         = WJP_theme()
){
  
  # Renaming variables in the data frame to match the function naming
  if (is.null(labels)) {
    data <- data %>%
      dplyr::mutate(labels_var    = "") %>%
      rename(target_var    = all_of(target),
             grouping_var  = all_of(grouping),
             colors_var     = all_of(colors))
  } else {
    data <- data %>%
      rename(labels_var    = all_of(labels),
             target_var    = all_of(target),
             grouping_var  = all_of(grouping),
             colors_var    = all_of(colors))
  } 
  
  # Ensure proper grouping by color (Metric) to connect the lines properly
  data <- data %>%
    mutate(
      grouping_var = as.numeric(grouping_var),  # Convert Year to numeric
      labpos = case_when(
        grouping_var == min(data$grouping_var) ~ grouping_var - 0.5,
        grouping_var == max(data$grouping_var) ~ grouping_var + 0.5,
      )
    )
  
  # Creating the plot
  plt <- ggplot(data, 
                aes(x = grouping_var,
                    y = target_var,
                    color = colors_var,
                    label = labels_var,
                    group = colors_var)) +  # Group by color (Metric)
    geom_point(size = 3, show.legend = F) +
    geom_line(size = 1, show.legend = F)
  
  # Adding text labels, optionally using geom_text or ggrepel
  if (repel == F) {
    plt <- plt + geom_text(aes(y = target_var,
                               x = labpos,
                               label = labels_var),
                           family = "Lato Full",
                           fontface = "bold",
                           size = 3.5,
                           show.legend = F)
  } else {
    plt <- plt + ggiraph::geom_label_repel_interactive(aes(y = target_var,
                                                           x = labpos,
                                                           label = labels_var),
                                                       family = "Lato Full",
                                                       fontface = "bold",
                                                       size = 3.5,
                                                       show.legend = F,
                                                       min.segment.length = 1000,
                                                       seed = 42,
                                                       direction = "y",
                                                       force = 5,
                                                       force_pull = 1)
  }
  
  # Apply color palette
  plt <- plt +
    scale_x_continuous(
      n.breaks = 2,
      breaks = data %>% ungroup() %>% distinct(grouping_var) %>% pull(grouping_var)
    ) +
    scale_y_continuous(limits = c(0, 1.05),
                       expand = c(0, 0),
                       breaks = seq(0, 1, 0.20),
                       labels = paste0(seq(0, 1, 0.2))) +
    scale_color_manual(values = cvec) +
    ptheme +
    theme(
      panel.grid.major.x = element_line(color = "#ACA8AC",
                                        linetype = "solid",
                                        linewidth = 0.75),
      panel.grid.major.y = element_blank(),
      axis.line.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank() 
    )
  
  return(plt)
}


