genDumbell <- function(data) {
  # Data preparation
  data <- data %>%
    group_by(label_var) %>%
    mutate(order_value = ifelse(Country == "Thailand", values, NA)) %>%
    ungroup() %>%
    arrange(order_value) %>%
    mutate(label_var = factor(label_var, levels = unique(label_var)))
  
  # Add a background_fill column to alternate background colors
  data <- data %>%
    mutate(background_fill = as.factor(as.numeric(label_var) %% 2)) # Alternating values (0, 1)
  
  # Calculate mean values for each label_var
  mean_values <- data %>%
    group_by(label_var) %>%
    summarize(mean_value = mean(values, na.rm = TRUE)) %>%
    ungroup()
  
  # Join the mean values back to the main data
  data <- data %>%
    left_join(mean_values, by = "label_var")
  
  # Adjust the x position to move the "Region Average" label further to the right
  # and set y to align it vertically with the axis labels
  # Create the plot
  plot <- ggplot(data) +
    # Add alternating background colors using geom_tile
    geom_tile(aes(x = 0.5, y = reorder(label_var, -order_value), fill = background_fill),
              width = 1, height = 1, alpha = 0.1) +
    scale_fill_manual(values = c("0" = "grey90", "1" = "white"), guide = "none") +
    
    # Plot layers for lollipop segments
    geom_segment(aes(x = tapply(values, label_var, min)[label_var],
                     xend = values, 
                     y = label_var, 
                     yend = label_var), 
                 color = "grey", size = 1) +
    
    # Points for each country with shapes and opacity for Thailand
    geom_point(aes(x = values, 
                   y = reorder(label_var, -values),
                   shape = Country == "Thailand",
                   alpha = Country == "Thailand",
                   color = Country),
               size = 5) +
    
    # Display Thailand values as text labels
    geom_text(data = subset(data, Country == "Thailand"),
              aes(x = values, y = reorder(label_var, -values), label = round(values, 2)),
              hjust = 0.45, vjust = -2, size = 4, 
              family = "Lato Bold", color = "#524F4C") +
    
    # Add mean value as a text ribbon on the right
    geom_text(data = mean_values, 
              aes(x = 1.15, y = reorder(label_var, -mean_value), 
                  label = round(mean_value, 2)),
              hjust = 0, vjust = 0.5, size = 3.5, 
              family = "Lato bold", color = "black") +
    
    # Manually add vertical lines only up to the "1" position
    geom_vline(xintercept = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
               color = "#d1cfd1", size = 0.5, linetype = "dashed") +
    
    # Shape and color scales
    scale_shape_manual(values = c(18, 16)) +
    scale_alpha_manual(values = c(0.5, 1)) +
    scale_color_manual(values = colors4plot) +
    
    # Coordinate adjustments with "Region Average" added to x-axis labels
    scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.175),
                       labels = c("0", "0.2", "0.4", "0.6", "0.8", "1", "Regional \nAverage"),
                       limits = c(0, 1.3), position = "top") +
    coord_cartesian(clip = "off") +
    
    # Custom theme and layout
    WJP_theme() +
    theme(legend.position = "none",
          panel.grid.major.x = element_blank(),  # Remove default major grid lines
          panel.grid.major.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_markdown(family = "Lato Medium",
                                         size = 3.5 * .pt,
                                         color = "Black", hjust = 0,
                                         lineheight = 1.3),
          plot.margin = margin(5, 5, 5, 0))
  
  # Print the plot
  plot
  
  return(plot)
}
