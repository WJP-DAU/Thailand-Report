genDumbell <- function(data) 
  {
  data <- data %>% 
    arrange(ifelse(Country == "Thailand", values, NA)) %>%
    mutate(label_var = factor(label_var, levels = unique(label_var)))
  # Create the lollipop plot
  plot <- ggplot(data, aes(x = values, 
                                y = reorder(label_var, -values),
                                color = Country)) +
    # Lollipop line
    geom_segment(aes(x = 0, xend = values, y = label_var, yend = label_var), color = "grey", size = 1) +
    
    # Points with different shapes for Thailand and others
    geom_point(aes(shape = Country == "Thailand",  # TRUE for Thailand (circle), FALSE for others (diamond)
                   alpha = Country == "Thailand"), # No transparency for Thailand, others transparent
               size = 5) +
    
    # Add values for Thailand only
    geom_text(data = subset(data, Country == "Thailand"),
              aes(label = round(values, 2)),  # Show values with 2 decimal points
              hjust = 0.45,  # Adjust horizontal positioning of text
              vjust = -2,   # Adjust vertical positioning of text
              size = 4,    # Text size
              family = "Lato Bold") +
    
    # Customize point shapes: 18 = diamond, 16 = circle
    scale_shape_manual(values = c(18, 16)) +  
    scale_alpha_manual(values = c(0.5, 1)) +  # Transparency: 0.5 for others, 1 for Thailand
    
    # X-axis scale without percentages, just values between 0 and 1
    scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1), position = "top") +
    scale_color_manual(values = colors4plot) +
    coord_cartesian(clip = "off") +
    WJP_theme() +  # Custom theme
    theme(legend.position = "none",
          panel.grid.major.x = element_line(colour = "#d1cfd1", size = 0.5),
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
  
  return(plot)
}

