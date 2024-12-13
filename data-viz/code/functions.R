## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Thailand Country Report - Functions
##
## Author(s):         Isabella Coddington         (icoddington@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     September 29th, 2024
##
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:  
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  General functions                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

resetDirs <- function(){
  
  # List and reset previous outputs
  prevOutputs <- list.files(
    "outputs", 
    include.dirs = F, 
    full.names   = T, 
    recursive    = T
  )
  file.remove(prevOutputs)
}

saveIT <- function(chart, figid, w, h) {
  ggsave(
    plot   = chart,
    file   = 
      file.path(
        path2DA,
        paste0(
          "6. Country Reports/Thailand-Report-2024/output/",
          figid, ".svg"
        ),
        fsep = "/"
      ),
    width  = w, 
    height = h,
    units  = "mm",
    dpi    = 72,
    device = "svg"
  )
} 

callVisualizer <- function(figid){
  print(glue("Working on chart {figid}"))
  
  type <- outline %>%
    filter(id == figid) %>%
    pull(type)
  
  data4chart <- data_points[[figid]]
  
  # three dumbells
  if (type == "dumbell"){
    chart <- genDumbell(data4chart)
  }
  
  

if (type == "radar"){
    latestYear <- "2024"
    colors4plot <- c(
      "2024" = "#2A2A94", 
      "2020" = "#A90099" 
    )
    
    chart <- wjp_radar(
      data = data_points[[figid]],
      axis_var = 'Metric',
      target_var = "Value",
      color_var = "Year",
      maincat = latestYear,
      label_var = 'label_var',
      colors = colors4plot,
      order_var = 'order_var'
    );chart
}
  
  if (type == "slope"){
    colors4plot <- c(
      "4.8" = "#2A2A94", 
      "6.5" = "#A90099" 
    )
    chart <- thailand_slope(
      data     = data4chart,
      target   = "Value",
      grouping = "Year",
      ngroups  = "Metric",
      colors   = "Metric",
      cvec     = colors4plot,
      labels   = "label",
      repel    = FALSE,
      ptheme   = WJP_theme()
    )
    
  }
  
  if (type == "bars"){
    chart <- thailand_bars(
      data          = data4chart,
      target        = "Value",
      grouping      = "Year",
      labels        = "label",        # Optional, shows text on bars if provided
      colors        = "Year"
    )
    
  }
  
  if (figid == "F4"){
    height = 190
  } else if (figid %in% c("F6_A", "F6_B", "F3", "F5")){
    
    height = 90
    
  } else{
    height = 210
  }
  
  # Save chart locally
  saveIT(
    chart = chart,
    figid = figid,
    w = 230,
    h = height
  )
  
  return(
    list(
      "plot" = chart,
      "data" = data4chart
    )
  )
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Wrangling function                                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wrangleData <- function(figid) {
  # Names of indicators
  metric_labels <- c(
    "1.5" = "Non-governmental checks<br> of government power",
    "3.3" = "Civic participation",
    "3.4" = "Access to complaint<br> mechanisms",
    "3.2" = "Right to <br> information",
    "4.3" = "Due process of<br>the law and rights<br> of the accused",
    "7.4" = "Improper<br> government influence<br> in civil justice",
    "8.6" = "Improper<br> government influence<br> in criminal justice",
    "7.1" = "Access to <br> and affordability<br> of civil justice",
    "8.4" = "Impartiality in <br> criminal justice",
    "4.1" = "Equality<br> and discrimination",
    "7.2" = "Discrimination<br> in civil justice",
    "4.8" = "Labor rights",
    "6.4" = "Due process is <br> respected in<br>administrative proceedings",
    "4.2" = "The right to <br>life and security of<br>the person is effectively<br> guaranteed",
    "6.3" = "Administrative <br>proceedings are conducted <br>without unreasonable delay",
    "4.7" = "Freedom of <br> assembly and association",
    "4.4" = "Freedom of <br> expression and <br> opinion",
    "4.5" = "Freedom of <br> belief and religion",
    "4.6" = "Freedom from <br> interference with <br> privacy",
    "6.5" = "Protection from <br>expropriation without <br>due process"
  )
  
  # Format wrapped labels for dumbbells
  dumbell_wrapped_metric_labels <- sapply(metric_labels, function(x) str_wrap(x, width = 30))
  
  # Pull all variables for the chart
  chart <- outline %>% filter(id %in% figid) %>%
    pull(id)
  
  variables <- outline %>%
    filter(id %in% chart) %>%
    select(!c("id", "section", "type")) %>%
    select(where(~ !all(is.na(.)))) %>%
    as.list()
  
  variables <- as.character(unlist(variables))
  
  # Determine chart type
  type <- outline %>% filter(id %in% figid) %>%
    pull(first(type))
  
  # For dumbbell plots (2024 values only)
  if (type == "dumbell") {
    thailand_report <- master_data %>%
      filter(Year == 2024) %>%
      select(Country, Year, all_of(variables)) %>%
      pivot_longer(cols = !c(Country, Year), names_to = "Metric", values_to = "values")
    
    data2plot <- thailand_report %>%
      mutate(label_var = recode(Metric, !!!dumbell_wrapped_metric_labels)) %>%
      group_by(Metric) %>%
      mutate(mean_value = mean(as.numeric(values), na.rm = TRUE))
  }
  
  # For radar plots (2015 and 2024 values)
  if (type == "radar") {
    
    data2join <- master_data %>%
      filter(Year %in% c(2020, 2024) & Country == 'Thailand') %>%
      select(Year, all_of(variables)) %>%
      pivot_longer(cols = all_of(variables), names_to = "Metric", values_to = "Value") %>%
      mutate(Value = as.numeric(Value)) %>%  # Convert Value to numeric
      pivot_wider(names_from = Year, values_from = Value) %>%
      rename(Value_2020 = `2020`, Value_2024 = `2024`) %>%
      pivot_longer(cols = starts_with("Value"), names_to = "Year", values_to = "Value", names_prefix = "Value_") %>%
      mutate(
        label_var = recode(Metric, !!!metric_labels),
        Metric = as.factor(Metric),
        figure = if_else(Year == "2024", sprintf("%.2f", Value), NA_character_),  # Use sprintf() for two decimals
        latestYear = "2024"
      ) %>%
      arrange(Metric, Year, Value) %>%
      group_by(Year) %>%
      arrange(desc(Value)) %>%
      mutate(order_var = if_else(Year == "2024", row_number(), NA_real_)) %>%
      ungroup()
    
    figure2.df <- data2join %>%
      mutate(figure2 = if_else(Year == "2020", sprintf("%.2f", Value), NA_character_)) %>%  # Use sprintf() for two decimals
      drop_na(figure2) %>%
      select(Metric, figure2)
    
    order_value <- data2join %>%
      drop_na(order_var) %>%
      select(Metric, order_var)
    
    data2plot <- data2join %>%
      left_join(figure2.df, by = "Metric") %>%
      left_join(order_value, by = "Metric") %>%
      mutate(
        category = label_var,
        across(label_var, ~paste0(
          "<span style='color:#2a2a9A;font-size:3.514598mm;font-weight:bold'>", figure, "</span>",
          "<span> | </span>",
          "<span style='color:#a90099;font-size:3.514598mm;font-weight:bold'>", figure2, "</span><br>",
          "<span style='color:#524F4C;font-size:3.514598mm;font-weight:bold'>", label_var, "</span>"
        )),
        figure2 = if_else(Year == "2020", NA_character_, figure2),
        label_var = if_else(Year == "2020", NA_character_, label_var)
      ) %>%
      arrange(desc(as.numeric(Year))) %>%
      select(-order_var.x) %>%
      rename(order_var = order_var.y)

  }
  
  # For slope plots (2015 and 2024 values)
  if (type %in% c("slope", "bars")) {
    data2plot <- master_data %>%
      filter((Year %in% c(2020, 2024)) & (Country == "Thailand") ) %>%
      select(Year, all_of(variables)) %>%
      pivot_longer(cols = all_of(variables), names_to = "Metric", values_to = "Value") %>%
      mutate(
        label = paste0(round(Value, 2))
      ) %>%
      arrange(Year)
    
  }
  
  return(data2plot)
}



