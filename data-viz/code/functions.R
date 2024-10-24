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
  
  
  # three radars
  
if (type == "radar"){
    latestYear <- "2024"
    colors4plot <- c(
      "2024" = "#2A2A94", 
      "2015" = "#A90099" 
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
  
  # Save chart locally
  saveIT(
    chart = chart,
    figid = figid,
    w = 230,
    h = 210
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

wrangleData <- function(figid){
  # names of indicators
  metric_labels <- c(
    "1.5" = "Non-Governmental Checks<br> of Government Power",
    "3.3" = "Civic Participation",
    "3.4" = "Access to Complaint<br> Mechanisms",
    "3.2" = "Right to <br> Information",
    "4.3" = "Due Process of<br>the Law and Rights<br> of the Accused",
    "7.4" = "Improper<br> Government Influence<br> in Civil Justice",
    "8.6" = "Improper<br> Government Influence<br> in Criminal Justice",
    "7.1" = "Access to <br> and Afforability<br> of Civil Justice",
    "8.4" = "Impartiality in <br> Criminal Justice",
    "4.1" = "Equality<br> and Discrimination",
    "7.2" = "Discrimination<br> in Civil Justice",
    "4.8" = "Labor Rights",
    "6.4" = "Due Process is <br> Respected in<br>Administrative Proceedings",
    "4.2" = "The Right to <br>Life and Security of<br>the Person is Effectively<br> Guaranteed",
    "6.3" = "Administrative <br>proceedings are conducted <br>without unreasonable delay",
    "4.7" = "Freedom of <br> Assembly and Association",
    "4.4" = "Freedom of <br> Expression and <br> Opinion",
    "4.5" = "Freedom of <br> Belief and Religion",
    "4.6" = "Freedom from <br> Interference with <br> Privacy",
    "6.5" = "No expropriation <br>without adequate <br>compensation"
    
  )
  
  dumbell_wrapped_metric_labels <- sapply(metric_labels, function(x) str_wrap(x, width = 30))
  
  # pull all variables for each chart
  chart <- outline %>% filter(id %in% figid) %>%
    pull(id)

  variables <- outline %>% 
    filter(id %in% chart) %>%
    select(!c("id","section","type")) %>%
    select(where(~ !all(is.na(.)))) %>% 
    as.list()
  
  variables <- as.character(unlist(variables))
  
  type <- outline %>% filter(id %in% figid) %>%
    pull(first(type))
  
  # for dumbells, all countries and 2024 vals only
  if (type == "dumbell") {
    thailand_report <- master_data %>%
      filter(Year == 2024) %>%
      select(Country, Year, all_of(variables)) %>%
      pivot_longer(cols = !c(Country, Year), 
                   names_to = "Metric", values_to = "values")
    
    
    data2plot <- thailand_report %>%
      mutate(
        label_var = recode(Metric, !!!dumbell_wrapped_metric_labels)
      ) %>%
      group_by(Metric) %>%
      mutate(
        mean_value = mean(values)
      )
  }
  
  if (type == "radar") {
    data2join <- master_data %>%
      filter(Year %in% c("2015", "2024") & Country == 'Thailand') %>%
      select(Year, all_of(variables)) %>%
      pivot_longer(
        cols = all_of(variables), 
        names_to = "Metric", 
        values_to = "Value"
      ) %>%
      pivot_wider(names_from = Year, values_from = Value) %>%
      rename(Value_2015 = `2015`, Value_2024 = `2024`) %>%
      pivot_longer(
        cols = starts_with("Value"), 
        names_to = "Year", 
        values_to = "Value",
        names_prefix = "Value_"
      ) %>%
      # order var to use wjp_radar
      mutate(
             label_var = as.character(recode(Metric, !!!metric_labels)),
             Metric = as.factor(Metric), 
             figure = if_else(Year == "2024", paste0(round(Value,2)),  NA_character_),
             label_var = ifelse(Year == 2024, label_var, NA),
             latestYear = "2024"
             )%>%
      arrange(Metric, Year, Value) %>%
      group_by(Year) %>%
      arrange(-Value) %>%
      mutate(
        order_var = if_else(Year == "2024", row_number(), NA_real_)
      ) %>%
      ungroup()
    
    
    figure2.df <- data2join %>% 
      mutate(
        figure2 = if_else(Year == "2015", paste0(round(Value,2)),  NA_character_)
      ) %>%
      drop_na(figure2) %>%
      select(Metric, figure2) 
    
    order_value <- data2join %>%
      drop_na(order_var) %>%
      select(Metric, order_var)
    
    data2plot <- data2join %>%
      left_join(figure2.df, by = c("Metric")) %>%
      left_join(order_value, by = c("Metric")) %>%
      mutate(
        category = label_var,
        across(label_var,
               ~paste0(
                 "<span style='color:#2a2a9A;font-size:3.514598mm;font-weight:bold'>", figure, "</span>",
                 "<span>", " | " ,"</span>", 
                 "<span style='color:#a90099;font-size:3.514598mm;font-weight:bold'>", figure2, "</span>",
                 "<br>",
                 "<span style=‘color:#524F4C;font-size:3.514598mm;font-weight:bold’>", 
                 label_var,
                 "</span>")
        ),
        figure2 = if_else(Year == "2015",  NA_character_, figure2),
        label_var = if_else(Year == "2015",  NA_character_, label_var),
        ) %>%
      arrange(-as.numeric(Year)) %>%
      select(!order_var.x) %>%
      rename(order_var = order_var.y)
    
  }
  
  
    return (data2plot)
}

