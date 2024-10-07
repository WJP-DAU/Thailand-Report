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
    chart <- wjp_radar(
      data = data_points[[figid]],
      axis_var = 'Metric',
      target_var = "Value",
      color_var = "Year",
      maincat = "2015",
      label_var = 'label_var',
      colors = c("#EF709D", "#3772FF"),
      order_var = 'order_var'
    )
  }
  
  # Save chart locally
  saveIT(
    chart = chart,
    figid = figid,
    w = 189.7883,
    h = 168.7007
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
    "4.1" = "Equal treatment and\n absence of discrimination" ,
    "4.3" = "Due process of the law\n and rights of the accused",
    "4.5" = "Freedom of belief and\n religion is effectively guaranteed",
    "4.6" = "Freedom from arbitrary\n interference with privacy is \n effectively guaranteed",
    "4.8" = "Fundamental labor\n rights are \neffectively guaranteed",
    "6.5" = "The government does\nnot expropriate without \n lawful process and adequate compensation",
    "1.5" = "Government powers are\n subject to non-governmental checks",
    "3.2" =  "Right to information",
    "3.3" = "Civic participation",
    "3.4" = "Complaint mechanisms",
    "4.4" = "Freedom of opinion and\n expression is effectively guaranteed",
    "4.7" = "Freedom of assembly and\n association is effectively guaranteed",
    "4.3" = "Due process of the\n law and rights of the accused",
    "6.3" = "Administrative proceedings are\n conducted without \n unreasonable delay",
    "8.7" = "Due process of the law\n and rights of the accused",
    "7.1" = "People can access and\n afford civil justice",
    "7.4" = "Civil justice is free of\n improper government influence",
    "8.6" = "Criminal system is free of\n improper government influence",
    "8.4" = "Criminal system is impartial",
    "4.2" = "The right to life and security\n of the person is \neffectively guaranteed"
  )
  
  # pull all variables for each chart
  chart <- outline %>% filter(id %in% figid) %>%
    pull(id)

  variables <- outline %>% 
    filter(id %in% chart) %>% 
    select(var_id1, var_id2, var_id3, var_id4, var_id5, var_id6) %>% as.list()
  
  
  if (figid %in% c("F5", "F6")) {
    variables <- c(variables, var_id7 = 8.4)
  }
  
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
        label_var = recode(Metric, !!!metric_labels)
        
      )
  }
  
  if (type == "radar") {
    data2plot <- master_data %>%
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
      mutate(order_var = row_number(),
             label_var = as.character(recode(Metric, !!!metric_labels)),
             Metric = as.factor(Metric) 
      )
  }
  
  
    return (data2plot)
}

