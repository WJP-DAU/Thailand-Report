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
    chart <- wjp_radar(
      data = data_points[[figid]],
      axis_var = 'Metric',
      target_var = "Value",
      color_var = "Year",
      maincat = latestYear,
      label_var = 'label_var',
      colors = c("#EF709D", "#3772FF"),
      order_var = 'order_var'
    )
  }
  
  # Save chart locally
  saveIT(
    chart = chart,
    figid = figid,
    w = 200.7883,
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
    "4.1" = "Equal treatment and\n absence of\n discrimination" ,
    "4.3" = "Due process of the law\n and rights of \nthe accused",
    "4.5" = "Freedom of belief and\n religion is \neffectively guaranteed",
    "4.6" = "Freedom from arbitrary\n interference with privacy is \n effectively guaranteed",
    "4.8" = "Fundamental labor\n rights are \neffectively guaranteed",
    "6.5" = "The government does\nnot expropriate without \n lawful process and \nadequate compensation",
    "1.5" = "Government powers are\n subject to non-governmental checks",
    "3.2" =  "Right to information",
    "3.3" = "Civic participation",
    "3.4" = "Complaint mechanisms",
    "4.4" = "Freedom of opinion\n and expression is\n effectively guaranteed",
    "4.7" = "Freedom of assembly\n and association is\n effectively guaranteed",
    "4.3" = "Due process of the\n law and rights of\n the accused",
    "6.3" = "Administrative proceedings are\n conducted without \n unreasonable delay",
    "8.7" = "Due process of the law\n and rights of\n the accused",
    "7.1" = "People can access and\n afford civil justice",
    "7.4" = "Civil justice is free of\n improper government influence",
    "8.6" = "Criminal system is free\n of improper government\n influence",
    "8.4" = "Criminal system is impartial",
    "4.2" = "The right to \nlife and security\n of the person is \neffectively guaranteed"
  )
  
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
             Metric = as.factor(Metric), 
             across(label_var,
                    ~paste0(
                      "<span style=‘color:#2a2a9A;font-size:3.514598mm;font-weight:bold’>", Value, 
                        "</span>", "<br>",
                      "<span style=‘color:#524F4C;font-size:3.514598mm;font-weight:bold’>", 
                        label_var,
                      "</span>")
             ),
             # unique label var
             label_var = ifelse(Year == 2024, label_var, NA),
             latestYear = "2024"
      )
  }
  
  
    return (data2plot)
}

