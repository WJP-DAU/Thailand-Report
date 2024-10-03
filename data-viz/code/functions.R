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
          # replace with path to thailand folder
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
  
  if (type == "bars"){
    chart <- genBars(data4chart)
  }
  
  if (type == "slope"){
    chart <- genSlope(data4chart)
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
  var <- outline %>% filter(id %in% figid) %>%
    pull(var_id)
  
  type <- outline %>% filter(id %in% figid) %>%
    pull(type)
  var <- gsub(" ", "_", var)    
  var <- gsub("-", "_", var) 
  var <- gsub("\\.", "_", var)  
  var <- paste0("x", var)
  var <- tolower(var)
  
  # for bars, all countries and 2024 vals only
  if (type == "bars"){
    data2plot <- master_data %>% filter(
      year == "2024"
    ) %>% select(country, all_of(var)) %>% rename(
      value2plot = var
    )
  }
  
  if (type == "slope"){
    data2plot <- master_data %>% filter(
      (country == 'Thailand')) %>%
        select(
          country, 
          year, 
          all_of(var)) %>%
        pivot_wider(names_from = year, 
                    values_from = all_of(var), 
                    names_prefix = "Year_") %>%
        rename(
          value2plot_2023 = Year_2023,
          value2plot_2024 = Year_2024
        )
  }
  
  return (data2plot)
}

