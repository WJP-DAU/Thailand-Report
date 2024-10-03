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
  
  # three dumbells
  if (type == "dumbells"){
    chart <- genDumbell(data4chart)
  }
  
  # three radars
  if (type == "radar"){
    chart <- genRadar(data4chart)
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
    data2plot <- master_data %>%
      filter(Year == "2024") %>%
      select(Country, all_of(variables)) 
  }
  
  
  if (type == "radar"){
    data2plot <- master_data %>% filter((Year %in% c("2015","2024")) &
      (Country == 'Thailand')) %>%
        select(
          Country, 
          Year, 
          all_of(variables)) %>%
        pivot_wider(names_from = Year, 
                    values_from = all_of(variables))
  }
  
  return (data2plot)
}

