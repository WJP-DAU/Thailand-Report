## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Thailand Country Report - RunMe File
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
## Outline: This is the single-call file for generating data points and visualizations for the EU Thematic
##          reports. 
##          
##          PRESETTINGS:  Load settings and function definitions, data
##          WRANGLE DATA: Create chart lists which are passed to the appropriate wrangle data function to acquire 
##                        data points for visualizations.
##          CREATE VIZ:   Call and apply the appropriate visualization function. 
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Presettings                                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# load modules
modules <- c(
  "settings", "functions", "ThailandDumbells", "thailandSlope","radarChart") # copy of radar -- 
                                                             # pushed some minor changes 
                                                             # to WJPr but old version was still showing


for (mod in modules){
  source(
    paste0("code/",mod,".R")
  )
}


# load data - replace with SP path
master_data <- read.xlsx(
  file.path(path2DA,
    "6. Country Reports/Thailand-Report-2024/data-viz/inputs/FINAL_2024_wjp_rule_of_law_index_HISTORICAL_DATA_FILE.xlsx"
  ),
  sheet = "Historical Data",
  check.names = FALSE
) %>%
  filter(
    # (Year %in% c("2023", "2024") & 
       Country %in% c(
         "Thailand",
         "Malaysia", 
         "Indonesia",
         "Thailand", 
         "Vietnam", 
         "Philippines", 
         "Myanmar", 
         "Cambodia")
        
     )  %>%  
  rename_with(
    ~ ifelse(grepl("^[0-9]", .x), substr(.x, 1, 3), .x) # renaming with numerical indicator
  )
    

# load outline from SP
outline <- read.xlsx(
  file.path(path2DA,paste0("6. Country Reports/Thailand-Report-2024/data-viz/inputs/thailand_outline.xlsx")),
  sheet = "figure_map"
)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Wrangle data                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

chart_list <- setNames(
  as.list(outline %>% pull(id)), 
  outline %>% pull(id))
  

data_points <- map(chart_list, function(chart) {
  # Call the wrangleData function for each chart id
  wrangled_data <- wrangleData(figid = chart)
  return(wrangled_data)
})

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Create Viz                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Calling the visualizer for each chart
lapply(
  chart_list,
  callVisualizer
)

