## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Thematic Report - Settings
##
## Author(s):         Isabella Coddington   (icoddington@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     June 7, 2024
##
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Required packages                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(pacman)
p_load(char = c(
  # Visualizations

  
  # Data Loading
  "haven", "writexl", "openxlsx", "janitor",
  
  
  # Utilities
  "margins", "kableExtra", "glue",
  
  # Good 'ol Tidyverse
  "tidyverse"
  
))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  SharePoint Path                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(Sys.info()["user"] == "icoddington"){
  path2DA <- "/Users/icoddington/OneDrive - World Justice Project/Data Analytics"
} else if (Sys.info()["user"] == "spardo"){
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Loading Fonts                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

path2fonts<- file.path(
  path2DA, "6. Country Reports/0. Fonts", 
  fsep = "/"
)
font_add(family     = "Lato Full",
         regular    = file.path(path2fonts, "Lato-Regular.ttf", fsep = "/"),
         italic     = file.path(path2fonts, "Lato-LightItalic.ttf", fsep = "/"),
         bold       = file.path(path2fonts, "Lato-Bold.ttf", fsep = "/"),
         bolditalic = file.path(path2fonts, "Lato-BoldItalic.ttf", fsep = "/"))
font_add(family  = "Lato Light",
         regular = file.path(path2fonts, "Lato-Light.ttf", fsep = "/"))
font_add(family  = "Lato Black",
         regular = file.path(path2fonts, "Lato-Black.ttf", fsep = "/"))
font_add(family  = "Lato Black Italic",
         regular = file.path(path2fonts, "Lato-BlackItalic.ttf", fsep = "/"))
font_add(family  = "Lato Medium",
         regular = file.path(path2fonts, "Lato-Medium.ttf", fsep = "/"))
showtext_auto()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  WJP theme                                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

WJP_theme <- function() {
  theme(panel.background   = element_blank(),
        plot.background    = element_blank(),
        panel.grid.major   = element_line(size     = 0.25,
                                          colour   = "#5e5c5a",
                                          linetype = "dashed"),
        panel.grid.minor   = element_blank(),
        axis.title.y       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(0, 10, 0, 0)),
        axis.title.x       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(10, 0, 0, 0)),
        axis.text.y        = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C"),
        axis.text.x = element_text(family = "Lato Full",
                                   face   = "plain",
                                   size   = 3.514598*.pt,
                                   color  = "#524F4C"),
        axis.ticks  = element_blank(),
        plot.margin  = unit(c(0, 0, 0, 0), "points")
  ) 
}


