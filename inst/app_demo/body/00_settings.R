
# SETUP ------------------------------------------------------------------------------------------------- 

library(shiny)
library(shinyjs)
library(bslib)        
library(echarts4r)     
library(reactable)     
library(shinycssloaders) 
library(magrittr)
library(openxlsx)
library(react)
# library(HPFC)
devtools::load_all()

mbs_theme =
    bs_theme(
        version = 5,
        # preset = 'zephyr',
        brand = file.path('body', '_brand.yml'),
        # navbar_light_bg = "#001437", 
        # navbar_dark_bg = "#001437",
        # danger = '#DE8969',
        font_scale = 0.9,
        # "navbar-nav-link-padding-x" = "0px",
        # "navbar-nav-link-padding-y" = "0px"
    )

## Sourcer ----------------------------------------------------------------

# Set up ===============================================================

source(file.path('body', "01_inputs.R"))
source(file.path('body', "02_ui.R"))
source(file.path('body', "03_server.R"))

 
# API ------------------------------------------
PLEASE_INSERT_REUTERS_KEY = read.xlsx("PLEASE INSERT REUTERS KEY.xlsx", sheet = 1, colNames = FALSE)[2,1]

