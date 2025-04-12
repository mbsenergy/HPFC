
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
        brand = file.path('body', '_brand.yml'),
        font_scale = 0.85
    )

## Sourcer ----------------------------------------------------------------

# Set up ===============================================================

source(file.path('body', "01_inputs.R"))
source(file.path('body', "02_ui.R"))
source(file.path('body', "03_server.R"))

 
# API ------------------------------------------
PLEASE_INSERT_REUTERS_KEY = read.xlsx("PLEASE INSERT REUTERS KEY.xlsx", sheet = 1, colNames = FALSE)[2,1]

