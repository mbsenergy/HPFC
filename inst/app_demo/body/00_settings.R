
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
PLEASE_INSERT_REUTERS_KEY = read.xlsx("PLEASE INSERT REUTERS KEY.xlsx", sheet = 1, colNames = FALSE)[2,1]

mbs_theme =
    bs_theme(
        version = 5,
        bootswatch = 'zephyr',
        primary = '#001437',
        secondary = '#dee2e6',
        success = '#2EB67D',
        info = '#0E9DD3',
        warning = '#f4bd61',
        danger = '#DE8969',
        base_font = font_google(family = "Inter"),
        heading_font = font_google(family = "Inter"),
        font_scale = 0.9,
        "navbar-nav-link-padding-x" = "0px",
        "navbar-nav-link-padding-y" = "0px"
    )

#bs_theme_preview(nordquant_theme)

bs_theme_update(mbs_theme, `enable-rounded` = FALSE)


## Sourcer ----------------------------------------------------------------

# Set up ===============================================================

source(file.path('body', "01_inputs.R"))
source(file.path('body', "02_ui.R"))
source(file.path('body', "03_server.R"))
