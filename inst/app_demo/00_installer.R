# install_packages.R

# CRAN packages
cran_packages = c(
    "shiny",
    "shinyjs",
    "bslib", # AT LEAST VERSION 0.9.0
    "echarts4r",
    "toastui",
    "shinycssloaders",
    "magrittr",
    "openxlsx",
    "knitr",
    "chron",
    "lubridate",
    "glmnet",
    "neverhpfilter",
    "mFilter",
    "changepoint",
    "crayon",
    "httr",
    "jsonlite",
    "tidyjson",
    "glue",
    "data.table"
)

# Install missing CRAN packages
installed = rownames(installed.packages())
to_install = setdiff(cran_packages, installed)
if (length(to_install) > 0) {
    install.packages(to_install)
}

# GitHub package: react (if not already installed)
if (!requireNamespace("react", quietly = TRUE)) {
    remotes::install_github("dreamRs/react")
}

remotes::install_github("hadley/emo")
remotes::install_github("mbsenergy/eikondata")
remotes::install_github("mbsenergy/HPFC")
