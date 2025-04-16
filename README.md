# HPFC  
**Hybrid Power & Fuel Curve Forecasting**

---

## Overview

`HPFC` is an R package to model, train, and forecast both gas and power market prices using a hybrid curve modeling approach. The workflow is controlled via a `params.json` file, ensuring modularity and reproducibility.

The package integrates with `eikondata` for data retrieval, and supports long-term and short-term model components for both commodities.

---

## Installation

```r
remotes::install_github("mbsenergy/HPFC")
```

## Run the App

```r
shiny::runApp(system.file("app_demo", package = "HPFC"))
```

## Dependencies thorough isntallation

```r
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

remotes::install_github("mbsenergy/eikondata")
remotes::install_github("mbsenergy/HPFC")

```