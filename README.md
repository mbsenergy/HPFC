# HPFC  
**Hybrid Power & Fuel Curve Forecasting**

---

## Overview

`HPFC` is an R package to model, train, and forecast both gas and power market prices using a hybrid curve modeling approach. The workflow is controlled via a `params.json` file, ensuring modularity and reproducibility.

The package integrates with `eikonapir` for data retrieval, and supports long-term and short-term model components for both commodities.

---

## Installation

```r
remotes::install_github("mbsenergy/HPFC")
```

