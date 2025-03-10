
# Packages
library(data.table)


## 0. Time Objects ------------------------------------------------------------------------------

months_code = c("MF", "MG", "MH", "MJ", "MK", "MM", "MN", "MQ", "MU", "MV", "MX", "MZ")
years_code_gas = 'YZ'
years_code_pwr = 'YF'

## 1. POWER --------------------------------------------------------------------------------------

countries = c('Greece', 'Germany', 'Hungary', 'France', 'Spain', 'Uk', 'Bulgaria', 'Italy', 'Romania', 'Czech Republic')
countries_2d = c('GR', 'DE', 'HU', 'FR', 'ES', 'UK', 'BU', 'IT', 'RO', 'CZ')

### FORWARD ---------------------------------------------------------------
products_PWR_code = c('FF', 'DE', 'F9', 'F7', 'FE', 'FU', 'FK', 'FD', 'FH', 'FX') 
type_code = c('B', 'P')
quarters_code = c("QF", "QJ", "QN", "QV") # differ from gas
eikon_PWR_products = as.data.table(cbind(countries, products_PWR_code))
setnames(eikon_PWR_products, names(eikon_PWR_products), c("country", "code"))

### SPOT ---------------------------------------------------------------
spot_PWR_code = c('HEEGRAUCH', 'EHLDE', 'HPXH', 'PNX', 'OMELES', 'EHLGB', 'IBEIDMEUR', 'GMEIT', 'OPCOMRTR', 'OTECZEUR')
spot_PWR_products = as.data.table(cbind(products_PWR_code, spot_PWR_code))
setnames(spot_PWR_products, names(spot_PWR_products), c("forward_code", "spot_code"))
spot_PWR_products_full = as.data.table(cbind(countries_2d, countries, products_PWR_code, spot_PWR_code))

## 2. GAS --------------------------------------------------------------------------------------

products_GAS = c('TTF', 'PSV', 'NBP', 'NCG')

### FORWARD ---------------------------------------------------------------
products_GAS_code = c('TFMB', 'IGAF', 'NGLN', 'NGM')
eikon_GAS_products = as.data.table(cbind(products_GAS, products_GAS_code))
setnames(eikon_GAS_products, names(eikon_GAS_products), c("product", "code"))
quarters_code_gas = c("QH", "QM", "QU", "QZ") # differ from power

### SPOT ---------------------------------------------------------------
spot_GAS_code = c('TTFDA', 'TRITPSVDA', 'TRGBNBPD1', 'TRDENCGDA')
spot_GAS_products = as.data.table(cbind(products_GAS_code, spot_GAS_code))
setnames(spot_GAS_products, names(spot_GAS_products), c("forward_code", "spot_code"))
spot_GAS_products_full = as.data.table(cbind(products_GAS, products_GAS_code, spot_GAS_code))

# Save datasets in the 'data/' directory
usethis::use_data(spot_GAS_products_full, spot_PWR_products_full, overwrite = TRUE)