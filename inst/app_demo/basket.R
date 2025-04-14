
library(data.table)

selected_codes <- products_full[products %in% input_markets][,- c("products_spot_code")][
    nchar(products_fwd_code) == 2, products_fwd_code := paste0(products_fwd_code, "B")]

HPFC::spot_PWR_products_full$

xlsx_path = file.path('inst', 'app_demo', 'market_data_sample.xlsx')

DATA_4 = as.data.table(readxl::read_excel(xlsx_path, sheet = 4))
expected_colnames = c("RIC", "DATE", 'CLOSE', 'VOLUME', 'STATUS', 'ZONE', 'EST_DELIVERY', 'YEAR', 'MONTH', 'ID', 'TIME_CODE', 'TIME_STAMP', 'TYPE', 'CLASS')
# expected_types = c("character", "Date", "character", "character", "numeric", "character", "Date", "numeric", "numeric", 'character', 'character', 'character', 'character', 'character')
colnames_match = all(colnames(DATA_4) == expected_colnames)
# types_match = all(sapply(DATA_4, class) == expected_types)
check_DATA_4 = colnames_match #&& types_match

DATA_4

DTc = merge(as.data.table(DATA_4), parameters_basket[, .(COMMODITY = id, ZONE = value)], by = 'ZONE', all.x = TRUE, allow.cartesian = TRUE)

dt_series_contcal = DTc

commodity_basis_basket_range = list(as.Date(min(dt_series_contcal$DATE)), as.Date(max(dt_series_contcal$DATE)))

DT_full = dt_series_contcal[DATE >= commodity_basis_basket_range[1][[1]] & DATE <= commodity_basis_basket_range[2][[1]]]
DTS = DT_full[ZONE %chin% c(commodity_main, commodity_basis_basket)]

obj = basket_selection(DT = DTS, mk_comm_0 = commodity_main, full = TRUE)
list_autobasket = obj

list_autobasket$coef_glm 

DT = parameters_basket

DTW = as.data.table(list_autobasket$coef_glm)
DTW = merge(DTW, DT[, .(ZONE = value, COMMODITY = id)], by = 'ZONE', all = TRUE)
DT_auto_basket = unique(DTW[, .(id = COMMODITY, basket = 'basis_model', value = ZONE, coef = round(coeff * 100), weight = round(weight, 2))])
DT = rbind(DT[, .(value, id, basket, coef, weight)], DT_auto_basket[!is.na(coef), .(value, id, basket, coef, weight)])
DT[, coeff := coef / 100]
DT[, ZONE := value]
DT = unique(DT[!(basket == 'basis_model' & id == 'mk_comm_0')])

setorder(DT, basket)