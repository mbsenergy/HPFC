devtools::load_all()
library(data.table)
library(echarts4r)
library(magrittr)
library(htmlwidgets)


time_range = as.numeric(data.table::year(as.Date('2017-01-02'))):as.numeric(data.table::year(as.Date('2024-12-31')))
calendar = HPFC::calendar_holidays
calendar[,`:=` (year = as.character(data.table::year(date)), quarter = as.character(data.table::quarter(date)), month = as.character(data.table::month(date)))]

# dt_fwds_pwr = HPFC::dt_fwds_pwr[substr(RIC, 1, 2) == 'FF']
dt_fwds_pwr = HPFC::dt_fwds_pwr
dt_fwds_pwr = dt_fwds_pwr[dt_fwds_pwr[, .I[date == max(date)], by = RIC]$V1]
dt_fwds_pwr = dt_fwds_pwr[, .(trade_close = mean(as.numeric(trade_close))), by = .(date = fluxer::to_yymm(date), RIC)]
dt_fwds_pwr[, products_PWR_code := substr(RIC, 1, 2)]

dt_fwds_pwr = merge(dt_fwds_pwr, HPFC::spot_PWR_products_full[, .(spot_PWR_code, products_PWR_code)], by = 'products_PWR_code', all.x = TRUE)

dt_spot_pwr = HPFC::dt_spot_pwr[, .(date, smp, spot_PWR_code = RIC)]
dt_spot_pwr = dt_spot_pwr[, .(smp = mean(smp, na.rm=TRUE)), by = .(date = fluxer::to_yymm(date), spot_PWR_code)]

dt_fwds_pwr = merge(dt_fwds_pwr, dt_spot_pwr, by = c('date', 'spot_PWR_code'), all.x = TRUE)
dt_fwds_pwr = dt_fwds_pwr[, .(date, spot_PWR_code, products_PWR_code, RIC, FWD = trade_close, DAM = smp)]

dt_fwds_pwr_fwddam = copy(dt_fwds_pwr)
usethis::use_data(dt_fwds_pwr_fwddam)

dt_fwds_pwr_fwd = dt_fwds_pwr[, .(value = FWD, quote = RIC)]
dt_fwds_pwr_dam = dt_fwds_pwr[, .(value = DAM, quote = RIC)]


dt_fwd_pwr_prep_fwd = HPFC::prep_fwd_curve(
  DT = dt_fwds_pwr_fwd[substr(quote, 1, 2) == 'FF'],
      list_rics = c('FF'),
      type = 'PWR',
      start_date = '2017-01-02',
      end_date = '2024-12-31', 
      calendar_sim = calendar
)
      

dt_fwd_pwr_prep_dam = HPFC::prep_fwd_curve(
  DT = dt_fwds_pwr_dam[substr(quote, 1, 2) == 'FF'],
      list_rics = c('FF'),
      type = 'PWR',
      start_date = '2017-01-02',
      end_date = '2024-12-31', 
      calendar_sim = calendar
)



# Compute difference
dt_fwds_pwr[, diff := FWD - DAM]

# Aggregate by date if needed
dt_fwds_pwr_agg = dt_fwds_pwr[, .(mean_diff = round(mean(diff, na.rm=TRUE), 2)), by = .(date, spot_PWR_code)]


selected_codes = c("GMEIT", "HEEGRAUCH", "OPCOMRTR")
spot_codes = unique(dt_fwds_pwr_agg$spot_PWR_code)

selected_vector = setNames(spot_codes %in% selected_codes, spot_codes)

p = 
dt_fwds_pwr_agg %>% 
  group_by(spot_PWR_code) %>% 
  e_charts(date) %>% 
  e_line(mean_diff, symbol = 'none') %>% 
  e_tooltip(trigger = "axis") %>% 
  e_title("Forward vs. Day-Ahead Market Price Difference") %>% 
  e_x_axis(name = "Date") %>% 
  e_y_axis(name = "Difference (€/MWh)", max = 100) %>%  
  e_theme("westeros") %>% 
  e_datazoom(type = "slider", x_index = 0) %>%  
  e_datazoom(type = "slider", y_index = 0) %>%  
  e_legend(selected = selected_vector) 

p

saveWidget(p, "data-raw/forward_vs_dam_plot.html", selfcontained = TRUE)
