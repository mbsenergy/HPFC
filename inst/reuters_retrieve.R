
# SETUP ----------------------------------------------------

box::use(data.table[...],
         magrittr[...])

devtools::load_all()


eikonapir::set_proxy_port(9000L)
eikonapir::set_app_id(as.character(Sys.getenv('REUTERS_KEY')))



# 0. Reuters  General ----------------------------------------
ric = 'TTFDA'
from_date = Sys.Date() - 30
to_date = Sys.Date()

dt_rics_d = eikonapir::get_timeseries(ric, start_date = Sys.Date() - (365 * 10), end_date = Sys.Date(), interval = 'daily')
dt_rics_d = get_rics(rics = ric, from_date = Sys.Date() - (365 * 10), to_date = Sys.Date(), interval = 'daily')


# 1. SPOT ----------------------------------------------------

## Gas ----------------
ric = 'TTFDA'
from_date = Sys.Date() - 30
to_date = Sys.Date()
type = 'GAS'


dt_spot_gas = retrieve_spot(ric, from_date, to_date, type)
print(dt_spot_gas)



## PWR ----------------
ric = 'FD'
from_date = Sys.Date() - 30
to_date = Sys.Date()
type = 'PWR'


dt_spot_pwr = retrieve_spot(ric, from_date, to_date, type)
print(dt_spot_pwr)




# 1. FWD ----------------------------------------------------
