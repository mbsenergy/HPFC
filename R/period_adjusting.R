
#' Adjust the Long seasonality estimated in the calendar according to the forward structure
#'
#' The regrossrs are cos, sin, yday, weekend ...
#'
#' @param x An hourly dataframe calendar with date, long term seasonality and forward structure.
#' @param y A string or date with the last date available for spot price history
#' @returns A hourly dataframe with 15 columns to be used for hourly estimation
#' @import data.table
#' @export


period_adjusting=function(dataframe,last_date){

  if (!('forward_month_BL' %in% colnames(dataframe)) | class(dataframe$forward_month_BL) != 'numeric') {stop("forward_month_BL column must be format numeric")
  } else if (!('forward_quarter_BL' %in% colnames(dataframe)) | class(dataframe$forward_quarter_BL) != 'numeric') {stop("forward_quarter_BL column must be format numeric")
  } else if (!('L_t' %in% colnames(dataframe)) | class(dataframe$L_t) != 'numeric') {stop("L_t column must be format numeric")
  } else if (!('BL_prev_m' %in% colnames(dataframe)) | class(dataframe$BL_prev_m) != 'numeric') {stop("BL_prev_m column must be format numeric") }


  Lt_day = copy(dataframe)

  #### create period of reference forward
  Lt_day[, period := fcase(
    is.na(forward_month_BL) & is.na(forward_quarter_BL) & !is.nan(BL_prev_m), paste("mese", month, year, sep = ''),
    is.na(forward_month_BL) & is.na(forward_quarter_BL) & is.nan(BL_prev_m), paste('remainder_year', year, sep = ''),
    is.na(forward_month_BL) & !is.na(forward_quarter_BL), paste('remainder_quarter', quarter, year, sep = ''),
    !is.na(forward_month_BL), paste("mese", month, year, sep = ''))
  ]


  #kc_cols(period, c('year', 'month', 'period'))

  begin_current_period = as.Date(paste(substr(last_date, 1, 7), '01', sep = "-"))

  #### merge period to L_t day
  #period[,`:=`(year = as.character(year), month = as.character(month))]
  #Lt_day = period[Lt_day, on = c('year', 'month')]

  #### detrend L.t by fwd period
  Lt_day[, period := fifelse(date < begin_current_period, 'history', period)]
  Lt_day[, history_forecast := fifelse(date <= last_date, 0, 1)]

  Lt_day[, L_t := L_t - mean(L_t), by = period]

  Lt_day[,':='
         (forward_month_BL=NULL,
           forward_quarter_BL=NULL,
           BL_prev_m=NULL)
  ]

  return(Lt_day)

}

