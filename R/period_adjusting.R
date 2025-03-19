
#' Adjust the Long seasonality estimated in the calendar according to the forward structure
#'
#' The regrossrs are cos, sin, yday, weekend ...
#'
#' @param x An hourly DT calendar with date, long term seasonality and forward structure.
#' @param y A string or date with the last date available for spot price history
#' @returns A hourly DT with 15 columns to be used for hourly estimation
#' @import data.table
#' @export


period_adjusting = function(DT, last_date) {

  if (!('forward_month_BL' %in% colnames(DT)) | class(DT$forward_month_BL) != 'numeric') {stop("forward_month_BL column must be format numeric")
  } else if (!('forward_quarter_BL' %in% colnames(DT)) | class(DT$forward_quarter_BL) != 'numeric') {stop("forward_quarter_BL column must be format numeric")
  } else if (!('L_t' %in% colnames(DT)) | class(DT$L_t) != 'numeric') {stop("L_t column must be format numeric")
  } else if (!('BL_prev_m' %in% colnames(DT)) | class(DT$BL_prev_m) != 'numeric') {stop("BL_prev_m column must be format numeric") }

  DTW = copy(DT)

  #### create period of reference forward
  DTW[, period := fcase(
    is.na(forward_month_BL) & is.na(forward_quarter_BL) & !is.nan(BL_prev_m), paste("mese", month, year, sep = ''),
    is.na(forward_month_BL) & is.na(forward_quarter_BL) & is.nan(BL_prev_m), paste('remainder_year', year, sep = ''),
    is.na(forward_month_BL) & !is.na(forward_quarter_BL), paste('remainder_quarter', quarter, year, sep = ''),
    !is.na(forward_month_BL), paste("mese", month, year, sep = ''))
  ]

  begin_current_period = as.Date(paste(substr(last_date, 1, 7), '01', sep = "-"))

  #### detrend L.t by fwd period
  DTW[, period := fifelse(date < begin_current_period, 'history', period)]
  DTW[, history_forecast := fifelse(date <= last_date, 0, 1)]

  DTW[, L_t := L_t - mean(L_t), by = period]

  DTW[,':='
         (forward_month_BL=NULL,
           forward_quarter_BL=NULL,
           BL_prev_m=NULL)
  ]

  return(DTW)

}

