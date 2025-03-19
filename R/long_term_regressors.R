
#' Create regressors in the daily DT to estimate the model
#'
#' The Macro trend is derived using HP filter
#'
#' @param x A daily DT with date, hp_trend and detr_smp_day
#' @param y a parameter for weight
#' @returns A DT with 41 columns
#' @import data.table
#' @export

long_term_regressor = function(DT, alpha){

  # check formati

    # Check if DT is a data.table
    if (!"data.table" %in% class(DT)) {
      stop(red("Error: Input is not a data.table"))
  }

  if(class(alpha) != 'numeric' | alpha > 1 | alpha < 0) {stop("alpha must be format numeric between 0 and 1")}

  DTW = copy(DT)

  DTW[,`:=` (yday = data.table::yday(date),
                         wday = data.table::wday(date),
                         quarter = data.table::quarter(date),
                         month = data.table::month(date),
                         weekend = as.numeric(chron::is.weekend(date)),
                         obs = .I)]

  #create weight with smoothing
  DTW[, time_distance := (max(obs) + 1 - obs) / max(obs)] #TO CHECK / max(obs) su 365*3
  DTW[, weight := exp(-alpha * (time_distance))]


  # create day dummies (intra-week seasonality)
  DTW[, (paste("day", 1:7, sep = "_")) := lapply(1:7, function(i) {fifelse(wday == i, 1, 0)})]

  #create summer dummy
  DTW[, summer := as.numeric(quarter == 2 | quarter == 3)]

  #create variable beginning season date AND delta date-beginning
  DTW[, begining_season := as.Date(
    fifelse(quarter == 4, paste0(format(date, format = "%Y"), "-10-01"),
            fifelse(quarter == 1, paste0(data.table::year(date) - 1, "-10-01"),
                    paste0(format(date, format = "%Y"), "-04-01"))))]

  DTW[, dist := as.numeric(date - begining_season)]
  DTW[, yday_season := dist / max(dist)]

  #create variable season.year
  DTW[, season := fifelse(summer == 1, paste0("summer-", data.table::year(date)),
                                      fifelse(quarter == 4, paste0("winter-", data.table::year(date)),
                                              paste0("winter-", data.table::year(date) - 1)))]


  #### create LT components Caldana 6.2
  DTW[,`:=`(cos_long_term = cos((2 * pi) * yday / 365),
                        cos_season = cos((2 * pi) * yday_season),
                        cos_season_summer = cos((2 * pi) * yday_season) * summer,
                        sin_long_term = sin((2 * pi) * yday / 365),
                        sin_season = sin((2 * pi) * yday_season),
                        sin_season_summer = sin((2 * pi) * yday_season) * summer)]

  ### generate polynomial via lapply
  DTW[, (paste("yday", 1:10, sep = "_")) := lapply(1:10, function(i) {yday^i})]

  return(DTW)

}

