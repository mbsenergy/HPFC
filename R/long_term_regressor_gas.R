
#' Create regressors in the daily dataframe to estimate the model
#'
#' The Macro trend is derived using HP filter
#'
#' @param x A daily dataframe with date, hp_trend, break_group_p and detr_smp_day
#' @param y a parameter for weight
#' @returns A dataframe with 41 columns
#' @import data.table
#' @export

long_term_regressor_gas = function(dataframe, alpha) {

  # check formati

  if (!('date' %in% colnames(dataframe)) | class(dataframe$date) != 'Date') {stop("date column must be format Date")
  } else if (!('hp_trend' %in% colnames(dataframe)) | class(dataframe$hp_trend) != 'ts') {stop("hp_trend column must be format ts")
  } else if (!('break_group_p' %in% colnames(dataframe)) | class(dataframe$break_group_p) != 'integer') {stop("break_group_p column must be format integer")
  } else if (!('detr_smp_day' %in% colnames(dataframe)) | class(dataframe$detr_smp_day) != 'ts') {stop("detr_smp_day column must be format ts")}

  if(class(alpha) != 'numeric' | alpha > 1 | alpha < 0){stop("alpha must be format numeric between 0 and 1")}

  filtered_ttf_dd = copy(dataframe)

  filtered_ttf_dd[,`:=` (yday = data.table::yday(date),
                         wday = data.table::wday(date),
                         quarter = data.table::quarter(date),
                         month = data.table::month(date),
                         weekend = as.numeric(chron::is.weekend(date)),
                         obs = .I)]

  #create weight with smoothing
  filtered_ttf_dd[, time_distance := (max(obs) + 1 - obs) / max(obs)] #TO CHECK / max(obs) su 365*3
  filtered_ttf_dd[, weight := exp(-alpha * (time_distance))]

  # create day dummies (intra-week seasonality)
  filtered_ttf_dd[, (paste("day", 1:7, sep = "_")) := lapply(1:7, function(i) {fifelse(wday == i, 1, 0)})]

  #create summer dummy
  filtered_ttf_dd[, summer := as.numeric(quarter == 2 | quarter == 3)]

  #create variable beginning season date AND delta date-beginning
  filtered_ttf_dd[, begining_season := as.Date(
    fifelse(quarter == 4, paste0(format(date, format = "%Y"), "-10-01"),
            fifelse(quarter == 1, paste0(data.table::year(date) - 1, "-10-01"),
                    paste0(format(date, format = "%Y"), "-04-01"))))]

  filtered_ttf_dd[, dist := as.numeric(date - begining_season)]
  filtered_ttf_dd[, yday_season := dist / max(dist)]

  #create variable season.year
  filtered_ttf_dd[, season := fifelse(summer == 1, paste0("summer-", data.table::year(date)),
                                      fifelse(quarter == 4, paste0("winter-", data.table::year(date)),
                                              paste0("winter-", data.table::year(date) - 1)))]


  #### create LT components Caldana 6.2
  filtered_ttf_dd[,`:=`(cos_long_term = cos((2 * pi) * yday / 365),
                        cos_season = cos((2 * pi) * yday_season),
                        cos_season_summer = cos((2 * pi) * yday_season) * summer,
                        sin_long_term = sin((2 * pi) * yday / 365),
                        sin_season = sin((2 * pi) * yday_season),
                        sin_season_summer = sin((2 * pi) * yday_season) * summer)]

  ### generate polynomial via lapply
  filtered_ttf_dd[, (paste("yday", 1:10, sep = "_")) := lapply(1:10, function(i) {yday^i})]

  # filtered_ttf_dd[,break_h:=fifelse(break_group_p==max(break_group_p),1,0)]
  #
  # filtered_ttf_dd[,`:=`(cos_long_term_p = break_group_p*cos_long_term,
  #                       cos_season_p = break_group_p*cos_season,
  #                       cos_season_summer_p = break_group_p*cos_season_summer,
  #                       sin_long_term_p = break_group_p*sin_long_term,
  #                       sin_season_p = break_group_p*sin_season,
  #                       sin_season_summer_p = break_group_p*sin_season_summer)]

  n_groups = max(filtered_ttf_dd$break_group_p) + 1
  filtered_ttf_dd[, (paste("break_group", 1:n_groups, sep = "_")) := lapply(1:n_groups, function(i) { as.numeric(break_group_p == (i - 1)) })]

  for (x in 1:n_groups){

    filtered_ttf_dd[, paste('cos_long_term', x,sep = '_')      := get(paste("break_group", x, sep = "_")) * cos_long_term]
    filtered_ttf_dd[, paste('cos_season', x,sep = '_')         := get(paste("break_group", x, sep = "_")) * cos_season]
    filtered_ttf_dd[, paste('cos_season_summer', x,sep = '_')  := get(paste("break_group", x, sep = "_")) * cos_season_summer]
    filtered_ttf_dd[, paste('sin_long_term', x,sep = '_')      := get(paste("break_group", x, sep = "_")) * sin_long_term]
    filtered_ttf_dd[, paste('sin_season', x,sep = '_')         := get(paste("break_group", x, sep = "_")) * sin_season]
    filtered_ttf_dd[, paste('sin_season_summer', x,sep = '_')  := get(paste("break_group", x, sep = "_")) * sin_season_summer]
    filtered_ttf_dd[, paste('holiday', x,sep = '_')            := get(paste("break_group", x, sep = "_")) * holiday]
    filtered_ttf_dd[, paste('summer', x,sep = '_')             := get(paste("break_group", x, sep = "_")) * summer]

    filtered_ttf_dd[, paste('day_2', x,sep = '_') := get(paste("break_group", x, sep = "_")) * day_2]
    filtered_ttf_dd[, paste('day_3', x,sep = '_') := get(paste("break_group", x, sep = "_")) * day_3]
    filtered_ttf_dd[, paste('day_4', x,sep = '_') := get(paste("break_group", x, sep = "_")) * day_4]
    filtered_ttf_dd[,  paste('day_5', x,sep = '_') := get(paste("break_group", x, sep = "_")) * day_5]
    filtered_ttf_dd[, paste('day_6', x,sep = '_') := get(paste("break_group", x, sep = "_")) * day_6]
    filtered_ttf_dd[, paste('day_7', x,sep = '_') := get(paste("break_group", x, sep = "_")) * day_7]

    filtered_ttf_dd[, (paste("yday", 1:10, x, sep = "_")) := lapply(1:10, function(i) { get(paste("break_group", x, sep = "_")) * get(paste("yday", i, sep = "_")) })]

  }

  return(filtered_ttf_dd)

}
