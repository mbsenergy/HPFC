
#' Creates calendar regressor to calibrate the model in the future
#'
#' The regrossrs are cos, sin, yday, weekend ...
#'
#' @param x A daily dataframe calendar with date and holiday.
#' @returns A dataframe with 31 columns
#' @export

create_calendar = function(dataframe){

  if (!('date' %in% colnames(dataframe)) | class(dataframe$date) != 'Date') {stop("date column must be format Date")}
  if (!('holiday' %in% colnames(dataframe)) | class(dataframe$holiday) != 'numeric') {stop("date column must be format numeric")}

  calendar=copy(dataframe)


  #### prepare forecast horizon grid
  calendar[,`:=`(month = as.numeric(data.table::month(date)),
                 year = as.numeric(data.table::year(date)),
                 quarter = as.numeric(data.table::quarter(date)))]

  calendar[, yday := data.table::yday(date)]
  calendar[, weekend := as.numeric(chron::is.weekend(date))]
  calendar[, wday := lubridate::wday(date, label = TRUE)]
  calendar[, wday2 := data.table::wday(date)]

  #### filter forecast interval

  #### create dummy for week day
  calendar[, unlist(lapply(1:7, function(i) {paste("day", i , sep = "_")})) := lapply(1:7, function(i) {fifelse(data.table::wday(date) == i, 1, 0)})]

  #### create dummy for summer, season, beginning, dist, yday
  calendar[, summer := as.numeric(quarter == "2" | quarter == "3")]

  #### create dummy for season (j1st quarter start from previous year)
  calendar[,season := fcase(
    summer == 1, paste('summer', year, sep = '-'),
    quarter == 1, paste('winter', as.numeric(year) - 1, sep = '-'),
    quarter == 4, paste('winter', year, sep = '-'))]

  #### create beginning season
  calendar[, begining_season := as.Date(fcase(
    summer==1, paste(year, '04-01',sep='-'),
    quarter==1, paste(as.numeric(year)-1, '10-01',sep='-'),
    quarter==4, paste(year, '10-01',sep='-')))]

  #### create time passed by beginning season
  calendar[, dist := as.numeric(date - begining_season)]
  calendar[, yday_season := dist / max(dist)]

  #### create yday polynomial
  calendar[, (paste("yday", 1:10, sep = "_")) := lapply(1:10, function(i) {yday^i})]

  return(calendar)

}
