
#' Create regressors in the hourly DT to estimate the model
#'
#' creates calendar variable, break dummy and bl polynomial
#'
#' @param x A hourly DT with price produced by filter outlier hourly
#' @returns A DT with 49 columns
#' @import data.table
#' @export

hourly_regressors = function(DT) {

    if (!"data.table" %in% class(DT)) {
        stop(red("Error: Input is not a data.table"))
    }

  DTW = copy(DT)

  #create dummy for monthly, quarterly, weekend seasonality
  DTW[,`:=` (yday = data.table::yday(date),
                           wday = data.table::wday(date),
                           quarter = data.table::quarter(date),
                           month = data.table::month(date),
                           weekend = as.numeric(chron::is.weekend(date)),
                           obs = .I)]

DTW[, 
    season := fcase(
        quarter == 1, "winter",
        quarter == 2, "spring",
        quarter == 3, "summer",
        quarter == 4, "fall"
)]
    
DTW[, (paste0("season_", c("winter", "spring", "summer", "fall"))) := 
                          lapply(c("winter", "spring", "summer", "fall"), function(s) fifelse(season == s, 1, 0))]

  ## Hourly Dummies ---------------------------------------------------------------------------------------------

  ####create dummy for weekday
  DTW[, (paste("day", 1:7, sep = "_")) := lapply(1:7, function(i) {fifelse(wday == i, 1, 0)})]
  ####create dummy for hours
  DTW[, (paste("hour", 1:24, sep = "_")) := lapply(1:24, function(i) {fifelse(hour == i, 1, 0)})]
  #### create baseload
  DTW[, bl := mean(value), by=date]
  #### create last market regime period
  DTW[, break_h := fifelse(break_group_p == max(break_group_p), 1, 0)]
  DTW[, value_h := value - bl]

  DTW[, yday2 := yday^2]
  DTW[, yday3 := yday^3]
  DTW[, bl2 := bl^2]
  DTW[, bl3 := bl^3]

  return(DTW)

}


cb_hourly_regressors=function(DT){
    
    # check formati
    
    if (!('date' %in% colnames(DT)) | class(DT$date) != 'Date') {stop("date column must be format Date")
    } else if (!('hour' %in% colnames(DT)) | class(DT$hour) != 'numeric') {stop("hour column must be format numeric")
    } else if (!('value' %in% colnames(DT)) | class(DT$value) != 'numeric') {stop("value column must be format numeric")
    } else if (!('break_group_p' %in% colnames(DT)) | class(DT$break_group_p) != 'integer') {stop("break_group_p column must be format integer") }
    
    DTW=copy(DT)
    
    #add hp_trend to hourly DT
    #DTW = filtered_dam_dd[, .(date, hp_trend, weight)][DTW, on = "date"]
    
    
    # subtract hp_trend from raw data
    #DTW[, detr_value := value - as.numeric(hp_trend)]
    
    #create dummy for monthly, quarterly, weekend seasonality
    DTW[,`:=` (yday = data.table::yday(date),
                             wday = data.table::wday(date),
                             quarter = data.table::quarter(date),
                             month = data.table::month(date),
                             weekend = as.numeric(chron::is.weekend(date)),
                             obs = .I)]
    
    
    DTW[, 
                      season := fcase(
                          quarter == 1, "winter",
                          quarter == 2, "spring",
                          quarter == 3, "summer",
                          quarter == 4, "fall"
                      )]
    
    DTW[, (paste0("season_", c("winter", "spring", "summer", "fall"))) := 
                          lapply(c("winter", "spring", "summer", "fall"), function(s) fifelse(season == s, 1, 0))]
    
    
    ## Hourly Dummies ---------------------------------------------------------------------------------------------
    
    ####create dummy for weekday
    DTW[, (paste("day", 1:7, sep = "_")) := lapply(1:7, function(i) {fifelse(wday == i, 1, 0)})]
    
    ####create dummy for hours
    DTW[, (paste("hour", 1:24, sep = "_")) := lapply(1:24, function(i) {fifelse(hour == i, 1, 0)})]
    
    #### create baseload
    DTW[, bl := mean(value), by=date]
    
    #### create last market regime period
    DTW[,break_h:=fifelse(break_group_p==max(break_group_p),1,0)]
    
    DTW[,value_h:=value-bl]
    
    DTW[,yday2:=yday^2]
    DTW[,yday3:=yday^3]
    DTW[,bl2:=bl^2]
    DTW[,bl3:=bl^3]
    
    
    return(DTW)
    
}