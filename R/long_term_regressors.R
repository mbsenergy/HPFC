
#' Create regressors in the daily dataframe to estimate the model
#'
#' The Macro trend is derived using HP filter
#'
#' @param x A daily dataframe with date, hp_trend and detr_smp_day
#' @param y a parameter for weight
#' @returns A dataframe with 41 columns
#' @import data.table
#' @export

long_term_regressor=function(dataframe, alpha) {
    
    # check formati
    if (!('date' %in% colnames(dataframe)) | class(dataframe$date) != 'Date') {
        stop("date column must be format Date")
    } else if (!('smp_day' %in% colnames(dataframe)) | class(dataframe$smp_day) != 'numeric') {
        stop("smp_day column must be format numeric")
    } else if (!('hp_trend' %in% colnames(dataframe)) | class(dataframe$hp_trend) != 'ts') {
        stop("hp_trend column must be format ts")
    } else if (!('detr_smp_day' %in% colnames(dataframe)) | class(dataframe$detr_smp_day) != 'ts') {
        stop("detr_smp_day column must be format ts")
    }
    
    if(class(alpha) != 'numeric' | alpha > 1 | alpha < 0){
        stop("alpha must be format numeric between 0 and 1")
    }
    
    filtered_dam_dd = copy(dataframe)
    
    # Create temporal features without yday
    filtered_dam_dd[, `:=`(
        wday = data.table::wday(date),
        quarter = data.table::quarter(date),
        month = data.table::month(date),
        weekend = as.numeric(chron::is.weekend(date)),
        obs = .I
    )]
    
    # Create weight with smoothing
    filtered_dam_dd[, time_distance := (max(obs) + 1 - obs) / max(obs)] 
    filtered_dam_dd[, weight := exp(-alpha * (time_distance))]
    
    # Create day dummies (intra-week seasonality)
    filtered_dam_dd[, (paste("day", 1:7, sep = "_")) := lapply(1:7, function(i) {fifelse(wday == i, 1, 0)})]
    
    # Create summer dummy
    filtered_dam_dd[, summer := as.numeric(quarter == 2 | quarter == 3)]
    
    # Create variable beginning season date AND delta date-beginning
    filtered_dam_dd[, beginning_season := as.Date(
        fifelse(quarter == 4, paste0(format(date, format = "%Y"), "-10-01"),
                fifelse(quarter == 1, paste0(data.table::year(date) - 1, "-10-01"),
                        paste0(format(date, format = "%Y"), "-04-01"))))]
    
    filtered_dam_dd[, dist := as.numeric(date - beginning_season)]
    filtered_dam_dd[, yday_season := dist / max(dist)]
    
    # Create variable season.year
    filtered_dam_dd[, season := fifelse(summer == 1, paste0("summer-", data.table::year(date)),
                                        fifelse(quarter == 4, paste0("winter-", data.table::year(date)),
                                                paste0("winter-", data.table::year(date) - 1)))]
    
    #### Create LT components Caldana 6.2 (without yday)
    filtered_dam_dd[, `:=`(
        cos_season = cos((2 * pi) * yday_season),
        cos_season_summer = cos((2 * pi) * yday_season) * summer,
        sin_season = sin((2 * pi) * yday_season),
        sin_season_summer = sin((2 * pi) * yday_season) * summer
    )]
    
    ### Remove polynomial terms for yday
    # No need to generate yday polynomial terms anymore
    
    return(filtered_dam_dd)
}
