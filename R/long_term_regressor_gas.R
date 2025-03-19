#' Long-Term Regressor for Gas Prices
#'
#' This function generates long-term regressors for gas prices based on time-related features, seasonal patterns,
#' and different break groups. It creates several seasonal components (e.g., long-term trend, seasonal trend, and holiday) 
#' for use in regression modeling.
#'
#' @param DT A `data.table` containing the following columns:
#'   - `date`: The date of observation.
#'   - `break_group_p`: A numeric variable indicating the break group for the data.
#'   - `holiday`: (Optional) A binary variable indicating whether the day is a holiday (1 = holiday, 0 = non-holiday).
#'   - Additional columns such as `yday`, `wday`, `quarter`, and `month` are generated within the function.
#' @param alpha A numeric smoothing parameter used to weight observations.
#'
#' @return A `data.table` with the following columns:
#'   - `cos_long_term`, `cos_season`, `cos_season_summer`, `sin_long_term`, `sin_season`, `sin_season_summer`: Long-term, seasonal, and seasonal-summer components.
#'   - `break_group_x`: Dummy variables representing different break groups.
#'   - `yday_1`, `yday_2`, ..., `yday_10`: Polynomial terms of `yday` for non-linear effects.
#'   - `day_1`, `day_2`, ..., `day_7`: Day-of-week dummy variables representing intra-week seasonality.
#'   - `summer`: A binary variable indicating the summer season.
#'   - `begining_season`, `dist`, `yday_season`: Variables related to the beginning of the season and the distance from it.
#'   - `season`: A categorical variable indicating the season (e.g., "summer-2021", "winter-2021").
#'
#' @details
#' The function generates seasonal components based on the year day (`yday`), the week day (`wday`), and the quarter (`quarter`).
#' It also introduces a polynomial transformation of `yday` and intra-week dummy variables to account for within-week seasonality.
#' The long-term seasonal components are generated using cosine and sine functions.
#' A smoothing weight based on the time distance is also included in the model.
#'
#' @import data.table
#' @importFrom mFilter hpfilter
#' @importFrom crayon red yellow
#'
#' @examples
#' # Example usage:
#' DT <- data.table(date = as.Date('2020-01-01') + 0:100,
#'                  break_group_p = sample(1:3, 101, replace = TRUE))
#' result <- long_term_regressor_gas(DT, alpha = 0.1)
#'
#' @export
long_term_regressor_gas = function(DT, alpha) {

    # Check if DT is a data.table
    if (!"data.table" %in% class(DT)) {
        stop(red("Error: Input is not a data.table"))
    }
    if(class(alpha) != 'numeric' | alpha > 1 | alpha < 0) {stop("alpha must be format numeric between 0 and 1")}

    # Copy DT to avoid modifying the original
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
  
    n_groups = max(DTW$break_group_p) + 1
    DTW[, (paste("break_group", 1:n_groups, sep = "_")) := lapply(1:n_groups, function(i) { as.numeric(break_group_p == (i - 1)) })]
  
    for (x in 1:n_groups){
  
      DTW[, paste('cos_long_term', x,sep = '_')      := get(paste("break_group", x, sep = "_")) * cos_long_term]
      DTW[, paste('cos_season', x,sep = '_')         := get(paste("break_group", x, sep = "_")) * cos_season]
      DTW[, paste('cos_season_summer', x,sep = '_')  := get(paste("break_group", x, sep = "_")) * cos_season_summer]
      DTW[, paste('sin_long_term', x,sep = '_')      := get(paste("break_group", x, sep = "_")) * sin_long_term]
      DTW[, paste('sin_season', x,sep = '_')         := get(paste("break_group", x, sep = "_")) * sin_season]
      DTW[, paste('sin_season_summer', x,sep = '_')  := get(paste("break_group", x, sep = "_")) * sin_season_summer]
      DTW[, paste('holiday', x,sep = '_')            := get(paste("break_group", x, sep = "_")) * holiday]
      DTW[, paste('summer', x,sep = '_')             := get(paste("break_group", x, sep = "_")) * summer]
      DTW[, paste('day_2', x,sep = '_') := get(paste("break_group", x, sep = "_")) * day_2]
      DTW[, paste('day_3', x,sep = '_') := get(paste("break_group", x, sep = "_")) * day_3]
      DTW[, paste('day_4', x,sep = '_') := get(paste("break_group", x, sep = "_")) * day_4]
      DTW[,  paste('day_5', x,sep = '_') := get(paste("break_group", x, sep = "_")) * day_5]
      DTW[, paste('day_6', x,sep = '_') := get(paste("break_group", x, sep = "_")) * day_6]
      DTW[, paste('day_7', x,sep = '_') := get(paste("break_group", x, sep = "_")) * day_7]
  
      DTW[, (paste("yday", 1:10, x, sep = "_")) := lapply(1:10, function(i) { get(paste("break_group", x, sep = "_")) * get(paste("yday", i, sep = "_")) })]
  
    }
  
    return(DTW)
  
}