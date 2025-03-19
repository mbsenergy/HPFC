
#' Estimate the long term seasonality model
#'
#' The function use a stepwise regression, This is the second version with value_gas included
#'
#' @param x A dataframe.
#' @param y Gas Price column name.
#' @returns A DT with 28 columns variable representing parameters
#' @import data.table
#' @export

model_long_term = function(DT) {

  # check formati

  if (!"data.table" %in% class(DT)) {
    stop(red("Error: Input is not a data.table"))
}
  DTW = copy(DT)

  DTW[, value_gas2 := value_gas^2]

  regression = "detr_value~cos_long_term+cos_season+cos_season_summer+holiday+day_2+day_3+day_4+day_5+day_6+day_7+sin_long_term+sin_season+sin_season_summer+summer"

  #model_0=step(lm(regression,weights = weight,data=filtered_dam_dd),direction="both")

  for (i in 1:10 ){
    regression = paste(regression, "+", paste("yday", i, sep = "_"))
  }

  regression=paste(regression, "+", 'value_gas', "+", 'value_gas2')
  model_1 = suppressWarnings(suppressMessages(
      eval(substitute(step(lm(regression,weights = weight, data = DTW), direction = "both", trace = 0)))
  ))

  #filtered_dam_dd[, LT_seas_1 := model_1$fitted.values]


  #### create model_1 coeff vector
  terms=c('cos_long_term', 'cos_season', 'cos_season_summer', 'holiday', 'sin_long_term', 'sin_season', 'sin_season_summer', 'summer', 'day_1', 'day_2', 'day_3', 'day_4', 'day_5', 'day_6', 'day_7', 'yday_1','yday_2','yday_3','yday_4','yday_5','yday_6','yday_7','yday_8','yday_9','yday_10','value_gas','value_gas2')
  missing_terms=terms[!(terms %in% names(model_1$coefficients))]
  profile_matrix=as.data.table(matrix(c(model_1$coefficients,rep(0,length(missing_terms))),nrow=1))
  setnames(profile_matrix,,c(names(model_1$coefficients),missing_terms))


  return(profile_matrix)

}
