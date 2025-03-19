
#' Estimate the long term seasonality model
#'
#' The function use a stepwise regression ...
#'
#' @param x A dataframe.
#' @returns A DT with 28 columns variable representing parameters
#' @import data.table
#' @export

model_long_term_gas = function(DT) {

  # check formati
    # Check if DT is a data.table
  if (!"data.table" %in% class(DT)) {
      stop(red("Error: Input is not a data.table"))
  }

  DT_reg = copy(DT)
  regression = "detr_value~0"

  n_groups = max(DT_reg$break_group_p) + 1
  
  for (x in 1:n_groups){

    regression = paste(regression, "+", paste("break_group", x, sep = "_"))
    regression = paste(regression, "+", paste("cos_long_term", x, sep = "_"))
    regression = paste(regression, "+", paste("sin_long_term", x, sep = "_"))
    regression = paste(regression, "+", paste("holiday", x, sep = "_"))
    regression = paste(regression, "+", paste("day_2", x, sep = "_"))
    regression = paste(regression, "+", paste("day_3", x, sep = "_"))
    regression = paste(regression, "+", paste("day_4", x, sep = "_"))
    regression = paste(regression, "+", paste("day_5", x, sep = "_"))
    regression = paste(regression, "+", paste("day_6", x, sep = "_"))
    regression = paste(regression, "+", paste("day_7", x, sep = "_"))

    for (i in 1:10 ) {
      regression = paste(regression, "+", paste("yday", i, x, sep = "_"))
    }

  }

  model_1 = suppressWarnings(suppressMessages(
      eval(substitute(step(lm(regression, weights = weight, data = DT_reg), direction = "both", trace = 0)))
  ))

  n_groups = max(DT_reg$break_group_p) + 1
  forecast_group = fifelse(min(DT_reg[break_group_p == max(break_group_p)]$date) < as.Date('2022-01-01'), n_groups - 1, n_groups)
  terms = paste(c('cos_long_term', 'cos_season', 'cos_season_summer', 'holiday', 'sin_long_term', 'sin_season', 'sin_season_summer', 'summer', 'day_1', 'day_2', 'day_3', 'day_4', 'day_5', 'day_6', 'day_7', 'yday_1','yday_2','yday_3','yday_4','yday_5','yday_6','yday_7','yday_8','yday_9','yday_10', 'break_group'), forecast_group, sep='_')

  missing_terms = terms[!(terms %in% names(model_1$coefficients))]
  profile_matrix = as.data.table(matrix(c(model_1$coefficients, rep(0, length(missing_terms))), nrow = 1))
  setnames(profile_matrix, , c(names(model_1$coefficients), missing_terms))

  set(profile_matrix, , names(profile_matrix)[!names(profile_matrix) %in% terms], NULL)
  
  setnames(profile_matrix, , substr(colnames(profile_matrix), 1, nchar(colnames(profile_matrix)) - 2L))

  return(profile_matrix)

}
