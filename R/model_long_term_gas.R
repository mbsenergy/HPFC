
#' Estimate the long term seasonality model
#'
#' The function use a stepwise regression ...
#'
#' @param x A dataframe.
#' @returns A dataframe with 28 columns variable representing parameters
#' @export

model_long_term_gas=function(dataframe){

  # check formati

  if (!('date' %in% colnames(dataframe)) | class(dataframe$date) != 'Date') {stop("date column must be format Date")
  } else if (!('detr_smp_day' %in% colnames(dataframe)) | class(dataframe$detr_smp_day) != 'ts') {stop("detr_smp_day column must be format ts")
  } else if (!('holiday' %in% colnames(dataframe)) | class(dataframe$holiday) != 'numeric') {stop("holiday column must be format numeric")}

  df_reg=copy(dataframe)

  regression ="detr_smp_day~0"

  n_groups=max(df_reg$break_group_p)+1
  for (x in 1:n_groups){

    regression=paste(regression,"+",paste("break_group", x, sep = "_"))

    regression=paste(regression,"+",paste("cos_long_term", x, sep = "_"))
    #regression=paste(regression,"+",paste("cos_season", x, sep = "_"))
    #regression=paste(regression,"+",paste("cos_season_summer", x, sep = "_"))
    regression=paste(regression,"+",paste("sin_long_term", x, sep = "_"))
    #regression=paste(regression,"+",paste("sin_season", x, sep = "_"))
    #regression=paste(regression,"+",paste("sin_season_summer", x, sep = "_"))
    regression=paste(regression,"+",paste("holiday", x, sep = "_"))
    #regression=paste(regression,"+",paste("summer", x, sep = "_"))

    regression=paste(regression,"+",paste("day_2", x, sep = "_"))
    regression=paste(regression,"+",paste("day_3", x, sep = "_"))
    regression=paste(regression,"+",paste("day_4", x, sep = "_"))
    regression=paste(regression,"+",paste("day_5", x, sep = "_"))
    regression=paste(regression,"+",paste("day_6", x, sep = "_"))
    regression=paste(regression,"+",paste("day_7", x, sep = "_"))


    for (i in 1:10 ){
      regression=paste(regression,"+",paste("yday", i, x, sep = "_"))
    }

  }

  #regression=paste(regression,"+",'trade_close',"+",'trade_close2')
  model_1=eval(substitute(step(lm(regression,weights = weight, data = df_reg),direction="both")))
  #filtered_ttf_dd_detr[, LT_seas_1 := model_1$fitted.values]

  n_groups=max(df_reg$break_group_p)+1
  forecast_group=fifelse(min(df_reg[break_group_p==max(break_group_p),]$date)<as.Date('2022-01-01'),n_groups-1,n_groups)
  terms=paste(c('cos_long_term', 'cos_season', 'cos_season_summer', 'holiday', 'sin_long_term', 'sin_season', 'sin_season_summer', 'summer', 'day_1', 'day_2', 'day_3', 'day_4', 'day_5', 'day_6', 'day_7', 'yday_1','yday_2','yday_3','yday_4','yday_5','yday_6','yday_7','yday_8','yday_9','yday_10', 'break_group'), forecast_group, sep='_')

  missing_terms=terms[!(terms %in% names(model_1$coefficients))]
  profile_matrix=as.data.table(matrix(c(model_1$coefficients,rep(0,length(missing_terms))),nrow=1))
  setnames(profile_matrix,,c(names(model_1$coefficients),missing_terms))

  set(profile_matrix, , names(profile_matrix)[!names(profile_matrix) %in% terms], NULL)
  
  setnames(profile_matrix,,substr(colnames(profile_matrix), 1, nchar(colnames(profile_matrix))-2L))


  return(profile_matrix)

}
