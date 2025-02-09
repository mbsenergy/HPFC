
#' Estimate the long term seasonality model
#'
#' The function use a stepwise regression, This is the second version with trade_close included
#'
#' @param x A dataframe.
#' @param y Gas Price column name.
#' @returns A dataframe with 28 columns variable representing parameters
#' @export

model_long_term=function(dataframe,gas_name){

  # check formati

  if (!('date' %in% colnames(dataframe)) | class(dataframe$date) != 'Date') {stop("date column must be format Date")
  } else if (!('smp_day' %in% colnames(dataframe)) | class(dataframe$smp_day) != 'numeric') {stop("smp_day column must be format numeric")
  } else if (!('detr_smp_day' %in% colnames(dataframe)) | class(dataframe$detr_smp_day) != 'ts') {stop("detr_smp_day column must be format ts")
  } else if (!(gas_name %in% colnames(dataframe)) | class(dataframe[,.(get(gas_name))][[1]]) != 'numeric') {stop("ttf price column must be format numeric")
  } else if (!('holiday' %in% colnames(dataframe)) | class(dataframe$holiday) != 'numeric') {stop("holiday column must be format numeric")}

  df_reg=copy(dataframe)

  setnames(df_reg, gas_name, 'trade_close')
  df_reg[,trade_close2:=trade_close^2]

  regression ="detr_smp_day~cos_long_term+cos_season+cos_season_summer+holiday+day_2+day_3+day_4+day_5+day_6+day_7+sin_long_term+sin_season+sin_season_summer+summer"

  #model_0=step(lm(regression,weights = weight,data=filtered_dam_dd),direction="both")

  for (i in 1:10 ){
    regression=paste(regression,"+",paste("yday", i, sep = "_"))
  }

  regression=paste(regression,"+",'trade_close',"+",'trade_close2')
  model_1=eval(substitute(step(lm(regression,weights = weight, data = df_reg),direction="both")))

  #filtered_dam_dd[, LT_seas_1 := model_1$fitted.values]


  #### create model_1 coeff vector
  terms=c('cos_long_term', 'cos_season', 'cos_season_summer', 'holiday', 'sin_long_term', 'sin_season', 'sin_season_summer', 'summer', 'day_1', 'day_2', 'day_3', 'day_4', 'day_5', 'day_6', 'day_7', 'yday_1','yday_2','yday_3','yday_4','yday_5','yday_6','yday_7','yday_8','yday_9','yday_10','trade_close','trade_close2')
  missing_terms=terms[!(terms %in% names(model_1$coefficients))]
  profile_matrix=as.data.table(matrix(c(model_1$coefficients,rep(0,length(missing_terms))),nrow=1))
  setnames(profile_matrix,,c(names(model_1$coefficients),missing_terms))


  return(profile_matrix)

}
