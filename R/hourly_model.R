
#' Estimate the hourly seasonality model
#'
#' The function use a nls function ...
#'
#' @param x A hourly dataframe.
#' @param y Gas Price column name.
#' @returns A nls object representing the hourly model
#' @import data.table
#' @export

hourly_model=function(dataframe,gas_name){

  if (!('date' %in% colnames(dataframe)) | class(dataframe$date) != 'Date') {stop("date column must be format Date")
  } else if (!(gas_name %in% colnames(dataframe)) | class(dataframe[,.(get(gas_name))][[1]]) != 'numeric') {stop("ttf price column must be format numeric")
  } else if (!('break_h' %in% colnames(dataframe)) | class(dataframe$break_h) != 'numeric') {stop("break_h column must be format numeric")}

  filtered_dam_ddhh=copy(dataframe)

  setnames(filtered_dam_ddhh, gas_name, 'trade_close')
  filtered_dam_ddhh[,trade_close2:=trade_close^2]

  nls.control(maxiter = 50, tol = 1e-05, minFactor = 1/2100,
              printEval = FALSE, warnOnly = FALSE, scaleOffset = 0,
              nDcentral = FALSE)

  model_h=nls(smp_h ~
                hour_1*coeff_hour_1+
                hour_2*coeff_hour_2+
                hour_3*coeff_hour_3+
                hour_4*coeff_hour_4+
                hour_5*coeff_hour_5+
                hour_6*coeff_hour_6+
                hour_7*coeff_hour_7+
                hour_8*coeff_hour_8+
                hour_9*coeff_hour_9+
                hour_10*coeff_hour_10+
                hour_11*coeff_hour_11+
                hour_12*coeff_hour_12+
                hour_13*coeff_hour_13+
                hour_14*coeff_hour_14+
                hour_15*coeff_hour_15+
                hour_16*coeff_hour_16+
                hour_17*coeff_hour_17+
                hour_18*coeff_hour_18+
                hour_19*coeff_hour_19+
                hour_20*coeff_hour_20+
                hour_21*coeff_hour_21+
                hour_22*coeff_hour_22+
                hour_23*coeff_hour_23+
                (-1*(coeff_hour_1+coeff_hour_2+
                       coeff_hour_3+coeff_hour_4+
                       coeff_hour_5+coeff_hour_6+
                       coeff_hour_7+coeff_hour_8+
                       coeff_hour_9+coeff_hour_10+
                       coeff_hour_11+coeff_hour_12+
                       coeff_hour_13+coeff_hour_14+
                       coeff_hour_15+coeff_hour_16+
                       coeff_hour_17+coeff_hour_18+
                       coeff_hour_19+coeff_hour_20+
                       coeff_hour_21+coeff_hour_22+
                       coeff_hour_23))*hour_24+ # end hour_ dummy --------------
              hour_1*bl*coeff_bl_hour_1+
                hour_2*bl*coeff_bl_hour_2+
                hour_3*bl*coeff_bl_hour_3+
                hour_4*bl*coeff_bl_hour_4+
                hour_5*bl*coeff_bl_hour_5+
                hour_6*bl*coeff_bl_hour_6+
                hour_7*bl*coeff_bl_hour_7+
                hour_8*bl*coeff_bl_hour_8+
                hour_9*bl*coeff_bl_hour_9+
                hour_10*bl*coeff_bl_hour_10+
                hour_11*bl*coeff_bl_hour_11+
                hour_12*bl*coeff_bl_hour_12+
                hour_13*bl*coeff_bl_hour_13+
                hour_14*bl*coeff_bl_hour_14+
                hour_15*bl*coeff_bl_hour_15+
                hour_16*bl*coeff_bl_hour_16+
                hour_17*bl*coeff_bl_hour_17+
                hour_18*bl*coeff_bl_hour_18+
                hour_19*bl*coeff_bl_hour_19+
                hour_20*bl*coeff_bl_hour_20+
                hour_21*bl*coeff_bl_hour_21+
                hour_22*bl*coeff_bl_hour_22+
                hour_23*bl*coeff_bl_hour_23+
                (-1*(coeff_bl_hour_1+coeff_bl_hour_2+
                       coeff_bl_hour_3+coeff_bl_hour_4+
                       coeff_bl_hour_5+coeff_bl_hour_6+
                       coeff_bl_hour_7+coeff_bl_hour_8+
                       coeff_bl_hour_9+coeff_bl_hour_10+
                       coeff_bl_hour_11+coeff_bl_hour_12+
                       coeff_bl_hour_13+coeff_bl_hour_14+
                       coeff_bl_hour_15+coeff_bl_hour_16+
                       coeff_bl_hour_17+coeff_bl_hour_18+
                       coeff_bl_hour_19+coeff_bl_hour_20+
                       coeff_bl_hour_21+coeff_bl_hour_22+
                       coeff_bl_hour_23))*hour_24*bl + # end hour_ bl interaction --------------
              hour_1*bl2*coeff_bl2_hour_1+
                hour_2*bl2*coeff_bl2_hour_2+
                hour_3*bl2*coeff_bl2_hour_3+
                hour_4*bl2*coeff_bl2_hour_4+
                hour_5*bl2*coeff_bl2_hour_5+
                hour_6*bl2*coeff_bl2_hour_6+
                hour_7*bl2*coeff_bl2_hour_7+
                hour_8*bl2*coeff_bl2_hour_8+
                hour_9*bl2*coeff_bl2_hour_9+
                hour_10*bl2*coeff_bl2_hour_10+
                hour_11*bl2*coeff_bl2_hour_11+
                hour_12*bl2*coeff_bl2_hour_12+
                hour_13*bl2*coeff_bl2_hour_13+
                hour_14*bl2*coeff_bl2_hour_14+
                hour_15*bl2*coeff_bl2_hour_15+
                hour_16*bl2*coeff_bl2_hour_16+
                hour_17*bl2*coeff_bl2_hour_17+
                hour_18*bl2*coeff_bl2_hour_18+
                hour_19*bl2*coeff_bl2_hour_19+
                hour_20*bl2*coeff_bl2_hour_20+
                hour_21*bl2*coeff_bl2_hour_21+
                hour_22*bl2*coeff_bl2_hour_22+
                hour_23*bl2*coeff_bl2_hour_23+
                (-1*(coeff_bl2_hour_1+coeff_bl2_hour_2+
                       coeff_bl2_hour_3+coeff_bl2_hour_4+
                       coeff_bl2_hour_5+coeff_bl2_hour_6+
                       coeff_bl2_hour_7+coeff_bl2_hour_8+
                       coeff_bl2_hour_9+coeff_bl2_hour_10+
                       coeff_bl2_hour_11+coeff_bl2_hour_12+
                       coeff_bl2_hour_13+coeff_bl2_hour_14+
                       coeff_bl2_hour_15+coeff_bl2_hour_16+
                       coeff_bl2_hour_17+coeff_bl2_hour_18+
                       coeff_bl2_hour_19+coeff_bl2_hour_20+
                       coeff_bl2_hour_21+coeff_bl2_hour_22+
                       coeff_bl2_hour_23))*hour_24*bl2 + # end hour_ bl2 interaction --------------
              hour_1*bl3*coeff_bl3_hour_1+
                hour_2*bl3*coeff_bl3_hour_2+
                hour_3*bl3*coeff_bl3_hour_3+
                hour_4*bl3*coeff_bl3_hour_4+
                hour_5*bl3*coeff_bl3_hour_5+
                hour_6*bl3*coeff_bl3_hour_6+
                hour_7*bl3*coeff_bl3_hour_7+
                hour_8*bl3*coeff_bl3_hour_8+
                hour_9*bl3*coeff_bl3_hour_9+
                hour_10*bl3*coeff_bl3_hour_10+
                hour_11*bl3*coeff_bl3_hour_11+
                hour_12*bl3*coeff_bl3_hour_12+
                hour_13*bl3*coeff_bl3_hour_13+
                hour_14*bl3*coeff_bl3_hour_14+
                hour_15*bl3*coeff_bl3_hour_15+
                hour_16*bl3*coeff_bl3_hour_16+
                hour_17*bl3*coeff_bl3_hour_17+
                hour_18*bl3*coeff_bl3_hour_18+
                hour_19*bl3*coeff_bl3_hour_19+
                hour_20*bl3*coeff_bl3_hour_20+
                hour_21*bl3*coeff_bl3_hour_21+
                hour_22*bl3*coeff_bl3_hour_22+
                hour_23*bl3*coeff_bl3_hour_23+
                (-1*(coeff_bl3_hour_1+coeff_bl3_hour_2+
                       coeff_bl3_hour_3+coeff_bl3_hour_4+
                       coeff_bl3_hour_5+coeff_bl3_hour_6+
                       coeff_bl3_hour_7+coeff_bl3_hour_8+
                       coeff_bl3_hour_9+coeff_bl3_hour_10+
                       coeff_bl3_hour_11+coeff_bl3_hour_12+
                       coeff_bl3_hour_13+coeff_bl3_hour_14+
                       coeff_bl3_hour_15+coeff_bl3_hour_16+
                       coeff_bl3_hour_17+coeff_bl3_hour_18+
                       coeff_bl3_hour_19+coeff_bl3_hour_20+
                       coeff_bl3_hour_21+coeff_bl3_hour_22+
                       coeff_bl3_hour_23))*hour_24*bl3 + # end hour_ bl3 interaction --------------
              hour_1*trade_close*coeff_trade_close_hour_1+
                hour_2*trade_close*coeff_trade_close_hour_2+
                hour_3*trade_close*coeff_trade_close_hour_3+
                hour_4*trade_close*coeff_trade_close_hour_4+
                hour_5*trade_close*coeff_trade_close_hour_5+
                hour_6*trade_close*coeff_trade_close_hour_6+
                hour_7*trade_close*coeff_trade_close_hour_7+
                hour_8*trade_close*coeff_trade_close_hour_8+
                hour_9*trade_close*coeff_trade_close_hour_9+
                hour_10*trade_close*coeff_trade_close_hour_10+
                hour_11*trade_close*coeff_trade_close_hour_11+
                hour_12*trade_close*coeff_trade_close_hour_12+
                hour_13*trade_close*coeff_trade_close_hour_13+
                hour_14*trade_close*coeff_trade_close_hour_14+
                hour_15*trade_close*coeff_trade_close_hour_15+
                hour_16*trade_close*coeff_trade_close_hour_16+
                hour_17*trade_close*coeff_trade_close_hour_17+
                hour_18*trade_close*coeff_trade_close_hour_18+
                hour_19*trade_close*coeff_trade_close_hour_19+
                hour_20*trade_close*coeff_trade_close_hour_20+
                hour_21*trade_close*coeff_trade_close_hour_21+
                hour_22*trade_close*coeff_trade_close_hour_22+
                hour_23*trade_close*coeff_trade_close_hour_23+
                (-1*(coeff_trade_close_hour_1+coeff_trade_close_hour_2+
                       coeff_trade_close_hour_3+coeff_trade_close_hour_4+
                       coeff_trade_close_hour_5+coeff_trade_close_hour_6+
                       coeff_trade_close_hour_7+coeff_trade_close_hour_8+
                       coeff_trade_close_hour_9+coeff_trade_close_hour_10+
                       coeff_trade_close_hour_11+coeff_trade_close_hour_12+
                       coeff_trade_close_hour_13+coeff_trade_close_hour_14+
                       coeff_trade_close_hour_15+coeff_trade_close_hour_16+
                       coeff_trade_close_hour_17+coeff_trade_close_hour_18+
                       coeff_trade_close_hour_19+coeff_trade_close_hour_20+
                       coeff_trade_close_hour_21+coeff_trade_close_hour_22+
                       coeff_trade_close_hour_23))*hour_24*trade_close + # end hour_ trade_close interaction --------------
              hour_1*trade_close2*coeff_trade_close2_hour_1+
                hour_2*trade_close2*coeff_trade_close2_hour_2+
                hour_3*trade_close2*coeff_trade_close2_hour_3+
                hour_4*trade_close2*coeff_trade_close2_hour_4+
                hour_5*trade_close2*coeff_trade_close2_hour_5+
                hour_6*trade_close2*coeff_trade_close2_hour_6+
                hour_7*trade_close2*coeff_trade_close2_hour_7+
                hour_8*trade_close2*coeff_trade_close2_hour_8+
                hour_9*trade_close2*coeff_trade_close2_hour_9+
                hour_10*trade_close2*coeff_trade_close2_hour_10+
                hour_11*trade_close2*coeff_trade_close2_hour_11+
                hour_12*trade_close2*coeff_trade_close2_hour_12+
                hour_13*trade_close2*coeff_trade_close2_hour_13+
                hour_14*trade_close2*coeff_trade_close2_hour_14+
                hour_15*trade_close2*coeff_trade_close2_hour_15+
                hour_16*trade_close2*coeff_trade_close2_hour_16+
                hour_17*trade_close2*coeff_trade_close2_hour_17+
                hour_18*trade_close2*coeff_trade_close2_hour_18+
                hour_19*trade_close2*coeff_trade_close2_hour_19+
                hour_20*trade_close2*coeff_trade_close2_hour_20+
                hour_21*trade_close2*coeff_trade_close2_hour_21+
                hour_22*trade_close2*coeff_trade_close2_hour_22+
                hour_23*trade_close2*coeff_trade_close2_hour_23+
                (-1*(coeff_trade_close2_hour_1+coeff_trade_close2_hour_2+
                       coeff_trade_close2_hour_3+coeff_trade_close2_hour_4+
                       coeff_trade_close2_hour_5+coeff_trade_close2_hour_6+
                       coeff_trade_close2_hour_7+coeff_trade_close2_hour_8+
                       coeff_trade_close2_hour_9+coeff_trade_close2_hour_10+
                       coeff_trade_close2_hour_11+coeff_trade_close2_hour_12+
                       coeff_trade_close2_hour_13+coeff_trade_close2_hour_14+
                       coeff_trade_close2_hour_15+coeff_trade_close2_hour_16+
                       coeff_trade_close2_hour_17+coeff_trade_close2_hour_18+
                       coeff_trade_close2_hour_19+coeff_trade_close2_hour_20+
                       coeff_trade_close2_hour_21+coeff_trade_close2_hour_22+
                       coeff_trade_close2_hour_23))*hour_24*trade_close2 + # end hour_ trade_close2 interaction --------------
              hour_1*weekend*coeff_weekend_hour_1+
                hour_2*weekend*coeff_weekend_hour_2+
                hour_3*weekend*coeff_weekend_hour_3+
                hour_4*weekend*coeff_weekend_hour_4+
                hour_5*weekend*coeff_weekend_hour_5+
                hour_6*weekend*coeff_weekend_hour_6+
                hour_7*weekend*coeff_weekend_hour_7+
                hour_8*weekend*coeff_weekend_hour_8+
                hour_9*weekend*coeff_weekend_hour_9+
                hour_10*weekend*coeff_weekend_hour_10+
                hour_11*weekend*coeff_weekend_hour_11+
                hour_12*weekend*coeff_weekend_hour_12+
                hour_13*weekend*coeff_weekend_hour_13+
                hour_14*weekend*coeff_weekend_hour_14+
                hour_15*weekend*coeff_weekend_hour_15+
                hour_16*weekend*coeff_weekend_hour_16+
                hour_17*weekend*coeff_weekend_hour_17+
                hour_18*weekend*coeff_weekend_hour_18+
                hour_19*weekend*coeff_weekend_hour_19+
                hour_20*weekend*coeff_weekend_hour_20+
                hour_21*weekend*coeff_weekend_hour_21+
                hour_22*weekend*coeff_weekend_hour_22+
                hour_23*weekend*coeff_weekend_hour_23+
                (-1*(coeff_weekend_hour_1+coeff_weekend_hour_2+
                       coeff_weekend_hour_3+coeff_weekend_hour_4+
                       coeff_weekend_hour_5+coeff_weekend_hour_6+
                       coeff_weekend_hour_7+coeff_weekend_hour_8+
                       coeff_weekend_hour_9+coeff_weekend_hour_10+
                       coeff_weekend_hour_11+coeff_weekend_hour_12+
                       coeff_weekend_hour_13+coeff_weekend_hour_14+
                       coeff_weekend_hour_15+coeff_weekend_hour_16+
                       coeff_weekend_hour_17+coeff_weekend_hour_18+
                       coeff_weekend_hour_19+coeff_weekend_hour_20+
                       coeff_weekend_hour_21+coeff_weekend_hour_22+
                       coeff_weekend_hour_23))*hour_24*weekend + # end hour_ weekend interaction --------------
              hour_1*break_h*coeff_break_h_hour_1+
                hour_2*break_h*coeff_break_h_hour_2+
                hour_3*break_h*coeff_break_h_hour_3+
                hour_4*break_h*coeff_break_h_hour_4+
                hour_5*break_h*coeff_break_h_hour_5+
                hour_6*break_h*coeff_break_h_hour_6+
                hour_7*break_h*coeff_break_h_hour_7+
                hour_8*break_h*coeff_break_h_hour_8+
                hour_9*break_h*coeff_break_h_hour_9+
                hour_10*break_h*coeff_break_h_hour_10+
                hour_11*break_h*coeff_break_h_hour_11+
                hour_12*break_h*coeff_break_h_hour_12+
                hour_13*break_h*coeff_break_h_hour_13+
                hour_14*break_h*coeff_break_h_hour_14+
                hour_15*break_h*coeff_break_h_hour_15+
                hour_16*break_h*coeff_break_h_hour_16+
                hour_17*break_h*coeff_break_h_hour_17+
                hour_18*break_h*coeff_break_h_hour_18+
                hour_19*break_h*coeff_break_h_hour_19+
                hour_20*break_h*coeff_break_h_hour_20+
                hour_21*break_h*coeff_break_h_hour_21+
                hour_22*break_h*coeff_break_h_hour_22+
                hour_23*break_h*coeff_break_h_hour_23+
                (-1*(coeff_break_h_hour_1+coeff_break_h_hour_2+
                       coeff_break_h_hour_3+coeff_break_h_hour_4+
                       coeff_break_h_hour_5+coeff_break_h_hour_6+
                       coeff_break_h_hour_7+coeff_break_h_hour_8+
                       coeff_break_h_hour_9+coeff_break_h_hour_10+
                       coeff_break_h_hour_11+coeff_break_h_hour_12+
                       coeff_break_h_hour_13+coeff_break_h_hour_14+
                       coeff_break_h_hour_15+coeff_break_h_hour_16+
                       coeff_break_h_hour_17+coeff_break_h_hour_18+
                       coeff_break_h_hour_19+coeff_break_h_hour_20+
                       coeff_break_h_hour_21+coeff_break_h_hour_22+
                       coeff_break_h_hour_23))*hour_24*break_h, # end hour_ break_group_h interaction --------------
              start=list(coeff_hour_1=0,coeff_hour_2=0,coeff_hour_3=0,coeff_hour_4=0,coeff_hour_5=0,coeff_hour_6=0,coeff_hour_7=0,coeff_hour_8=0,coeff_hour_9=0,coeff_hour_10=0,coeff_hour_11=0,coeff_hour_12=0,coeff_hour_13=0,
                         coeff_hour_14=0,coeff_hour_15=0,coeff_hour_16=0,coeff_hour_17=0,coeff_hour_18=0,coeff_hour_19=0,coeff_hour_20=0,coeff_hour_21=0,coeff_hour_22=0,coeff_hour_23=0,
                         coeff_bl_hour_1=0,coeff_bl_hour_2=0,coeff_bl_hour_3=0,coeff_bl_hour_4=0,coeff_bl_hour_5=0,coeff_bl_hour_6=0,coeff_bl_hour_7=0,coeff_bl_hour_8=0,coeff_bl_hour_9=0,coeff_bl_hour_10=0,coeff_bl_hour_11=0,coeff_bl_hour_12=0,coeff_bl_hour_13=0,
                         coeff_bl_hour_14=0,coeff_bl_hour_15=0,coeff_bl_hour_16=0,coeff_bl_hour_17=0,coeff_bl_hour_18=0,coeff_bl_hour_19=0,coeff_bl_hour_20=0,coeff_bl_hour_21=0,coeff_bl_hour_22=0,coeff_bl_hour_23=0,
                         coeff_bl2_hour_1=0,coeff_bl2_hour_2=0,coeff_bl2_hour_3=0,coeff_bl2_hour_4=0,coeff_bl2_hour_5=0,coeff_bl2_hour_6=0,coeff_bl2_hour_7=0,coeff_bl2_hour_8=0,coeff_bl2_hour_9=0,coeff_bl2_hour_10=0,coeff_bl2_hour_11=0,coeff_bl2_hour_12=0,coeff_bl2_hour_13=0,
                         coeff_bl2_hour_14=0,coeff_bl2_hour_15=0,coeff_bl2_hour_16=0,coeff_bl2_hour_17=0,coeff_bl2_hour_18=0,coeff_bl2_hour_19=0,coeff_bl2_hour_20=0,coeff_bl2_hour_21=0,coeff_bl2_hour_22=0,coeff_bl2_hour_23=0,
                         coeff_bl3_hour_1=0,coeff_bl3_hour_2=0,coeff_bl3_hour_3=0,coeff_bl3_hour_4=0,coeff_bl3_hour_5=0,coeff_bl3_hour_6=0,coeff_bl3_hour_7=0,coeff_bl3_hour_8=0,coeff_bl3_hour_9=0,coeff_bl3_hour_10=0,coeff_bl3_hour_11=0,coeff_bl3_hour_12=0,coeff_bl3_hour_13=0,
                         coeff_bl3_hour_14=0,coeff_bl3_hour_15=0,coeff_bl3_hour_16=0,coeff_bl3_hour_17=0,coeff_bl3_hour_18=0,coeff_bl3_hour_19=0,coeff_bl3_hour_20=0,coeff_bl3_hour_21=0,coeff_bl3_hour_22=0,coeff_bl3_hour_23=0,
                         coeff_trade_close_hour_1=0,coeff_trade_close_hour_2=0,coeff_trade_close_hour_3=0,coeff_trade_close_hour_4=0,coeff_trade_close_hour_5=0,coeff_trade_close_hour_6=0,coeff_trade_close_hour_7=0,coeff_trade_close_hour_8=0,coeff_trade_close_hour_9=0,coeff_trade_close_hour_10=0,coeff_trade_close_hour_11=0,coeff_trade_close_hour_12=0,coeff_trade_close_hour_13=0,
                         coeff_trade_close_hour_14=0,coeff_trade_close_hour_15=0,coeff_trade_close_hour_16=0,coeff_trade_close_hour_17=0,coeff_trade_close_hour_18=0,coeff_trade_close_hour_19=0,coeff_trade_close_hour_20=0,coeff_trade_close_hour_21=0,coeff_trade_close_hour_22=0,coeff_trade_close_hour_23=0,
                         coeff_trade_close2_hour_1=0,coeff_trade_close2_hour_2=0,coeff_trade_close2_hour_3=0,coeff_trade_close2_hour_4=0,coeff_trade_close2_hour_5=0,coeff_trade_close2_hour_6=0,coeff_trade_close2_hour_7=0,coeff_trade_close2_hour_8=0,coeff_trade_close2_hour_9=0,coeff_trade_close2_hour_10=0,coeff_trade_close2_hour_11=0,coeff_trade_close2_hour_12=0,coeff_trade_close2_hour_13=0,
                         coeff_trade_close2_hour_14=0,coeff_trade_close2_hour_15=0,coeff_trade_close2_hour_16=0,coeff_trade_close2_hour_17=0,coeff_trade_close2_hour_18=0,coeff_trade_close2_hour_19=0,coeff_trade_close2_hour_20=0,coeff_trade_close2_hour_21=0,coeff_trade_close2_hour_22=0,coeff_trade_close2_hour_23=0,
                         coeff_weekend_hour_1=0,coeff_weekend_hour_2=0,coeff_weekend_hour_3=0,coeff_weekend_hour_4=0,coeff_weekend_hour_5=0,coeff_weekend_hour_6=0,coeff_weekend_hour_7=0,coeff_weekend_hour_8=0,coeff_weekend_hour_9=0,coeff_weekend_hour_10=0,coeff_weekend_hour_11=0,coeff_weekend_hour_12=0,coeff_weekend_hour_13=0,
                         coeff_weekend_hour_14=0,coeff_weekend_hour_15=0,coeff_weekend_hour_16=0,coeff_weekend_hour_17=0,coeff_weekend_hour_18=0,coeff_weekend_hour_19=0,coeff_weekend_hour_20=0,coeff_weekend_hour_21=0,coeff_weekend_hour_22=0,coeff_weekend_hour_23=0,
                         coeff_break_h_hour_1=0,coeff_break_h_hour_2=0,coeff_break_h_hour_3=0,coeff_break_h_hour_4=0,coeff_break_h_hour_5=0,coeff_break_h_hour_6=0,coeff_break_h_hour_7=0,coeff_break_h_hour_8=0,coeff_break_h_hour_9=0,coeff_break_h_hour_10=0,coeff_break_h_hour_11=0,coeff_break_h_hour_12=0,coeff_break_h_hour_13=0,
                         coeff_break_h_hour_14=0,coeff_break_h_hour_15=0,coeff_break_h_hour_16=0,coeff_break_h_hour_17=0,coeff_break_h_hour_18=0,coeff_break_h_hour_19=0,coeff_break_h_hour_20=0,coeff_break_h_hour_21=0,coeff_break_h_hour_22=0,coeff_break_h_hour_23=0),
              data = filtered_dam_ddhh,
              control = nls.control(maxiter = 50, tol = 1e-05, minFactor = 1/2100,
                                    printEval = FALSE, warnOnly = FALSE, scaleOffset = 0,
                                    nDcentral = FALSE))

  #### store estimated coefficients
  #coefs_xx = data.frame(coef = coefficients(model_xx),name = names(coefficients(model_xx))) |> setDT()

  return(model_h)

}
