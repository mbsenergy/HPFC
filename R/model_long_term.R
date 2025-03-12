
#' Estimate the long term seasonality model
#'
#' The function use a stepwise regression, This is the second version with trade_close included
#'
#' @param x A dataframe.
#' @param y Gas Price column name.
#' @returns A dataframe with 28 columns variable representing parameters
#' @import data.table
#' @export

model_long_term = function(dataframe, gas_name) {
    
    # Check the input formats
    if (!('date' %in% colnames(dataframe)) | class(dataframe$date) != 'Date') {
        stop("date column must be format Date")
    } else if (!('smp_day' %in% colnames(dataframe)) | class(dataframe$smp_day) != 'numeric') {
        stop("smp_day column must be format numeric")
    } else if (!('detr_smp_day' %in% colnames(dataframe)) | class(dataframe$detr_smp_day) != 'ts') {
        stop("detr_smp_day column must be format ts")
    } else if (!(gas_name %in% colnames(dataframe)) | class(dataframe[, .(get(gas_name))][[1]]) != 'numeric') {
        stop("ttf price column must be format numeric")
    } else if (!('holiday' %in% colnames(dataframe)) | class(dataframe$holiday) != 'numeric') {
        stop("holiday column must be format numeric")
    }
    
    df_reg = copy(dataframe)
    
    setnames(df_reg, gas_name, 'trade_close')
    df_reg[, trade_close2 := trade_close ^ 2]
    
    # Create the regression formula without yday terms
    regression = "detr_smp_day ~ cos_season + cos_season_summer + holiday + day_2 + day_3 + day_4 + day_5 + day_6 + day_7 + sin_season + sin_season_summer + summer"
    
    # Add the trade_close and its square terms
    regression = paste(regression, "+ trade_close + trade_close2")
    
    # Fit the model using stepwise regression
    model_1 = suppressWarnings(suppressMessages(
        eval(substitute(step(lm(regression, weights = weight, data = df_reg), direction = "both", trace = 0)))
    ))
    
    # Create model_1 coefficient vector
    terms = c('cos_season', 'cos_season_summer', 'holiday', 'sin_season', 'sin_season_summer', 'summer', 'day_1', 'day_2', 'day_3', 'day_4', 'day_5', 'day_6', 'day_7', 'trade_close', 'trade_close2')
    
    # Identify missing terms in the model and create a profile matrix
    missing_terms = terms[!(terms %in% names(model_1$coefficients))]
    profile_matrix = as.data.table(matrix(c(model_1$coefficients, rep(0, length(missing_terms))), nrow = 1))
    setnames(profile_matrix, c(names(model_1$coefficients), missing_terms))
    
    return(profile_matrix)
    
}

