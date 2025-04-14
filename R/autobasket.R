#' Automatic Basis Basket Selection for Commodity Curve Modeling
#'
#' Selects an optimal basket of commodities to model the continuous calendar curve
#' of a target commodity using regularized linear regression (Lasso or Ridge).
#'
#' @param DT A `data.table` containing time series data with columns: `DATE`, `VALUE`, `COMMODITY`, and `TYPE`.
#' @param mk_comm_0 Character. The reference commodity to model.
#' @param basket Optional character vector of additional commodities to consider as predictors. Defaults to `NULL`.
#' @param preview Logical. Currently unused. Defaults to `FALSE`.
#' @param full Logical. If `TRUE`, returns the full modeling output including the test set. Defaults to `FALSE`.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{coef_glm}{`data.frame` with selected commodities and their model coefficients and weights}
#'   \item{R2_test}{R-squared of the out-of-sample test}
#'   \item{mse_test}{Mean squared error on the test set}
#'   \item{mae_test}{Mean absolute error on the test set}
#'   \item{sd_residuals_test}{Standard deviation of residuals on the test set}
#'   \item{dt_contcal_curves_test}{(only if `full = TRUE`) Full dataset with predictions and error analysis}
#' }
#'
#' @details
#' This function uses a 6-month lookback window for out-of-sample validation. It applies L2-regularized linear regression (`alpha = 0`)
#' using the `glmnet` package. NA values are forward-filled (NOCB). Model performance is reported with R², MSE, MAE, and residuals' standard deviation.
#'
#' @import data.table
#' @importFrom glmnet cv.glmnet coef makeX
#' @importFrom stats var
#' @importFrom zoo na.fill
#' @export
basket_selection = function(DT, mk_comm_0, basket = NULL, preview = FALSE, full = FALSE) {
    
    dt_series_contcal = DT[COMMODITY %in% c(mk_comm_0, basket)]
    dt_series_contcal = unique(dt_series_contcal)
    
    dt_contcal_curves = 
        dt_series_contcal[TYPE == 'c1', .(DATE, VALUE, COMMODITY)] %>% 
        dcast(DATE ~ COMMODITY, value.var = c('VALUE')) 
    
    ### Prepare data for automatic-basis basket  
    dt_contcal_curves_model = copy(dt_contcal_curves)
    dt_contcal_curves_model[, (names(dt_contcal_curves_model)[-1]) := lapply(.SD, function(x) {nafill(x, type = 'nocb')}), .SDcols = names(dt_contcal_curves_model)[-1]]
    dt_contcal_curves_model = dt_contcal_curves_model[complete.cases(dt_contcal_curves_model)]
    
    
    ### Split into TRAIN-TEST sets
    dt_contcal_curves_model = dt_contcal_curves_model %>% as.data.frame()
    latest_date = max(dt_contcal_curves_model$DATE)
    
    split_date = as.Date(latest_date) - months(6)
    if (is.na(split_date)) {
        split_date = as.Date(latest_date) - months(7)
    }    
    dt_contcal_curves_train = dt_contcal_curves_model[dt_contcal_curves_model$DATE <= split_date, ]
    
    x = glmnet::makeX(dt_contcal_curves_train[,!(names(dt_contcal_curves_train) %in% c('DATE', mk_comm_0))],
                      na.impute = TRUE)
    
    y = dt_contcal_curves_train[,(names(dt_contcal_curves_train) %in% c(mk_comm_0))]
    
    ### Model Parameters
    weights_vector = seq(1, nrow(x)) / max(seq(1, nrow(x)))
    lambda_seq = 10^seq(10, -2, length = 100)
    
    
    ## Model GLM - Weighted - Lasso
    model = glmnet::cv.glmnet(x = x,
                              y = y,
                              alpha = 0,
                              lambda = lambda_seq, 
                              lower.limits = 0,
                              weights = weights_vector,
                              intercept = FALSE)
    
    
    ### Model Perfomance (Train)
    best_model_index = which.min(model$cvm)
    coef_glm = coef(model, s = model$lambda[best_model_index])
    
    coef_glm = data.frame(COMMODITY = coef(model)@Dimnames[[1]][coef(model)@i + 1], coeff = coef(model)@x)
    coef_glm =  merge(coef_glm, unique(dt_series_contcal[TYPE == 'c1', .(RIC, COMMODITY)]), all.x = TRUE, by = 'COMMODITY')
    
    predictions = predict(model, newx = x, s = "lambda.min", bestmodel = TRUE)
    residuals = y - predictions
    mse = mean(residuals ^ 2)
    
    var_y = var(y)
    
    R2 = 1 - mse / var_y
    
    sd_residuals = sd(residuals)
    
    # Backtesting & Definitive Performance Metrics
    dt_contcal_curves_test = copy(dt_contcal_curves_model)
    x_test = as.matrix(dt_contcal_curves_test[, !(names(dt_contcal_curves_train) %in% c('DATE', mk_comm_0))])
    
    predictions_test = as.vector(predict(model, newx = x_test, s = "lambda.min", bestmodel = TRUE))
    residuals_test = dt_contcal_curves_train[,(names(dt_contcal_curves_train) %in% c(mk_comm_0))] - predictions_test
    
    dt_contcal_curves_test$residuals = as.vector(residuals_test)
    basis_residuals = as.vector(residuals_test)
    
    dt_contcal_curves_test = as.data.table(dt_contcal_curves_test)
    setnames(dt_contcal_curves_test, old = mk_comm_0, new = 'mk_comm_0')
    
    ## Model performance
    mse_test = mean(residuals_test ^ 2)
    mae_test = mean(abs(residuals_test))
    mse_test
    
    R2_test = 1 - mse_test / var_y
    R2_test
    
    # Standard deviation of residuals
    sd_residuals_test = sd(residuals_test)
    sd_residuals_test
    
    ## Plot for Model outcome control
    setDT(dt_contcal_curves_test)
    dt_contcal_curves_test[, basis := predictions_test]
    dt_contcal_curves_test[, DELTA := mk_comm_0 - basis]
    dt_contcal_curves_test[, SET := fifelse(DATE <= split_date, 'TRAINING', 'VALIDATION')]
    
    ## PRINT MODEL RESULTS COMPARED TO COMMODITY 0
    coef_glm$weight = coef_glm$coeff / sum(coef_glm$coeff, na.rm = TRUE)
    
    coef_glm
    R2_test
    mae_test
    sd_residuals_test
    
    dt_contcal_curves_test$SET = NULL
    dt_contcal_curves_test$residuals = NULL
    
    if(isTRUE(full)) {
        
        list_basis_basket = list(coef_glm, R2_test, mse_test, mae_test, sd_residuals_test, dt_contcal_curves_test)
        names(list_basis_basket) = c('coef_glm', 'R2_test', 'mse_test', 'mae_test', 'sd_residuals_test', 'dt_contcal_curves_test') 
        
    } else {
        
        list_basis_basket = list(coef_glm, R2_test, mse_test, mae_test, sd_residuals_test)
        names(list_basis_basket) = c('coef_glm', 'R2_test', 'mse_test', 'mae_test', 'sd_residuals_test')    
        
    }
    
    return(list_basis_basket)
    
}