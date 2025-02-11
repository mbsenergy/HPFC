#' Spot GAS Products Data
#'
#' This dataset contains spot gas products and their corresponding forward codes.
#'
#' @format A data.table with columns:
#' \describe{
#'   \item{products_GAS}{Gas products (e.g., 'TTF', 'PSV', etc.)}
#'   \item{products_GAS_code}{Forward codes}
#'   \item{spot_GAS_code}{Spot market codes}
#' }
#' @examples
#' data(spot_GAS_products_full)
"spot_GAS_products_full"

#' Spot Power Products Data
#'
#' This dataset contains spot power products and their corresponding forward codes.
#'
#' @format A data.table with columns:
#' \describe{
#'   \item{countries}{Countries involved in power markets}
#'   \item{products_PWR_code}{Forward power codes}
#'   \item{spot_PWR_code}{Spot market codes}
#' }
#' @examples
#' data(spot_PWR_products_full)
"spot_PWR_products_full"


#' Generate a List of RICs for Gas Codes
#'
#' This function generates Reuters Instrument Codes (RICs) based on the selected gas codes.
#' It automatically retrieves the required environment variables.
#'
#' @param selected_GAS_codes A vector of selected gas codes.
#' @param time_range years to recreate
#'
#' @return A character vector of RICs.
#' @export
#' @import data.table
#'
#' @examples
#' lst_rics_gas <- generate_rics_gas(LST_PARAMS$selected_GAS_codes)
generate_rics_gas = function(selected_GAS_codes, time_range) {
    # Retrieve environment variables
    timeframe_GAS_code = c("MF", "MG", "MH", "MJ", "MK", "MM", "MN", "MQ", "MU", "MV", "MX", "MZ", 
                           "QH", "QM", "QU", "QZ",
                           "YZ")
    time_range = time_range
    reuters_months = HPFC::reuters_months
    reuters_quarters_GAS = HPFC::reuters_quarters_GAS  # Adjust namespace if needed
    
    # Create data table with all combinations
    dt_rics_gas = expand.grid(selected_GAS_codes, timeframe_GAS_code, time_range) |> setDT()
    
    # Generate RIC column based on conditions
    dt_rics_gas[, RIC := paste0(
        Var1, Var2,
        fcase(
            (substr(Var2, 1, 1) == "M" & Var3 == year(Sys.Date()) &
                 as.integer(match(substr(Var2, 2, 2), reuters_months$code)) <= month(Sys.Date())),
            paste0(substr(Var3, 4, 4), "^", substr(Var3, 3, 3)),
            
            (substr(Var2, 1, 1) == "Q" & Var3 == year(Sys.Date()) &
                 as.integer(match(substr(Var2, 2, 2), reuters_quarters_GAS$code)) <= quarter(Sys.Date())),
            paste0(substr(Var3, 4, 4), "^", substr(Var3, 3, 3)),
            
            (substr(Var2, 1, 1) == "Y" & Var3 == year(Sys.Date())),
            paste0(substr(Var3, 4, 4), "^", substr(Var3, 3, 3)),
            
            Var3 < year(Sys.Date()),
            paste0(substr(Var3, 4, 4), "^", substr(Var3, 3, 3)),
            
            default = substr(Var3, 4, 4)
        )
    )]
    
    # Return list of RICs
    return(dt_rics_gas$RIC)
}


#' Generate a List of RICs for Power Codes
#'
#' This function generates Reuters Instrument Codes (RICs) based on selected power codes.
#' It automatically retrieves the required environment variables.
#'
#' @param selected_PWR_codes A vector of selected power codes.
#' @param time_range years to recreate
#'
#' @return A character vector of RICs.
#' @export
#' @import data.table
#'
#' @examples
#' lst_rics_pwr <- generate_rics_pwr(LST_PARAMS$selected_PWR_codes)
generate_rics_pwr = function(selected_PWR_codes, time_range) {
    # Retrieve environment variables
    time_range = time_range
    reuters_months = HPFC::reuters_months
    reuters_quarters_PWR = HPFC::reuters_quarters_PWR
    
    # Define month and quarter codes
    month_quarter_codes = c("MF", "MG", "MH", "MJ", "MK", "MM", "MN", "MQ", "MU", "MV", "MX", "MZ", 
                            "QF", "QJ", "QN", "QV", "YF")
    
    # Create data table with all combinations
    dt_rics_pwr = expand.grid(selected_PWR_codes, c('B', 'P'), month_quarter_codes, time_range) |> setDT()
    
    # Generate RIC column based on conditions
    dt_rics_pwr[, RIC := paste0(
        Var1, Var2, Var3,
        fcase(
            (substr(Var3, 1, 1) == "M" & Var4 == year(Sys.Date()) &
                 as.integer(match(substr(Var3, 2, 2), reuters_months$code)) < month(Sys.Date())),
            paste0(substr(Var4, 4, 4), "^", substr(Var4, 3, 3)),
            
            (substr(Var3, 1, 1) == "Q" & Var4 == year(Sys.Date()) &
                 as.integer(match(substr(Var3, 2, 2), reuters_quarters_PWR$code)) <= quarter(Sys.Date())),
            paste0(substr(Var4, 4, 4), "^", substr(Var4, 3, 3)),
            
            (substr(Var3, 1, 1) == "Y" & Var4 == year(Sys.Date())),
            paste0(substr(Var4, 4, 4), "^", substr(Var4, 3, 3)),
            
            Var4 < year(Sys.Date()),
            paste0(substr(Var4, 4, 4), "^", substr(Var4, 3, 3)),
            
            default = substr(Var4, 4, 4)
        )
    )]
    
    # Apply replacements
    lst_rics_pwr = dt_rics_pwr$RIC
    lst_rics_pwr = gsub("FFBQ", "FFBQQ", lst_rics_pwr)
    lst_rics_pwr = gsub("FFPQ", "FFPQQ", lst_rics_pwr)
    
    return(lst_rics_pwr)
}