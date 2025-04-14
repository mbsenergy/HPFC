#' Spot Prices - PWR
#'
#' A small dataset for demonstration purposes.
#'
#' @format A data.table with 5 rows and 2 columns:
#' \describe{
#'   \item{id}{Integer ID}
#'   \item{value}{Numeric values}
#' }
#' @source Generated manually
"dt_spot_pwr"

#' Spot Prices - GAS
#'
#' A small dataset for demonstration purposes.
#'
#' @format A data.table with 5 rows and 2 columns:
#' \describe{
#'   \item{id}{Integer ID}
#'   \item{value}{Numeric values}
#' }
#' @source Generated manually
"dt_spot_gas"


#' FWD Prices - PWR
#'
#' A small dataset for demonstration purposes.
#'
#' @format A data.table with 5 rows and 2 columns:
#' \describe{
#'   \item{id}{Integer ID}
#'   \item{value}{Numeric values}
#' }
#' @source Generated manually
"dt_fwds_pwr"

#' FWD Prices - GAS
#'
#' A small dataset for demonstration purposes.
#'
#' @format A data.table with 5 rows and 2 columns:
#' \describe{
#'   \item{id}{Integer ID}
#'   \item{value}{Numeric values}
#' }
#' @source Generated manually
"dt_fwds_gas"



#' FWD Prices or DAM Prices- PWR
#'
#' A small dataset for demonstration purposes.
#'
#' @format A data.table with 5 rows and 2 columns:
#' \describe{
#'   \item{id}{Integer ID}
#'   \item{value}{Numeric values}
#' }
#' @source Generated manually
"dt_fwds_pwr_fwddam"


#' New Calendar of Public Holidays
#'
#' Updated version of `calendar_holidays` with additional countries or extended date range.
#'
#' @format A data.table with columns:
#' \describe{
#'   \item{date}{Date}
#'   \item{holiday_GR}{Greece}
#'   \item{holiday_HU}{Hungary}
#'   ...
#' }
#' @usage data(new_calendar_holidays)
"new_calendar_holidays"

#' Power Market Mapping Table
#'
#' A reference table mapping countries to availability in Eikon data.
#'
#' @format A `data.table` with 11 rows and 2 columns:
#' \describe{
#'   \item{countries}{Country name (character)}
#'   \item{eikon}{Eikon data availability flag ("YES"/"NO")}
#' }
#' @usage data(pwr_mapped_codes)
#' @keywords datasets
"pwr_mapped_codes"

#' Gas Market Mapping Table
#'
#' A reference table mapping gas products to availability in Eikon data.
#'
#' @format A `data.table` with 5 rows and 2 columns:
#' \describe{
#'   \item{product}{Country name (character)}
#'   \item{eikon}{Eikon data availability flag ("YES"/"NO")}
#' }
#' @usage data(gas_mapped_codes)
#' @keywords datasets
"gas_mapped_codes"
