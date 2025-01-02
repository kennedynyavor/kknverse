# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Converts to End of Month
#'
#' @param date A date value
#'
#' @returns A date representing the end of the month of the input date
#' @export
#'
#' @examples eomonth(as.Date("2024-02-12"))
eomonth <- function(date) {
  lubridate::ceiling_date(date, "month") - lubridate::days(1)
}


#' Converts to Beginning of Month
#'
#' @param date A date value
#' @param n_months Number of months from current date
#'
#' @returns The beginning month of the input date
#' @export
#'
#' @examples bomonth(as.Date("2024-02-12"))
bomonth <- function(date, n_months = 0){
  lubridate::floor_date(date, "month") + base::months(n_months)
}


#' Add months to date
#'
#' @param date The date value
#' @param months The number of months to add
#'
#' @returns A date value "months" from the input date
#' @export
#'
#' @examples bomonth(as.Date("2024-02-12"),4)
edate <- function(date,months) {
  ans <- date %m+% base::months(months)
  return(ans)
}



#' Calculate date difference
#'
#' @param start_date The start date of the period
#' @param end_date The end date of the period
#' @param unit The unit of difference between the two dates (m=months, d = days, y = years,q=quarters)
#'
#' @returns Returns the number of periods (days, months, years, quarters) between two dates
#' @export
#'
#' @examples date_diff(as.Date('2021-02-16'),as.Date('2024-12-21'),"m")
date_diff <- function(start_date, end_date,unit = 'month') {
  int <- lubridate::interval(start_date,end_date)
  unit_ <- trimws(tolower(unit),which = "both")
  result <-
    dplyr::case_when(
    grepl("^[dD]",unit_) ~ floor(as.numeric(int/lubridate::days(1))),
    grepl("^[mM]",unit_) ~ floor(as.numeric(int/months(1))),
    grepl("^[yY]",unit_) ~ floor(as.numeric(int/lubridate::years(1))),
    grepl("^[qQ]",unit_) ~ (lubridate::year(end_date) - lubridate::year(start_date))*4 +
                           (lubridate::quarter(end_date) - lubridate::quarter(start_date)),
    .default = NA
  )
  return(result)
}



#' A function that convert a date into a numeric yyyymm form
#'
#' @param date A date value
#'
#' @returns A numeric representation of a date in the for yyyymm
#' @export
#'
#' @examples date_yyyymm(as.Date('2023-03-23'))
date_yyyymm <- function(date){
  ans <- lubridate::year(date)*100+lubridate::month(date)
  return(ans)
}


