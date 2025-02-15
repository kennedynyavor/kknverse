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


#' @title Converts to End of Month
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


#' @title Converts to Beginning of Month
#'
#' @param date A date value
#' @param n_months Number of months from current date
#'
#' @returns The beginning month of the input date
#' @export
#'
#' @import lubridate
#'
#' @examples bomonth(as.Date("2024-02-12"))
bomonth <- function(date, n_months = 0) {
  floor_date(date, "month") + months(n_months)
}


#' Add months to date
#'
#' @param date The date value
#' @param months The number of months to add
#'
#' @returns A date value "months" from the input date
#' @export
#'
#' @examples bomonth(as.Date("2024-02-12"), 4)
edate <- function(date, months) {
  ans <- date %m+% months(months)
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
#' @import lubridate
#'
#' @examples date_diff(as.Date("2021-02-16"), as.Date("2024-12-21"), "m")
date_diff <- function(start_date, end_date, unit = "month") {
  int <- lubridate::interval(start_date, end_date)
  unit_ <- trimws(tolower(unit), which = "both")
  result <-
    dplyr::case_when(
      grepl("^[dD]", unit_) ~ floor(as.numeric(int / lubridate::days(1))),
      grepl("^[mM]", unit_) ~ floor(as.numeric(int / months(1))),
      grepl("^[yY]", unit_) ~ floor(as.numeric(int / lubridate::years(1))),
      grepl("^[qQ]", unit_) ~ (year(end_date) - year(start_date)) * 4 +
        (quarter(end_date) - quarter(start_date)),
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
#' @import lubridate
#'
#' @examples date_yyyymm(as.Date("2023-03-23"))
date_yyyymm <- function(date) {
  ans <- year(date) * 100 + month(date)
  return(ans)
}



#' Function that loads global variables
#'
#' @param var A string of lines to add
#'
#' @returns Nothing, load global variables
#' @export
#'
#' @examples
#' update_param("library(devtools)")
update_param <- function(var) {
  if (!file.exists(".Rprofile")) {
    writeLines(var, ".Rprofile")
    .rs.restartR(clean = TRUE)
  } else {
    writeLines(var, ".Rprofile")
    .rs.restartR(clean = TRUE)
  }
}
