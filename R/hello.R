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

hello <- function() {
  print("Hello, world!")
}

eomonth <- function(date) {
  lubridate::ceiling_date(date, "month") - lubridate::days(1)
}

bomonth <- function(date){
  lubridate::floor_date(date, "month")
}

edate <- function(date,months) {
  ans <- date %m+% base::months(months)
  return(ans)
}

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

date_yyyymm <- function(date){
  ans <- lubridate::year(date)*100+lubridate::month(date)
  return(ans)
}
