% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hello.R
\name{date_diff}
\alias{date_diff}
\title{Calculate date difference}
\usage{
date_diff(start_date, end_date, unit = "month")
}
\arguments{
\item{start_date}{The start date of the period}

\item{end_date}{The end date of the period}

\item{unit}{The unit of difference between the two dates (m=months, d = days, y = years,q=quarters)}
}
\value{
Returns the number of periods (days, months, years, quarters) between two dates
}
\description{
Calculate date difference
}
\examples{
date_diff(as.Date('2021-02-16'),as.Date('2024-12-21'),"m")
}
