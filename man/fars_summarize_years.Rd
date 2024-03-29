% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_function.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Summary of FARS data by years}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{List of strings or numbers, which correspond to the years from
which to extract and summarize data.}
}
\value{
A tibble with the number of accidents by month and years, or an error if
  the input was invalid.
}
\description{
\code{fars_summarize_years} summarizes accidents by month and year.
}
\examples{
fars_summarize_years(c(2014, 2015))

}
