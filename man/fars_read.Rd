% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_function.R
\name{fars_read}
\alias{fars_read}
\title{Reads csv file with FARS data}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A character string. Path to the file which should be read.}
}
\value{
If the filepath exists, then the output will be a tibble containing
  the data of the csv file \code{filename}. Otherwise it will throw an error
  message.
}
\description{
\code{fars_read} reads a csv file and writes an error when the file does not
exist
}
\examples{
\dontrun{
data <- fars_read("./data/accident_2013.csv.bz2")
}

}
\seealso{
\code{\link{read_csv}}

\code{\link{tbl_df}}
}
