#' Reads csv file with FARS data
#'
#' \code{fars_read} reads a csv file and writes an error when the file does not
#' exist
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @param filename A character string. Path to the file which should be read.
#'
#' @return If the filepath exists, then the output will be a tibble containing
#'   the data of the csv file \code{filename}. Otherwise it will throw an error
#'   message.
#'
#' @seealso \code{\link{read_csv}}
#' @seealso \code{\link{tbl_df}}
#'
#' @examples
#' \dontrun{
#' data <- fars_read("./data/accident_2013.csv.bz2")
#' }
#'
#' @export
fars_read <- function(filename) {
  if (!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Creates a data file name
#'
#' \code{make_filename} creates a filename which finishes with the stated year.
#'
#' @param year Number or string. This value will be added to the end of the
#'   file name.
#'
#' @return If the input is a number or a string, then the output will be a
#'   character string containg the year at the end. Otherwise it will throw an
#'   error.
#'
#' @examples
#' filename1 <- make_filename(2014)
#' filename2 <- make_filename("2015")
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  system.file("extdata",
              sprintf("accident_%d.csv.bz2", year),
              package = "TestPackage",
              mustWork = TRUE)
}

#' Read FARS years
#'
#' An auxiliary function used by \code{fars_summarize_years} to extract years
#' and months from FARS data.
#'
#' @importFrom dplyr mutate_
#' @importFrom dplyr select_
#' @importFrom magrittr %>%
#'
#' @param years A List of strings or numbers, which correspond to the years from
#'   which to extract data.
#'
#' @return A tibble including entries of month and year read from the different
#'   csv files of the different years provided in \code{years}, or \code{NULL}
#'   and a warning if the input is not valid.
#'
#' @seealso \code{\link{fars_read}}
#' @seealso \code{\link{make_filename}}
#' @seealso \code{\link{fars_summarize_years}}
#'
#' @examples
#' data <- fars_read_years(c(2014, 2015))
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summary of FARS data by years
#'
#' \code{fars_summarize_years} summarizes accidents by month and year.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by_
#' @importFrom dplyr summarize_
#' @importFrom tidyr spread_
#' @importFrom magrittr "%>%"
#'
#' @param years List of strings or numbers, which correspond to the years from
#'   which to extract and summarize data.
#'
#' @return A tibble with the number of accidents by month and years, or an error if
#'   the input was invalid.
#'
#' @examples
#' fars_summarize_years(c(2014, 2015))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    tidyr::spread(year, n)
}

#' Creates accident maps
#'
#' Creates a map for a state and a year to show where the corresponding
#' accicdents occured.
#'
#' @importFrom maps map
#' @importFrom dplyr filter_
#' @importFrom graphics points
#'
#' @param state.num An integer value, which corresponds to the State Code.
#' @param year A character string or an integer, which determines the year.
#'
#' @return NULL or an error if one of the input value is invalid.
#'
#' @examples
#' fars_map_state(49,2015)
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}

