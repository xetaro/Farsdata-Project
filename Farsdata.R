#' Read in  data from the US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System
#'
#' @description
#' The function loads a CSV file defined by \code{fars_read} argument and returns
#' a tibble. If the path is incorrect the function will end with an error.
#'
#' @param filename csv file containing data
#'
#' @return \code{fars_read} will search within the specified path for the
#'   filename provided. If the file exists, it will be imported and returned as
#'   a data frame tbl.  If it does not exist an error message will be returned.
#'
#' @importFrom readr fonction read_csv
#'
#' @importFrom dplyr fonction tbl_df
#'
#' @examples
#' \dontrun{
#' accident_2013 <- fars_read("./data/accident_2013.csv.bz2")
#' }

#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Name the File
#'
#' \code{make_file} creates a name for the csv file for each year.
#'
#' @param year the year to add to the file name
#'
#' @return \code{make_file} will returns a character string in a format "accident_<year>.csv.bz2"
#' that can be used as a file name
#'
#' @examples
#' \dontrun{
#' makefilename(2013)
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read in Fatality Analysis Reporting System data files
#'
#' \code{fars_read_years} will read in multiple Fatality Analysis Reporting
#'   System data files based on the years provided.
#'
#' @param years A vector or list of years in numeric or integer format.
#'
#' @return \code{fars_read_years} will search for the file names based on the
#'   years provided. For example, if 2013:2014 is provided \code{fars_read_years}
#'   will search for the following files:
#'   \itemize{
#'     \item "accident_2013.csv.bz2"
#'     \item "accident_2014.csv.bz2"
#'   }
#'   If the files exist a list containing the respective data will be returned.
#'   If the files do not exist an error will be returned stating the invalid year(s).
#'
#'
#' @examples
#' fars_read_years(2013:2015)
#'
#' @importFrom dplyr %>% mutate select
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



#' Counts number of accidents per month and year
#'
#' The function calculates the number of accidents by year and by month in the US.
#' The accident files need to be in the working directory.
#' The years can be passed as a list or a vector.
#'
#'
#' \code{fars_summarize_years} will read in multiple Fatality Analysis Reporting
#'   System data files based on the years provided and summarise the number of
#'   observations by month and year.
#'
#' @param years The years relating to the file names to be read in
#'
#' @return \code{fars_summarize_years} will return a wide-formatted data frame.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013:2015)
#' }
#'
#' @importFrom dplyr %>% bind_rows group_by summarize
#'
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Plots the accidents on a US state map
#'
#' \code{fars_map_state} will plot the accidents on a map for a given state
#'   and year.
#'
#' @param state.num State number
#' @param years The year of concern
#'
#' @return \code{fars_map_state} will returns a plot of the accidents based on the \code{state.num}
#'  and \code{year} inputs. Returns an error if the state or year do not exist in the data set.
#'
#'
#' @seealso
#' \code{\link{make_filename}} to understand how the file name is created
#' \code{\link{fars_read}} to understand how the file is read in
#'
#' @examples
#' \dontrun{
#' fars_map_state(42, 2013)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
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

