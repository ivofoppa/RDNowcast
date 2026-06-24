#' Function to set the dates for the data completeness estimation
#' 
#' @description
#' This function creates a vector of dates, each representing the same day of the week as \code{dateAnal}. 
#' 
#' @import lubridate
#' @param dateAnal Both character input (format "YYYY-mm-dd") or date accepted, representing the date of analysis; the most recent data is assumed to have been submitted the day before or prior to that.
#' @param data A data frame; each row corresponds to one report/case.
#' @param fd_distance Interger, the number of weeks after data are assumed to be complete. Only used if NCdates is not assigned.
#' @param NCperiods Integer, the number of data sets used for completeness estimation. Only used if NCdates not assigned. 
#' @param NCdatesProp A date vector with proposed dates for the Nowcast estimation.
#' @param reference_date A character variable representing the name of the column with event dates.
#' @returns A date vector.
#' @example man/examples/RDdates_create_example.R
#' @export

NCdates_create <- function(data, dateAnal, fd_distance = 10, NCperiods = 52, NCdatesProp = NULL,
                           reference_date = "reference_date") {
  
  dateAnal <- as.Date(dateAnal)
  refday <- wday(dateAnal)
  
  if(is.null(NCdatesProp)) {
    dseq0 <- seq.Date(dateAnal - (fd_distance + NCperiods)*7,length.out = NCperiods,by="week")
    dseqsel <- dseq0 |> 
      sapply(function(d) d %in% unique(data$reference_date))
    dseq0[dseqsel]
  } else if (is.Date(NCdatesProp)) {
    dseq0 <- NCdatesProp
    dseqsel <- dseq0 |> 
      sapply(function(d) d %in% unique(data$reference_date))
    dseq <- dseq0[dseqsel]
    sapply(dseq, function(d) if_else(wday(d)==refday,d,d + (refday - wday(d)))) |> as.Date()
  } else {
    stop("No adequate date vector provided or found!")
  }
}
