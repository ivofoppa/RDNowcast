#' Helper function for the function \code{RDNowcast::Nowcast()}
#' 
#' @description
#' This helper function processes the draws from the imputed full numbers (without reporting delay), i.e. the posterior 
#' distributions for the full data. 
#' 
#' @import dplyr, lubridate
#' @param data A data frame containing data, with one column representing event times and one with reporting times.
#'     The data should be complete in the sense, that reporting delays no longer play a role. 
#' @param dateAnal A date, represented in the format "YYYY-mm-dd" that sets the date of analysis; if missing, the most recent report_date + 1 is chosen.
#' @param recentRef A Date object in the format "%YYYY-%mm-%dd" that represents the latest date to be considered in the analyses for events.
#' @param NCdates A date vector, representing the dates used for the completeness estimation; these dates have to correspond to the same day of week as dateAnal.
#' @param NCsize An integer, representing the length of the time series in chosen time units, for which completeness estimates should be obtained.
#' @param NCperiod The numbers of weeks used for data generation. By default 52 weeks.
#' @param reference_date A string variable representing the name of the column with event dates.
#' @param report_date A string variable representing the name of the column with reporting dates.
#' @param unit String representing time unit of analysis, week ("week") vs. day ("day").
#' @param nsamples An integer of default 100,000, representing the number of samples from the posterior distributions.
#' @param probs A numeric vector 0-1, representing the desired quantiles of the nowcast MCMC samples, besides the median.  
#' @param fd_distance Interger, the number of weeks after data are assumed to be complete. Only used if NCdates is not assigned.
#' @param NCperiods Integer, the number of data sets used for completeness estimation. Only used if NCdates not assigned. 
#' @param cnames A character vector with the desired names of the output table; the default values are \code{c(unit,"median","lowerCrI","upperCrI","observed")}, representing
#'     time (in chosen time units), the median of the MCMC samples, the lower credible interval (level chosen in the argument \code{probs}), the upper credible interval and the observed counts. 
#' @param tu_lab Desired label of time units in table.   
#' @returns A table with quantiles of the MCMC samples per date.
#' @example man/examples/NowcastProcessed_example.R
#' @export
NowcastProcessed <- function(data, dateAnal = NULL, recentRef = NULL, NCdates = NULL, NCsize = 10, week_start = 2,
                             reference_date = "reference_date", report_date = "report_date", 
                             unit = "week",nsamples = 100000, probs = c(0.025,0.975),
                             fd_distance = 20, NCperiods = 52, cnames = c("median","lowerCrI","upperCrI","observed"),tu_lab = "week") {
  
  data <- data |> 
    rename(reference_date = reference_date, report_date = report_date)
  
  if(is.null(dateAnal)) {
    dateAnal <- data |> 
      group_by(report_date) |> 
      summarize(n = n()) |> ungroup() |> 
      filter(n >= 5) |> 
      tail(n = 1) |> pull(report_date) + 1
  } else {
    dateAnal <- as.Date(dateAnal)
  }
  
  if(is.null(NCdates)) {
    NCdates <- NCdates_create(
      data = data,
      dateAnal = dateAnal,
      fd_distance = fd_distance,
      NCperiods = NCperiods,
      reference_date = reference_date
    )
  } else {
    NCdates <- NCdates_create(
      data = data,
      dateAnal = dateAnal,
      fd_distance = fd_distance,
      NCperiods = NCperiods,
      NCdatesProp = NCdates,
      reference_date = reference_date
    )
  }
  
  if(is.null(recentRef)) {
    if(wday(dateAnal) %in% 1:2) {
      while(wday(recentRef)!=6) {
        recentRef <- recentRef - 1
      }
    } else {
      if(unit=="week") {
        while(wday(recentRef)!=1) {
          recentRef <- recentRef - 1
        }
      }
    }
  }
  
  recentRef <- as.Date(recentRef)
  
  offset <- (dateAnal - recentRef) |> as.integer()
  
  NC <- Nowcast(df = data, dateAnal = dateAnal, offset = offset, week_start = week_start, NCdates = NCdates, NCsize = NCsize,  
                reference_date = "reference_date", report_date = "report_date", 
                unit = unit, nsamples = nsamples) 
  
  if (unit=="week") {
    mult <- 7;

    dateseq <- seq.Date(recentRef - mult*(NCsize),length.out = NCsize,by="week") + 1

    n_obs <- data |> ### die "simulierten" Analysedaten (nach gewählter zeitl. Perspektive)
      filter(reference_date <= recentRef,
             report_date < dateAnal) |> 
      mutate(week = floor_date(reference_date,unit = "week",week_start =  (wday(recentRef)))) |> 
      group_by(week) |> 
      summarize(n = n()) |> ungroup() |>
      slice_tail(n = NCsize) |> pull(n) |> unlist() |> as.vector()
  } else {
    mult <- 1;
    dateseq <- seq.Date(recentRef - mult*(NCsize),length.out = NCsize,by="day") + 1

    n_obs <- data |> ### die "simulierten" Analysedaten (nach gewählter zeitl. Perspektive)
      filter(reference_date <= recentRef,
             report_date < dateAnal) |> 
      group_by(reference_date) |> 
      summarize(n = n()) |> ungroup() |>
      slice_tail(n = NCsize) |> pull(n) |> unlist() |> as.vector()
  }
  
  tab <- sapply(1:NCsize, function(d) quantile(NC[,d],probs = c(0.5,probs))) |>
    t() |>
    as.data.frame() |>
    mutate(date=dateseq) |> select(4,1,2,3) |> 
    mutate(n = n_obs,
           across(2:4,\(x) as.integer(x)))
  
  colnames(tab) <- c(tu_lab,cnames)
  
  tab
}
