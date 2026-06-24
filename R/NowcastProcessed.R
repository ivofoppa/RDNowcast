#' Wrapper function for the function \code{RDNowcast::Nowcast()}
#' 
#' @description
#' This helper function processes the draws from the imputed full numbers (without reporting delay), i.e. the posterior 
#' distributions for the full data. 
#' 
#' @import dplyr, lubridate
#' @param data A data frame containing data, with one column representing event times and one with reporting times.
#'     The data should be complete in the sense, that reporting delays no longer play a role. 
#' @param dateAnal A date, represented in the format "YYYY-mm-dd" that sets the date of analysis; if missing, the most recent report_date + 1 is chosen.
#' @param NCdates A date vector, representing the dates used for the completeness estimation; these dates have to correspond to the same day of week as dateAnal.
#' @param NCsize An integer, representing the length of the time series in chosen time units, for which completeness estimates should be obtained.
#' @param NCperiod The numbers of weeks used for data generation. By default 52 weeks.
#' @param reference_date A string variable representing the name of the column with event dates.
#' @param report_date A string variable representing the name of the column with reporting dates.
#' @param day_anal Integer representing the day of the analysis, with 1 (sunday) to 7 (saturday); the most recent reporting date is assumed
#'     to be on day_anal - 1.
#' @param week_start Integer representing the start of the week; default (monday: 2).
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
NowcastProcessed <- function(data, dateAnal = NULL, NCdates = NULL, NCsize = 10,  
                             reference_date = "reference_date", report_date = "report_date", 
                             week_start = 2, unit = "week",nsamples = 100000, probs = c(0.025,0.975),
                             fd_distance = 20, NCperiods = 52, cnames = c("median","lowerCrI","upperCrI","observed"),tu_lab = "week") {
  
 
   data <- data |> 
     rename(reference_date = reference_date, report_date = report_date)
   
   if(is.null(dateAnal)) {
     rep_date_max <- data |> 
       slice_max(reference_date) |> pull(reference_date) |> unique() |>  
       as.Date(origin="1970-01-01")
     
    dateAnal <- rep_date_max + 1
  }
  
  if(is.null(NCdates)) {
    NCdates <- seq.Date(dateAnal - 7*fd_distance - NCperiods*7,length.out = NCperiods,by = "weeks")
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
  
  NC <- Nowcast(data, dateAnal = dateAnal, NCdates = NCdates, NCsize = NCsize, week_start = week_start, unit = unit,
                nsamples = nsamples)
  
  day_anal <- wday(dateAnal)
  cnames <- c(tu_lab,cnames)
  
  if (unit=="week") {
    dtecomp <- day_anal - week_start;
    mult <- 7;
    dateseq <- seq.Date(dateAnal - dtecomp - mult*(NCsize),length.out = NCsize,by=unit)
    w_start <- if_else((6 + week_start) %% 7==0,7,(6 + week_start) %% 7)
    
    n_obs <- data |> ### die "simulierten" Analysedaten (nach gewählter zeitl. Perspektive)
      filter(reference_date < (dateAnal - dtecomp),
             report_date < dateAnal) |> 
      mutate(week = floor_date(reference_date,unit = "week",week_start =  w_start)) |> 
      group_by(week) |> 
      summarize(n = n()) |> ungroup() |>
      slice_tail(n = NCsize) |> pull(n) |> unlist() |> as.vector()
  } else {
    dtecomp <- 1;
    mult <- 1;
    dateseq <- seq.Date(dateAnal - dtecomp - mult*(NCsize) + 1,length.out = NCsize,by=unit)
    
    n_obs <- data |> ### die "simulierten" Analysedaten (nach gewählter zeitl. Perspektive)
      filter(reference_date < dateAnal,
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
  
  colnames(tab) <- cnames
  
  tab
}
