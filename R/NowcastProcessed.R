#' Wrapper function for the function \code{RDNowcast::Nowcast()} that computes quantiles for the Nowcast estimates.
#' 
#' @description
#' This function processes the draws from the imputed full numbers (without reporting delay), i.e. the posterior 
#' distributions for the full data. 
#' 
#' @import dplyr,lubridate,stringr
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
#' @param probs A numeric vector 0-1, representing the desired quantiles of the nowcast estimates.  
#' @param fd_distance Interger, the number of weeks after data are assumed to be complete. Only used if NCdates is not assigned.
#' @param NCperiods Integer, the number of data sets used for completeness estimation. Only used if NCdates not assigned. 
#' @returns A table with quantiles of the MCMC samples per date.
#' @example man/examples/NowcastProcessed_example.R
#' @export
NowcastProcessed <- function(data, dateAnal = NULL, NCdates = NULL, NCsize = 10,  
                             reference_date = "reference_date", report_date = "report_date", 
                             week_start = 2, unit = "week",nsamples = 100000, probs = c(0.5,0.025,0.975),
                             fd_distance = 20, NCperiods = 52) {
  
  
  if(is.null(dateAnal)) {
    eval(parse(text = str_c("rep_date_max <- data[\"",reference_date,"\"] |> rename(reference_date = ",reference_date,") |> 
                        slice_max(reference_date) |> unlist() |> as.vector() |> unique() |> 
                        as.Date(origin=\"1970-01-01\")")))
    dateAnal <- rep_date_max + 1
  }
  
  if(is.null(NCdates)) {
    NCdates <- seq.Date(dateAnal - 7*fd_distance - NCperiods*7,length.out = NCperiods,by = "weeks")
  }
  
  NC <- Nowcast(data, dateAnal, NCdates, NCsize, reference_date,report_date, week_start, unit,
                nsamples)
  
  day_anal <- wday(dateAnal)
  
  if (unit=="week") {
    dtecomp <- day_anal - week_start;
    mult <- 7;
    dateseq <- seq.Date(dateAnal - dtecomp - mult*(NCsize),length.out = NCsize,by=unit)
  } else {
    dtecomp <- 1;
    mult <- 1;
    dateseq <- seq.Date(dateAnal - dtecomp - mult*(NCsize) + 1,length.out = NCsize,by=unit)
  }
  
  sapply(1:NCsize, function(d) quantile(NC[,d],probs = probs)) |>
    t() |>
    as.data.frame() |>
    mutate(date=dateseq) |> select(4,1,2,3)
}
