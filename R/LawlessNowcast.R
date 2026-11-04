#' Helper function for the function \code{RDNowcast::NowcastProbsLawless()}
#' 
#' @description
#' This helper function generates full numbers (without reporting delay), i.e. the posterior 
#' distributions for the full data and organizes the results in a table. 
#' 
#' @import dplyr, lubridate
#' @param data A data frame containing data, with one column representing event times and one with reporting times.
#'     The data should be complete in the sense, that reporting delays no longer play a role. 
#' @param dateAnal A date, represented in the format "YYYY-mm-dd" that sets the date of analysis; if missing, the most recent report_date + 1 is chosen.
#' @param week_start An integer, denoting start of the week (1: Sunday); default value is 2 (Monday).
#' @param reference_date A string variable representing the name of the column with event dates.
#' @param report_date A string variable representing the name of the column with reporting dates.
#' @param nsamples An integer of default 100,000, representing the number of samples from the posterior distributions.
#' @returns A table with quantiles of the MCMC samples per date.
#' @example man/examples/NowcastProcessed_example.R
#' @export
LawlessNowcast <- function(data, dateAnal = NULL, maxdelay = 25, week_start = 2,
                           reference_date = "reference_date", report_date = "report_date", 
                           nsamples = 100000, probs = c(0.025,0.975), cnames = c("median","lowerCrI","upperCrI","observed"),
                           tu_lab = "week") {
  
  data <- data |> 
    rename(reference_date = reference_date, report_date = report_date)
  
  NCsize <- maxdelay + 2
  
  if(is.null(dateAnal)) {
    dateAnal <- data |> 
      group_by(report_date) |> 
      summarize(n = n()) |> ungroup() |> 
      filter(n >= 5) |> 
      tail(n = 1) |> pull(report_date) + 1
  } else {
    dateAnal <- as.Date(dateAnal)
  }
  
  NCprobs <- LawlessNowcastProbs(data = data, maxdelay = maxdelay, 
                                 dateAnal = dateAnal, week_start = week_start, 
                                reference_date = "reference_date", report_date = "report_date", 
                                nsamples = nsamples) 
  
  begweek <- dateAnal - 7
  
  while(wday(begweek)!=week_start) {
    begweek <- begweek - 1
  }
  
  mult <- 7;
  
  dateseq <- seq.Date(begweek - mult*(NCsize-1),length.out = NCsize,by="week")
  
  n_obs <- data |> ### die "simulierten" Analysedaten (nach gewählter zeitl. Perspektive)
    filter(reference_date < begweek + 7,
           report_date < begweek + 7) |> 
    mutate(week = floor_date(reference_date,unit = "week",week_start = week_start - 1)) |> 
    group_by(week) |> 
    summarize(n = n()) |> ungroup() |>
    slice_tail(n = NCsize) |> pull(n) |> unlist() |> as.vector()
  
  NC <- f_N_arr(xvec = n_obs,pmat = NCprobs)
  
  tab <- sapply(1:NCsize, function(d) quantile(NC[,d],probs = c(0.5,probs))) |>
    t() |>
    as.data.frame() |>
    mutate(date=dateseq) |> select(4,1,2,3) |> 
    mutate(n = n_obs,
           across(2:4,\(x) as.integer(x)))
  
  colnames(tab) <- c(tu_lab,cnames)
  
  tab
}
