NowcastFull <- function(data = RDNowcast::RDdata, date_anal = NULL,
                         NCdates, NCsize = 10, NCperiod = 52, 
                         reference_date = "reference_date", report_date = "report_date", 
                         week_start = 2, unit = "week") {

    NCprobs <- NowcastProb(data = data,NCdates = NCdates, NCsize = NCsize,  
                       reference_date = reference_date, report_date = report_date, 
                       week_start = week_start, unit = unit,nsamples = 100000)
  
  if (is.null(date_anal)) {
    date_anal <- max(data$reference_date) + 1
  }
  date_analpr <- as.Date(data_anal)
  wd_anal <- date_analpr |> wday()
  
  day_comp <- if_else(unit=="week",wd_anal - week_start + 1,0)
  
  
  data_an <- data |> ### 
    filter(report_date < data_analpr - 1,
           reference_date<(data_analpr - day_comp - 1)) |> 
    group_by(reference_date) |> 
    summarize(n = n()) |> ungroup() |>
    slice_tail(n = ncsize)
  
  pmat <- pSampleMult(NC)
  samples1 <- matrix((pmat |> unlist()), ncol = ncsize)
  samples <- samples1[sample(nrow(samples1),size=10000,replace=TRUE),] # Source - https://stackoverflow.com/a/7806767
  
  narr <- f_N_arr(analysedata$n,samples)
  # ls <- NowcastDataMat(data,NCstart = ncs)
  ci <- sapply(seq_along(analysedata$n), function(k) quantile(narr[,k],probs = c(0.5,.025,.975)))
  #################################################################################################
  
  signal_current0 <- tibble(date = analysedata$reference_date, o = analysedata$n, 
                             n = ci[1,],ll = as.integer(ci[2,]),ul = as.integer(ci[3,]))
  # ncs0 <- ncs+7
  # ncs <- ncs-7
  
  # data |> 
  #   filter(reference_date>=ncs,reference_date<(ncs+7),report_date<(ncs0+7+3)) |> 
  #   nrow()
  
  # group_by(week)
  echtdaten <- data |> ### die "simulierten" analysedata (nach gewählter zeitl. Perspektive)
    filter(reference_date<(date1-3)) |> 
    group_by(reference_date) |> 
    summarize(n = n()) |> ungroup() |>
    slice_tail(n = ncsize)
  
  signal_current <- signal_current0
  signal_current$n_komp <- echtdaten$n
  
  ggplot(signal_current,aes(x = date,y = o)) +
    ylim(0,300) +
    geom_point(color = "red") +
    geom_point(aes(y = n_komp),color = "blue") +
    geom_errorbar(aes(y=n,ymin = ll,ymax = ul,fill = "red"),alpha = 0.8,width = 1)
  
}
