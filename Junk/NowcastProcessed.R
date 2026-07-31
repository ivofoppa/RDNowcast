NowcastProcessed2 <- function(data, dateAnal = NULL, NCdates = NULL, NCsize = 10,  
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
  } else {
    dtecomp <- 0;
    mult <- 1;
  }
  
  dateseq <- seq.Date(dateAnal - dtecomp - mult*(NCsize),length.out = NCsize,by=unit)
  
  sapply(1:NCsize, function(d) quantile(NC[,d],probs = probs)) |>
    t() |>
    as.data.frame() |>
    mutate(date=dateseq) |> select(4,1,2,3)

  }
