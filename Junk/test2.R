
# pacman::p_load(dplyr,data.table,ggplot2,tibble,Rcpp,lubridate,stingr)

data <- RDNowcast::RDdata

today1 <- max(data$report_date) + 1 - 364

ncdates <- seq.Date(today1 - 40*7,length.out = 12,by = "week")
datean <- today1
# unit <- "day"

unit <- "day"
NCsize <- 20

if (unit=="week") {
  dtecomp <- day_anal - week_start;
  mult <- 7;
} else {
  dtecomp <- 0;
  mult <- 1;
}

probs <- c(0.5,.025,.975)

ls <- NowcastProcessed(data, NCdates = ncdates,unit = "day",NCsize = NCsize)

analysedata <- data |> ### die "simulierten" Analysedata (nach gewählter zeitl. Perspektive)
  group_by(reference_date) |>
  summarize(n = n()) |> ungroup() |>
  slice_tail(n = NCsize)

ls2 <- f_N_arr(analysedata$n,ls) 

ls <- Nowcast(data, dateAnal = datean,unit = "week")

dateseq <- seq.Date(datean - dtecomp - mult*(NCsize - 1),length.out = NCsize,by=unit)

ls1 <- sapply(1:NCsize, function(d) quantile(ls[,d],probs = probs)) |> t() |> as.data.frame() |> 
  mutate(date=dateseq) |> select(4,1,2,3)

# unit <- "week"; NCsize <- 10

NC <- NowcastProcessed(data,dateAnal = datean,NCsize = NCsize,unit = unit)
NC <- NowcastProcessed(data,NCsize = NCsize,unit = unit)
# NC <- NowcastProcessed(data,NCperiods = 52)

echtdaten <- data |> ### die "simulierten" Analysedata (nach gewählter zeitl. Perspektive)
  filter(reference_date<(datean)) |>
  group_by(reference_date) |>
  summarize(n = n()) |> ungroup() |>
  slice_tail(n = NCsize)

NC1 <- NC |> mutate(echt = echtdaten$n,
                    n = analysedata$n) |> rename(est = '50%')

analysedata <- data |> ### die "simulierten" Analysedata (nach gewählter zeitl. Perspektive)
  mutate(week = floor_date(reference_date,week_start = 1,unit = "weeks")) |> 
  filter(reference_date<(datean-3)) |>
  group_by(week) |>
  summarize(n = n()) |> ungroup() |>
  slice_tail(n = NCsize)

NC1 <- NC |> 
  mutate(truth = analysedata$n)

library(Rcpp)

sourceCpp("~/Projects/GitHub/RDNowcast/Junk/test3.cpp")

Test3(data = data)
