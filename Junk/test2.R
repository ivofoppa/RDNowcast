
# pacman::p_load(dplyr,data.table,ggplot2,tibble,Rcpp,lubridate,stringr)

# library(RDNowcast)

data <- RDdata

dateAnal <- "2026-07-09" |> as.Date()
recentRef <- dateAnal - 1

ls <- Test1(data, dateAnal = dateAnal, recentRef = recentRef,maxdelay = 25)
ls2 <- LawlessNowcast(data, maxdelay = 25,dateAnal = datean)

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
