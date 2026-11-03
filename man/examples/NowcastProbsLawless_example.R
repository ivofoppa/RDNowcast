library(RDNowcast,dplyr,lubridate)

NCdata <- RDNowcast::RDdata  ### Loading the data set from the package

dateAnal <- as.Date( "2026-07-09") ## a Thursday

weekstart <- 2
maxdelay <- 15
### Generating the completeness probabilies
NCprobs <- NowcastProbsLawless(data = data,dateAnal = dateAnal,maxdelay = maxdelay,nsamples = 10000, week_start = weekstart)

### Retrospectively generating the observed numbers
nobs <- data |> 
  mutate(week = floor_date(reference_date,unit = "week", week_start = weekstart - 1)) |> 
  filter(reference_date < dateAnal - 3,report_date < dateAnal - 3) |> 
  summarise(.by = "week",n = n()) |> 
  arrange(week) |> pull(n) |> tail(n = maxdelay + 2)

### Generating the Nowcast estimates
NCestimates <- f_N_arr(nobs,NCprobs) ### 10000 samples from the posterior distribution of the Nowcast estimates for the past 17 weeks

