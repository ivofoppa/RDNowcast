library(RDNowcast)
library(dplyr)
library(lubridate)

NCdata <- RDNowcast::RDdata  ### Loading the data set from the package

dateAnal <- as.Date( "2026-07-09")
recentRef <- dateAnal - 4

ncdates <- NCdates_create(data = NCdata, NCdatesProp = seq.Date(dateAnal - 364 -5*7,dateAnal - 364 +5*7,by = "week"),dateAnal = dateAnal)

NCprobs <- RDNowcastProbs(data = NCdata,dateAnal = dateAnal,recentRef = recentRef, NCsize = 10,unit = "week",
                NCdates = ncdates)

nobs <- NCdata |> 
  filter(reference_date <= recentRef,report_date < dateAnal) |> 
  mutate(week = floor_date(reference_date,unit = "week",week_start = 1)) |> 
  summarize(.by = "week",n = n()) |> 
  tail(n = 10) |> pull(n)

NC <- f_N_arr(xvec = nobs, pmat = NCprobs) ### Generating samples from posteriors
