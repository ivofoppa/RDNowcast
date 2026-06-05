pacman::p_load(dplyr,lubridate,RDNowcast)

data <- RDNowcast::RDdata  ### Loading the data from the R package

today <- data$report_date |> max() + 1 ## Assuming the data set was submitted "yesterday"

day_anal <- 5 ## Day of analysis, here 5: Thursday
date_anal <- today-(wday(today-day_anal)) ## forcing that day to be Thursday 

ncstart <- date_anal - 7*24 ### Most recent date for data is assumed to be complete
ncdates <- seq.Date(ncstart - 51*7,length.out = 52,by = "weeks")

ncsize <- 10 ### Length in time units of the nowcasting period in time units, here weeks

Nowcast_probs <- NowcastProb(data = data, NCdates = ncdates) ### List containing observed (incomplete) and complete data.
