RDdata <- RDNowcast::RDdata

today <- RDdata$report_date |> max() + 1 ## Most recent report_date yesterday
day_anal <- 5 ### Weekday of analysis, here Thursday
ncstart <- today-(wday(today()-day_anal))- 7*20 ### Most recent date for data completeness estimation
ncsize <- 20 ### Length of the nowcasting period in time units

NowcastData <- NowcastDataMat(RDdata,NCstart = ncstart,NCperiod=52) ### List containing observed (incomplete) and complete data.
