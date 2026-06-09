library(dplyr)
library(RDNowcast)

datean <- as.Date("2025-05-15") ## Day of analysis, here a Thursday 

ncdates <- NCdates_create(data = NCdata,dateAnal = datean)

NC <- Nowcast(data = NCdata, dateAnal = datean, NCdates = ncdates) ### List containing observed (incomplete) and complete data.

head(NC)
