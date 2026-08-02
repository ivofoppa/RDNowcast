library(dplyr)
library(RDNowcast)

NCdates <- NCdates_create(data = NCdata,dateAnal = "2025-05-15", NCperiods = 5)

NC <- Nowcast(data = NCdata, dateAnal = as.Date("2025-05-15"), NCdates = NCdates, offset = 4) ### List containing observed (incomplete) and complete data.

head(NC)
