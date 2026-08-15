library(dplyr)
library(RDNowcast)

NCdates <- NCdates_create(data = RDdata,dateAnal = "2025-05-15", NCperiods = 5)

NC <- NowcastFull(data = NCdata, dateAnal = as.Date("2025-05-15"), recentRef = as.Date("2025-05-15") - 4, NCdates = NCdates) ### List containing observed (incomplete) and complete data.

head(NC)
