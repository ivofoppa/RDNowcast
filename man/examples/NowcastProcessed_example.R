library(RDNowcast)

NCdata <- RDNowcast::RDdata  ### Loading the data set from the package

dateAnal <- as.Date( "2026-07-09")

ncdates <- NCdates_create(data = NCdata, NCdatesProp = seq.Date(dateAnal - 364 -5*7,dateAnal - 364 +5*7,by = "week"),dateAnal = dateAnal)

NCproc <- NowcastProcessed(data = NCdata,dateAnal = dateAnal,recentRef = dateAnal-4, NCsize = 10,unit = "week",cnames = c("sm","sll","sul","o"),
                                       NCdates = ncdates)

NCproc