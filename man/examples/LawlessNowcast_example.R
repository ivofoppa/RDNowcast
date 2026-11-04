library(RDNowcast)

NCdata <- RDNowcast::RDdata  ### Loading the data set from the package

dateAnal <- as.Date( "2026-07-09")

NC <- LawlessNowcast(data = NCdata,dateAnal = dateAnal,maxdelay = 15,cnames = c("sm","sll","sul","o"))

NC
