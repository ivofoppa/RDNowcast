library(RDNowcast)

NCdata <- RDNowcast::RDdata  ### Loading the data set from the package

dateAnal <- as.Date("2026-05-13")  ### Setting the date of the analysis; the most recent data were reported one day prior

NCdates <- NCdates_create(data = NCdata, dateAnal = dateAnal, NCperiods = 5)

NCproc <- NowcastProcessed(data = NCdata, dateAnal = dateAnal, offset = 4, NCsize = 10 ,unit = "week")

NCproc
