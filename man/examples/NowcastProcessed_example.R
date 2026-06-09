library(RDNowcast)

NCdata <- RDNowcast::RDdata  ### Loading the data set from the package

datean <- as.Date("2026-05-14")  ### Setting the date of the analysis; the most recent data were reported one day prior

ncdates <- NCdates_create(data = NCdata, dateAnal = date_anal, NCperiods = 5)

NC <- NowcastProcessed(data,dateAnal = datean,NCsize = 10 ,unit = "week")

NC
