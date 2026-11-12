
# pacman::p_load(dplyr,data.table,ggplot2,tibble,Rcpp,lubridate,stringr)

# library(RDNowcast)

pfad <- "Q:/Infektionsepidemiologie/Projekte/Sterblichkeitsmonitoring/Mortalitätsdaten/Daten/RDdata.csv"
data <- read.csv2(pfad) |> 
  filter(!is.na(reference_date),!is.na(report_date))

write.csv(data,pfad,row.names = FALSE)

data <- RDdata

data1 <- data |> 
  mutate(week = floor_date(reference_date,uni = "week",week_start = 1)) |> 
  summarize(.by = "week",n = n())

data |> summarize(.by = "reference_date",n = n()) |> 
  ggplot(aes(x = reference_date,y = n)) + geom_col()

dateAnal <- "2026-07-09" |> as.Date()
recentRef <- dateAnal - 4

ntrue <- data |> 
  mutate(week = floor_date(reference_date,uni = "week",week_start = 1)) |>
  summarize(.by = "week",ntrue = n()) 

ncdates <- NCdates_create(data = data, NCdatesProp = seq.Date(dateAnal - 364 -5*7,dateAnal - 364 +5*7,by = "week"),
                          dateAnal = dateAnal)

ls <- RDNowcast(data = data,unit = "week",dateAnal = dateAnal,recentRef = recentRef,NCdates = ncdates)
ls2 <- LawlessNowcast(data = data,maxdelay = 25,dateAnal = dateAnal)

ls |> left_join(ntrue,by = "week")

ls2 |> left_join(ntrue,by = "week")
