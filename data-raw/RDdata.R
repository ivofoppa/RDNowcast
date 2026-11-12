## code to prepare `RDdata` dataset goes here
library(dplyr,lubridate)
RDdata <- read.csv("./data/RDdata.csv") |> 
  mutate(across(everything(),\(x) as.Date(x))) |> filter(!is.na(reference_date),!is.na(report_date))

usethis::use_data(RDdata, overwrite = TRUE)
