## code to prepare `RDdata` dataset goes here
library(dplyr,lubridate)
RDdata <- read.csv("./data/RDdata.csv") |> 
  mutate(across(everything(),\(x) as.Date(x))) |> 
  filter(report_date<"2026-05-14")

usethis::use_data(RDdata, overwrite = TRUE)
