pacman::p_load(dplyr,lubridate,RDNowcast)

data <- RDNowcast::RDdata  ### Loading the data from the R package

NCdates <- NCdates_create(data = data,dateAnal="2025-05-15")
ncsize <- 10 ### Length in time units of the nowcasting period in time units, here weeks

Nowcast_probs <- NowcastProb(data = data, NCdates = NCdates) ### List containing observed (incomplete) and complete data.
