library(RDNowcast)

NCdata <- RDNowcast::RDdata  ### Loading the data set from the package

dateAnal <- as.Date( "2026-07-09")
recentRef <- dateAnal-4
NCsize <- 20

nakt <- NCdata |> ### die "simulierten" Analysedata (nach gewählter zeitl. Perspektive)
  mutate(week = floor_date(reference_date,week_start = 1,unit = "weeks")) |> 
  filter(reference_date<=recentRef) |>
  summarize(.by = "week",n = n()) |> 
  slice_tail(n = NCsize) |> pull(n)

naktd <- NCdata |> ### die "simulierten" Analysedata (nach gewählter zeitl. Perspektive)
  mutate(week = floor_date(reference_date,week_start = 1,unit = "weeks")) |> 
  filter(reference_date<=recentRef) |>
  summarize(.by = "reference_date",n = n()) |> 
  slice_tail(n = NCsize) |> pull(n)

nobsd <- NCdata |> ### die "simulierten" Analysedata (nach gewählter zeitl. Perspektive)
  mutate(week = floor_date(reference_date,week_start = 1,unit = "weeks")) |> 
  filter(reference_date<=recentRef,
         report_date < dateAnal) |>
  summarize(.by = "reference_date",n = n()) |> 
  slice_tail(n = NCsize) |> pull(n)

nobs <- NCdata |> ### die "simulierten" Analysedata (nach gewählter zeitl. Perspektive)
  mutate(week = floor_date(reference_date,week_start = 1,unit = "weeks")) |> 
  filter(reference_date<=recentRef,
         report_date < dateAnal) |>
  summarize(.by = "week",n = n()) |> 
  slice_tail(n = NCsize) |> pull(n)

ls <- Test1(NCdata,dateAnal = dateAnal,recentRef = dateAnal-1,maxdelay = NCsize - 2,unit = "week")

NC <- f_N_arr(nobs,ls)

quantile(NC[,45],probs = c(.5,.025,.975),na.rm = T)

LawlessNC <- LawlessNowcast(data = NCdata,dateAnal = dateAnal,maxdelay = NCsize - 2,cnames = c("sm","sll","sul","o")) |> 
  mutate(nakt = nakt)

ncdates <- seq.Date(dateAnal - 364 - 5*7,dateAnal - 364 + 5*7,by = "week")

NCsize <- 60
RDNC <- RDNowcast(data = NCdata,dateAnal = dateAnal,NCsize =NCsize,recentRef = dateAnal - 1,NCdates = ncdates,unit = "week",nsamples = 10000)|> 
  mutate(nakt = nakt)

ls <- RDNowcastProbs(data = NCdata,dateAnal = dateAnal,NCsize =NCsize,recentRef = dateAnal - 4,NCdates = ncdates,unit = "week")
ls <- NowcastFull(data = NCdata,dateAnal = dateAnal,NCsize =NCsize,recentRef = dateAnal - 1,NCdates = ncdates,unit = "day")
  
ls1 <- ls
