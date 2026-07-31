library(epinowcast)
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(RDNowcast)
library(tidyr)

nat_germany_hosp <-
  germany_covid19_hosp[location == "DE"][age_group == "00+"] |>
  enw_filter_report_dates(latest_date = "2021-10-01")

data <- nat_germany_hosp |> group_by(reference_date) |> arrange(report_date) |> 
  mutate(cs2 = lag(confirm,n =1)) |> ungroup() |> 
  mutate(cs2=if_else(is.na(cs2),0,cs2),
         n = confirm - cs2) |> select(-c(confirm,cs2)) |> 
  filter(n>0)

dataraw <- data[1,1:4]
dataraw <- dataraw[-1,]
for(k in 1:nrow(data)) {
  dftmp <- do.call("rbind", replicate(
    as.vector(data[k,5] |> unlist()), data[k,1:4], simplify = FALSE))
  
  dataraw <- dataraw |> dplyr::add_row(dftmp)
}

dataraw1 <- dataraw |> 
  select(-c(location,age_group))

datean <- as.Date("2021-08-22")+1

ncdates <- seq.Date(datean - 5*7,length.out = 10,by="week")
ncsize <- 22

NC <- NowcastProcessed(dataraw1,dateAnal = datean,NCdates = ncdates,unit = "day",NCsize = ncsize)

dataanal <- dataraw1 |> 
  filter(reference_date<datean,
         report_date<datean) |> 
  group_by(reference_date) |>
  summarize(n = n()) |> ungroup() |>
  slice_tail(n = ncsize)

datafull <- dataraw1 |> 
  filter(reference_date<datean) |> 
  group_by(reference_date) |>
  summarize(n = n()) |> ungroup() |>
  slice_tail(n = ncsize)

NC1 <- NC |> 
  mutate(n = dataanal$n,
         full = datafull$n, date = as.Date(date)) |> 
  rename(med = '50%',ll = '2.5%',ul='97.5%') |> 
  rename(reference_date = date)

write.csv2(NC,"NC.csv",row.names = FALSE)

knitr::kable(NC1)

NC2 <- NC1 |> 
  mutate(across(.cols='med':'full', \(x) cumsum(x)))

######### Epinowcast

# nat_germany_hosp <-
#   germany_covid19_hosp[location == "DE"][age_group == "00+"] |>
#   enw_filter_report_dates(latest_date = "2021-10-01")
# 
# retro_nat_germany <- nat_germany_hosp |>
#   enw_filter_report_dates(remove_days = 40) |>
#   enw_filter_reference_dates(include_days = 40)

# retro_nat_germany

dateseq <- RDdata$reference_date |> floor_date(week_start = 1,unit = "week") |> 
  unique() |> sort()

nat_germany_hosp <- RDdata |> 
  mutate(across(reference_date:report_date, \(x) floor_date(x,week_start = 1,unit = "week"))) |> 
  group_by(reference_date,report_date) |> summarize(confirm = n()) |> ungroup() |> 
  tidyr::complete(reference_date=dateseq,report_date=dateseq) |> 
  replace_na(list(confirm=0))

retro_nat_germany <- nat_germany_hosp |>
  enw_filter_report_dates(latest_date = datean -1) |>
  enw_filter_reference_dates(latest_date = datean - 4)

retro_nat_germany

pobs <- enw_preprocess_data(
  retro_nat_germany, max_delay = 10, timestep = "week"
)

pobs_full
summary(pobs_full)

plot(pobs_full, type = "obs") +
  labs(y = "Hospitalisations", x = "Reference date")

pobs <- enw_preprocess_data(
  retro_nat_germany, max_delay = 4,timestep = "week"
)

pobs |> summary()

latest_germany_hosp <- nat_germany_hosp |>
  enw_filter_reference_dates(earliest_date = "2025-03-03",
                             latest_date = datean - 4)


head(latest_germany_hosp, n = 10)

model <- enw_model(target_dir = "C:/Analyse/Mortalitaetsanalysen")

options(mc.cores = 2)
fit_opts <- enw_fit_opts(
  save_warmup = FALSE,
  pp = TRUE,
  chains = 2,
  iter_sampling = 500,
  iter_warmup = 500,
  adapt_delta = 0.9,
  show_messages = interactive()
)

nowcast_default <- epinowcast(
  data = pobs,
  fit = fit_opts,
  model = model
)

plot(
  nowcast_default,
  latest_obs = latest_germany_hosp
)
plot(nowcast_default, type = "posterior")

expectation_module <- enw_expectation(
  ~ 1 + rw(week) + (1 | day_of_week),
  data = pobs
)

np_reference <- enw_reference(
  parametric = ~0,
  non_parametric = ~ 1 + rw(delay) + (1 | day_of_week),
  data = pobs
)

nowcast_np <- epinowcast(
  data = pobs,
  expectation = expectation_module,
  reference = np_reference,
  fit = fit_opts,
  model = model
)

# extract samples
samples <- summary(
  nowcast_np, type = "nowcast_samples"
)

est <- samples |> as.data.frame() |>
  filter(!is.na(sample)) |> 
  select(any_of(c("reference_date","report_date","confirm","sample"))) |> 
  group_by(reference_date) |> summarise(est = median(sample),llen = quantile(sample,p=.025),ulen = quantile(sample,p=.975)) |> 
  mutate(confirm =(samples |> filter(!is.na(sample)) |> select(reference_date,report_date,confirm) |> distinct() |> pull(confirm)),
         reference_date = as.Date(reference_date))

esttot <- est |> filter(report_date>="2021-08-01") |> 
  left_join(NC1,by = reference_date)

ggplot(esttot,aes(x = reference_date,y=full)) +
  geom_point(color = "blue",size =2) + ylim(0,600) +
  geom_line(aes(y = est),color = "green",size = 2) +
  geom_line(aes(y = med),color = "orange",size = 2)
