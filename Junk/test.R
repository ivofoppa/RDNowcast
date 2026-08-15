# rm(list = ls())
paketmussliste <- c("dplyr","tidyr","lubridate","data.table","stringr","estatistikR") ### diese Pakete werden benötigt
paketegeladen <- pacman::p_loaded() ### Diese Pakete sind schon geladen
paketliste <- setdiff(paketmussliste,paketegeladen) ### Diese Pakete müssen noch geladen werden!

pacman::p_load(char = paketliste, install = FALSE)

importpfad <- "/home/ifoppa/Downloads"

# Setzen des aktuellen Datums
# Setzen der Working Directory
## Neue Sterblichkeitsdaten holen, wenn vorhanden
#rm(list = ls())
#https://erhebungsportal.estatistik.de/Erhebungsportal/Erhebungsportal.html#
###################################################################################################

  ###################################################################################################
  ###  Zufügen der aktuellen Daten   ################################################################
  # ###################################################################################################
  user_auth <- list(
    username = "mortsurv@hlfgp.hessen.de",
    password = "Ai5kF!23"
  )

  df_nachrichten <- get_nachrichten(user_auth = user_auth) |>
    get_anhaenge_info(user_auth = user_auth)

  df_nachrichten_downloaded <- df_nachrichten |>
    filter(absender == "Sterbefalldaten RKI") |>
    slice_max(versanddatum) |>
    download_anhaenge(user_auth = user_auth, path = importpfad)


    unzip(file.path(importpfad,df_nachrichten_downloaded$anhang_dateiname),exdir = importpfad)
    
  dateinme0 <- list.files(path=importpfad,pattern = "Land_06_basis",full.names = TRUE) |> 
    file.info() |> slice_max(mtime) |> 
    row.names()
###################################################################################################
###################################################################################################
  daten <- fread(dateinme0,quote = "\"",sep = "auto") %>% rename_all(tolower) %>%
    # mutate(across(eingang:sterbedatum,~as_date(.x,format = "%Y%m%d")))
    mutate(across(c(eingang,sterbedatum), ~ as.Date(as.character(.x),"%Y%m%d")),
           woche = floor_date(sterbedatum,unit = "week",week_start = 1)) |> 
    filter(!is.na(eingang),!is.na(sterbedatum)) |> 
    rename(reference_date = sterbedatum,report_date =eingang) |> 
    mutate(woche = floor_date(reference_date,unit = "week",week_start = 1))
# heute0 <- today()
  
heute0 <- as.Date("2026-08-03")

dateAnal <- daten$report_date |> max() + 1-2-56
### Definition der Referenzperiode 
vgldatum1 <- ymd(str_c(year(da),"-05-01"))
vgldatum2 <- ymd(str_c(year(heute0)-1,"-05-30"))

###################################################################################################
##  Verarbeiten der Daten, holen der Temperaturdaten, Nowcast und erzeugen von Grafiken  ##########
###################################################################################################
# Aggregieren nach Sterbedatum --------------------------------------------

ncdates <- NCdates_create(data = daten, NCdatesProp = seq.Date(dateAnal - 364 -5*7,dateAnal - 364 +5*7,by = "week"),dateAnal = dateAnal)

schaetzwerte_woche <- NowcastProcessed(data = daten,dateAnal = dateAnal,recentRef = dateAnal-1, NCsize = 10,unit = "week",cnames = c("sm","sll","sul","o"),reference_date = "sterbedatum",
                                         report_date = "eingang",tu_lab = "sterbedatum",
                                         NCdates = ncdates)
write.csv2(schaetzwerte_tag,file = "Nowcast_tag.csv")
### Wochenanalyse

vergleichsdaten <- daten |>  
  filter(woche >= vgldatum1,woche <= vgldatum2) |> 
  group_by(woche) |> 
  summarize(n = n())

# options(mc.cores = parallel::detectCores())
# rstan_options(auto_write = TRUE)
# 

vgl <- sapply(vergleichsdaten$n, function(n) rSample(n)) |> unlist() |> as.vector()

vglul <- quantile(vgl,probs = 0.975,type = 4) |> unlist() |> as.vector()

ncdates <- NCdates_create(data = daten, NCdatesProp = seq.Date(dateAnal - 364 -5*7,dateAnal - 364 +5*7,by = "week"),
                          dateAnal = dateAnal)

# dateAnal <- as.Date("2026-08-04")

recentRef <- dateAnal - 1

schaetzwerte_woche <- NowcastFull(df = daten,dateAnal = dateAnal, recentRef = dateAnal-1, 
                                       NCsize = 10,unit = "week",
                                       NCdates = ncdates)

nobs <- daten |> 
  filter(report_date<dateAnal,reference_date<=(dateAnal-1)) |> 
  group_by(woche) |> summarise(n = n()) |> slice_tail(n = 10) |> pull(n)

