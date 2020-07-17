library(rjson)
library(jsonlite)
library(jsonlite)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
setwd("C:/Users/simon/OneDrive/LENA_Project/lena")

#json_data <- fromJSON("sd-t-17-02-20140518-eidgAbstimmung.json", flatten = TRUE)
json_data <- fromJSON("https://app-prod-static-voteinfo.s3.eu-central-1.amazonaws.com/v1/ogd/sd-t-17-02-20200209-eidgAbstimmung.json", flatten = TRUE)

get_results <- function(dta_raw,
                        object_nr = 1,
                        level = "communal",
                        format = F,
                        clean_comm = T,
                        add_ct_nr = T) {
  
  if(level == "national") {
    out <- dta_raw$schweiz$vorlagen[object_nr,] %>%
      select(-kantone)
    if(format) {
      names(out) <- gsub("resultat.", "", names(out))
      out <- format_data_ch(out)
    }
  }
  
  if(level == "cantonal") {
    out <- dta_raw$schweiz$vorlagen$kantone[[object_nr]] %>%
      mutate(Kantons_Nr = as.integer(geoLevelnummer)) %>%
      select(-gemeinden, -bezirke, -geoLevelnummer)
    if(format) {
      names(out) <- gsub("resultat.", "", names(out))
      out <- format_data_k(out)
    }
  }
  
  if(level == "district") {
    out <- map_df(dta_raw$schweiz$vorlagen$kantone[[object_nr]]$bezirke, function(x) x) %>%
      mutate(Bezirk_Nr = as.integer(geoLevelnummer)) %>%
      select(-geoLevelnummer)
  }
  
  if(level == "communal") {
    out <- map_df(dta_raw$schweiz$vorlagen$kantone[[object_nr]]$gemeinden, function(x) x) %>%
      mutate(Gemeinde_Nr = as.integer(geoLevelnummer)) %>%
      select(-geoLevelnummer)
    if(format) {
      names(out) <- gsub("resultat.", "", names(out))
      if(clean_comm) {
        out <- treat_gemeinden(out)
        out <- out %>% arrange(Gemeinde_Nr)
      }
      out <- format_data_g(out)
    }
    if(add_ct_nr) {
      out <- out %>% left_join(read_csv("Data/MASTERFILE_GDE.csv"), by = c("Gemeinde_Nr"="Gemeinde_Nr"))
    }
  }
  names(out) <- gsub("resultat.", "", names(out))
  names(out) <- gsub("staende.", "", names(out))
  # if(format && level != "communal") {
  #   print("been here")
  #   out <- format_data(out)
  # }
  return(out)
}


# Gemeindedaten säubern: Auslandschweizer aussortieren, Zürich/Winti zusammenfassen
treat_gemeinden <- function(res_comm) {
  # Auslandschweizer-"Gemeinden" aussortieren
  out <- res_comm %>% filter(Gemeinde_Nr < 9000 & Gemeinde_Nr <= 10000)
  # Zürich zusammenfassen
  zurich <- res_comm %>% 
    filter(Gemeinde_Nr == 10261 |
             Gemeinde_Nr == 20261 |
             Gemeinde_Nr == 30261 |
             Gemeinde_Nr == 40261 |
             Gemeinde_Nr == 50261 |
             Gemeinde_Nr == 60261 |
             Gemeinde_Nr == 70261 |
             Gemeinde_Nr == 80261 |
             Gemeinde_Nr == 90261) %>%
    group_by(geoLevelParentnummer) %>%
    summarise(geoLevelname = "Z?rich",
              gebietAusgezaehlt = ifelse(sum(gebietAusgezaehlt) == 9, T, F),
              jaStimmenAbsolut = sum(jaStimmenAbsolut),
              neinStimmenAbsolut = sum(neinStimmenAbsolut),
              jaStimmenInProzent = 100*jaStimmenAbsolut/(neinStimmenAbsolut+jaStimmenAbsolut),
              eingelegteStimmzettel = sum(eingelegteStimmzettel),
              anzahlStimmberechtigte = sum(anzahlStimmberechtigte),
              gueltigeStimmen = sum(gueltigeStimmen),
              stimmbeteiligungInProzent = 100*eingelegteStimmzettel/anzahlStimmberechtigte,
              Gemeinde_Nr = 261) %>%
    ungroup() %>%
    mutate(geoLevelParentnummer = "112")
  # Winterthur zusammenfassen
  winti <- res_comm %>% 
    filter(Gemeinde_Nr == 10230 |
             Gemeinde_Nr == 20230 |
             Gemeinde_Nr == 30230 |
             Gemeinde_Nr == 40230 |
             Gemeinde_Nr == 50230 |
             Gemeinde_Nr == 60230 |
             Gemeinde_Nr == 70230) %>%
    group_by(geoLevelParentnummer) %>%
    summarise(geoLevelname = "Winterthur",
              gebietAusgezaehlt = ifelse(sum(gebietAusgezaehlt) == 7, T, F),
              jaStimmenAbsolut = sum(jaStimmenAbsolut),
              neinStimmenAbsolut = sum(neinStimmenAbsolut),
              jaStimmenInProzent = 100*jaStimmenAbsolut/(neinStimmenAbsolut+jaStimmenAbsolut),
              eingelegteStimmzettel = sum(eingelegteStimmzettel),
              anzahlStimmberechtigte = sum(anzahlStimmberechtigte),
              gueltigeStimmen = sum(gueltigeStimmen),
              stimmbeteiligungInProzent = 100*eingelegteStimmzettel/anzahlStimmberechtigte,
              Gemeinde_Nr = 230) %>%
    ungroup() %>%
    mutate(geoLevelParentnummer = "110")
  
  out <- bind_rows(out, zurich)
  out <- bind_rows(out, winti)
}

format_data_g <- function(results) {
  out <- results %>%
    select(Gemeinde_Nr,
           Ja_Stimmen_In_Prozent = jaStimmenInProzent,
           Ja_Stimmen_Absolut = jaStimmenAbsolut,
           Nein_Stimmen_Absolut = neinStimmenAbsolut,
           Stimmbeteiligung_In_Prozent = stimmbeteiligungInProzent,
           Eingelegte_Stimmzettel = eingelegteStimmzettel,
           Anzahl_Stimmberechtigte = anzahlStimmberechtigte,
           Gueltige_Stimmen = gueltigeStimmen,
           Gebiet_Ausgezaehlt = gebietAusgezaehlt)
  return(out)
}



results <- get_results(json_data,2)
results <- treat_gemeinden(results)
#results <- format_data_g(results)

results <- results[,c(11,4,15)]
colnames(results) <- c("ID","Ja_Anteil","Gemeinde")

for (i in 1:nrow(results)) {
 
  if (results$ID[i] < 200) {
    
    results$Ja_Anteil[i] <- NA
  
}
}

write.csv(results,"results.csv", na = "", row.names = FALSE,fileEncoding = "UTF-8")
View(results)
