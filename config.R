#Bibliotheken laden
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(stringr)
library(stringi)
library(xml2)
library(rjson)
library(jsonlite)
library(readxl)

print("Benötigte Bibliotheken geladen")

#Link zu JSON-Daten / Daten einlesen
#link_json <- "https://app-prod-static-voteinfo.s3.eu-central-1.amazonaws.com/v1/ogd/sd-t-17-02-20200209-eidgAbstimmung.json"
link_json <- "https://app-prod-static-voteinfo.s3.eu-central-1.amazonaws.com/v1/ogd/sd-t-17-02-20200927-eidgAbstimmung.json" 
json_data <- fromJSON(link_json, flatten = TRUE)

("Aktuelle Abstimmungsdaten geladen")


#Kurznamen Vorlagen (Verwendet im File mit den Textbausteinen)
vorlagen_short <- c("Zuwanderung","Jagdgesetz","Bundessteuer","Erwerbsersatz","Kampfjet")

###Vorhandene Daten laden Gripen / Masseneinwanderungsinitiative
daten_gripen_bfs <- read_excel("Data/daten_gripen_bfs.xlsx", 
                               skip = 10)
daten_masseneinwanderung_bfs <- read_excel("Data/daten_masseneinwanderung_bfs.xlsx", 
                               skip = 10)


cat("Daten zu historischen Abstimmungen geladen")

#Metadaten Gemeinden und Kantone
meta_gmd_kt <- read_csv("Data/MASTERFILE_GDE.csv")

cat("Metadaten zu Gemeinden und Kantonen geladen")



