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

print("Benötigte Bibliotheken geladen")

#Link zu JSON-Daten
link_json <- "https://app-prod-static-voteinfo.s3.eu-central-1.amazonaws.com/v1/ogd/sd-t-17-02-20200209-eidgAbstimmung.json"

###Vorhandene Daten laden

#Metadaten Gemeinden und Kantone
meta_gmd_kt <- read_csv("Data/MASTERFILE_GDE.csv")

print("Metadaten zu Gemeinden und Kantonen geladen")



