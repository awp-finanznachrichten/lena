#Working Directory definieren
setwd("C:/Users/simon/OneDrive/LENA_Project/lena")

###Config: Bibliotheken laden, Pfade/Links definieren, bereits vorhandene Daten laden
source("config.R",encoding = "UTF-8")

###Funktionen laden
source("functions.R", encoding = "UTF-8")

###Rohdaten von JSON einlesen
json_data <- fromJSON(link_json, flatten = TRUE)

#Anzahl, Name und Nummer der Vorlagen von JSON einlesen
vorlagen <- get_vorlagen(json_data,"de")

#####Loop für jede Vorlage
for (i in 1:nrow(vorlagen)) {

print(paste0("Ermittle Daten für folgende Vorlage: ",vorlagen$text[i]))
  
###Resultate aus JSON auslesen 
results <- get_results(json_data,i)

#Daten anpassen Gemeinden
results <- treat_gemeinden(results)
results <- format_data_g(results)

#Wie viele Gemeinden sind ausgezählt?
results$Gebiet_Ausgezaehlt[1] <- FALSE
print(paste0(sum(results$Gebiet_Ausgezaehlt)," Gemeinden sind ausgezählt."))

#Neue Variablen
results$Ja_Nein <- NA
results$Oui_Non <- NA
results$Nein_Stimmen_In_Prozent <- NA
results$Unentschieden <- NA
results$Einstimmig_Ja <- NA
results$Einstimmig_Nein <- NA
results$Storyboard <- NA
results$Text <- "Noch keine Daten vorhanden"


#Ausgezählte Gemeinden auswählen
results_available <- results[results$Gebiet_Ausgezaehlt == TRUE,]
results_notavailable <- results[results$Gebiet_Ausgezaehlt == FALSE,]

#Daten anpassen für ausgezählte Gemeinden
results_available <- augment_raw_data(results_available)

###Storyfinder für ausgezählte Gemeinden
source("Storyfinder.R")
results_available$Einstimmig_Ja[1] <- TRUE
results_available$Einstimmig_Nein[1] <- TRUE

results_available <- normal_intro(results_available)


###Storybuilder für ausgezählte GEmeinden


#Daten zusammenführen
results <- rbind(results_available,results_notavailable) %>%
  arrange(Gemeinde_Nr)



###Storyfinder für LENA Endresultate


###Storybuilder für LENA Endresultate


###Texte anpassen und optimieren


###Output generieren für Datawrapper



}




