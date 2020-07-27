#Working Directory definieren
setwd("C:/Users/simon/OneDrive/LENA_Project/lena")

###Config: Bibliotheken laden, Pfade/Links definieren, bereits vorhandene Daten laden
source("config.R",encoding = "UTF-8")

###Funktionen laden
source("functions_readin.R", encoding = "UTF-8")
source("functions_storyfinder.R", encoding = "UTF-8")
source("functions_storybuilder.R", encoding = "UTF-8")

###Rohdaten von JSON einlesen
json_data <- fromJSON(link_json, flatten = TRUE)

#Anzahl, Name und Nummer der Vorlagen von JSON einlesen
vorlagen <- get_vorlagen(json_data,"de")

#####Loop für jede Vorlage
#for (i in 1:nrow(vorlagen)) {

i <- 1 #LÖSCHEN!!!!
cat(paste0("Ermittle Daten für folgende Vorlage: ",vorlagen$text[i]))
  
###Resultate aus JSON auslesen 
results <- get_results(json_data,i)

#Daten anpassen Gemeinden
results <- treat_gemeinden(results)
results <- format_data_g(results)

#Wie viele Gemeinden sind ausgezählt?
results$Gebiet_Ausgezaehlt[1] <- FALSE
cat(paste0(sum(results$Gebiet_Ausgezaehlt)," Gemeinden sind ausgezählt."))

#Neue Variablen
results$Ja_Nein <- NA
results$Oui_Non <- NA
results$Nein_Stimmen_In_Prozent <- NA
results$Unentschieden <- NA
results$Einstimmig_Ja <- NA
results$Einstimmig_Nein <- NA
results$Storyboard <- NA
results$Text_d <- "Noch keine Daten vorhanden"
results$Text_f <- "Aucune donnée disponible pour l'instant"

#Ausgezählte Gemeinden auswählen
results_available <- results[results$Gebiet_Ausgezaehlt == TRUE,]
results_notavailable <- results[results$Gebiet_Ausgezaehlt == FALSE,]

#Daten anpassen
results_available <- augment_raw_data(results_available)

###Storyfinder 

results_available$Einstimmig_Ja[1] <- TRUE
results_available$Einstimmig_Nein[1] <- TRUE

results_available <- normal_intro(results_available)

#Falls alle Gemeinden ausgezählt -> Weitere Storyfinder-Elemente


###Storybuilder

#Textvorlagen laden
Textbausteine <- read_excel("Data/Textbausteine_LENA_September2020.xlsx", 
                                               sheet = "Kampfjet")
cat("Textvorlagen geladen")

#Texte einfügen
results_available <- build_texts(results_available)

#Variablen ersetzen 
results_available <- replace_variables(results_available)

###Texte anpassen und optimieren

###Ausgezählte und nicht ausgezählte Gemeinden wieder zusammenführen -> Immer gleiches Format für Datawrapper
#results <- rbind(results_available,results_notavailable) %>%
#  arrange(Gemeinde_Nr)

###Output generieren für Datawrapper



#}
View(results_available)
