#Working Directory definieren
setwd("C:/Users/sw/OneDrive/LENA_Project/lena")

###Config: Bibliotheken laden, Pfade/Links definieren, bereits vorhandene Daten laden
source("config.R",encoding = "UTF-8")

###Funktionen laden
source("functions_readin.R", encoding = "UTF-8")
source("functions_storyfinder.R", encoding = "UTF-8")
source("functions_storybuilder.R", encoding = "UTF-8")

#Anzahl, Name und Nummer der Vorlagen von JSON einlesen
vorlagen <- get_vorlagen(json_data,"de")

#####Loop für jede Vorlage
for (i in 1:nrow(vorlagen)) {

#i <- 1 #LÖSCHEN!!!!
cat(paste0("Ermittle Daten für folgende Vorlage: ",vorlagen$text[i],"\n"))
  
###Resultate aus JSON auslesen 
results <- get_results(json_data,i)

#Daten simulieren
#for (a in 3:nrow(results)) {

#results$gebietAusgezaehlt[a] = TRUE
#results$jaStimmenInProzent[a] <-   runif(1,0,100)
#results$jaStimmenAbsolut[a] <- sample(0:10000,1)
#results$neinStimmenAbsolut[a] <- sample(0:10000,1)
#results$gueltigeStimmen[a] <- sample(0:10000,1)

#}


#Daten anpassen Gemeinden
results <- treat_gemeinden(results)
results <- format_data_g(results)

#Kantonsdaten hinzufügen
results_kantone <- get_results(json_data,i,"cantonal")
Ja_Stimmen_Kanton <- results_kantone %>%
  select(Kantons_Nr,jaStimmenInProzent) %>%
  rename(Ja_Stimmen_In_Prozent_Kanton = jaStimmenInProzent) %>%
  mutate(Highest_Yes_Kant = FALSE,
         Highest_No_Kant = FALSE)

results <- merge(results,Ja_Stimmen_Kanton)



#Wie viele Gemeinden sind ausgezählt?
results$Gebiet_Ausgezaehlt[1] <- FALSE
cat(paste0(sum(results$Gebiet_Ausgezaehlt)," Gemeinden sind ausgezählt.\n"))

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

hist_check <- FALSE

#Ausgezählte Gemeinden auswählen
results_notavailable <- results[results$Gebiet_Ausgezaehlt == FALSE,]
results <- results[results$Gebiet_Ausgezaehlt == TRUE,]

#Sind schon Daten vorhanden?
if (nrow(results) > 0) {

#Daten anpassen
results <- augment_raw_data(results)

###Storyfinder 
#results$Einstimmig_Ja[1] <- TRUE
#results$Einstimmig_Nein[1] <- TRUE

#Intros generieren
results <- normal_intro(results)

#LENA-Classics (falls alle Gemeinden ausgezählt):
if (nrow(results_notavailable) == 0) {

results <- lena_classics(results)

}  

#Historischer Vergleich (falls vorhanden)

#Check Vorlagen-ID


if (vorlagen$id[i] == "6300") {

hist_check <- TRUE 
data_hist <- format_data_hist(daten_masseneinwanderung_bfs)
results <- merge(results,data_hist,all.x = TRUE)
results <- hist_storyfinder(results)

}

if (vorlagen$id[i] == "6350") {

hist_check <- TRUE
data_hist <- format_data_hist(daten_gripen_bfs)
results <- merge(results,data_hist,all.x = TRUE)
results <- hist_storyfinder(results)
  
}

#Vergleich innerhalb des Kantons (falls alle Daten vom Kanton vorhanden)

#Check Vorlagen-ID
if (vorlagen$id[i] == "6320" || vorlagen$id[i] == "6330" || vorlagen$id[i] == "6340") {
  

#Falls mindestens ein Kanton ausgezählt -> Stories für die Kantone finden
  
if (sum(results_kantone$gebietAusgezaehlt) > 0) {
  
results <- kanton_storyfinder(results)

}

}

###Storybuilder

#Textvorlagen laden
Textbausteine <- as.data.frame(read_excel("Data/Textbausteine_LENA_September2020.xlsx", 
                                               sheet = vorlagen_short[i]))
cat("Textvorlagen geladen")

#Texte einfügen
results <- build_texts(results)

#Variablen ersetzen 
results <- replace_variables(results)

###Texte anpassen und optimieren


}


###Ausgezählte und nicht ausgezählte Gemeinden wieder zusammenführen -> Immer gleiches Format für Datawrapper
if (nrow(results_notavailable) > 0) {

results_notavailable$Ja_Stimmen_In_Prozent <- 50

if (hist_check == TRUE) {
results_notavailable$Hist_Ja_Stimmen_In_Prozent <- NA
results_notavailable$Hist_Ja_Stimmen_Absolut <- NA
results_notavailable$Hist_Nein_Stimmen_In_Prozent <- NA
results_notavailable$Hist_Nein_Stimmen_Absolut <- NA
}

results <- rbind(results,results_notavailable) %>%
  arrange(Gemeinde_Nr)

}


###Output generieren für Datawrapper

output_dw <- results %>%
  select(Gemeinde_Nr,Ja_Stimmen_In_Prozent,Gemeinde_KT_d,Gemeinde_KT_f,Text_d,Text_f)


write.csv(output_dw,paste0("Output/",vorlagen_short[i],"_dw.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")

cat(paste0("\nGenerated output for Vorlage ",vorlagen_short[i],"\n"))

}
