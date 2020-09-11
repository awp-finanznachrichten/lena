time_start <- Sys.time()
print(time_start)

#Working Directory definieren
setwd("C:/Users/simon/OneDrive/LENA_Project/lena")

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

#Daten simulieren Gemeinde!!!
for (a in 1:1000) { #nrow(results)
  
results$gebietAusgezaehlt[a] = TRUE

results$jaStimmenAbsolut[a] <- sample(0:10000,1)
results$neinStimmenAbsolut[a] <- sample(0:10000,1)
results$gueltigeStimmen[a] <- results$jaStimmenAbsolut[a] + results$neinStimmenAbsolut[a]
results$jaStimmenInProzent[a] <- results$jaStimmenAbsolut[a]*100/results$gueltigeStimmen[a]

}

#Daten anpassen Gemeinden
results <- treat_gemeinden(results)
results <- format_data_g(results)

#Kantonsdaten hinzufügen
results_kantone <- get_results(json_data,i,"cantonal")

#Daten simulieren Kantone!!!
for (b in 1:nrow(results_kantone)) {
  
  results_kantone$gebietAusgezaehlt[b] <- TRUE
  results_kantone$jaStimmenInProzent[b] <- runif(1,0,100)
  
}

Ja_Stimmen_Kanton <- results_kantone %>%
  select(Kantons_Nr,jaStimmenInProzent) %>%
  rename(Ja_Stimmen_In_Prozent_Kanton = jaStimmenInProzent) %>%
  mutate(Highest_Yes_Kant = FALSE,
         Highest_No_Kant = FALSE)

results <- merge(results,Ja_Stimmen_Kanton)


#Kanton Jura entfernen
#for (c in 1:nrow(results)) {
  
#  if (results$Kantons_Nr[c] == 26) {

#results$Gebiet_Ausgezaehlt[c] <- FALSE

#}

#}


#Wie viele Gemeinden sind ausgezählt?
cat(paste0(sum(results$Gebiet_Ausgezaehlt)," Gemeinden sind ausgezählt.\n"))

#Neue Variablen
results$Ja_Nein <- NA
results$Oui_Non <- NA
results$Nein_Stimmen_In_Prozent <- NA
results$Unentschieden <- NA
results$Einstimmig_Ja <- NA
results$Einstimmig_Nein <- NA
results$Storyboard <- NA
results$Text_d <- "Die Resultate von dieser Gemeinde sind noch nicht bekannt."
results$Text_f <- "Les résultats ne sont pas encore connus dans cette commune."

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

if (vorlagen$id[i] == "6310") {

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
cat("Textvorlagen geladen\n\n")

#Texte einfügen
results <- build_texts(results)

#Variablen ersetzen 
results <- replace_variables(results)

###Texte anpassen und optimieren
results <- excuse_my_french(results)

#Print out texts
cat(paste0(results$Gemeinde_d,"\n",results$Text_d,"\n\n",results$Text_f,collapse="\n\n"))

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

#library(xlsx)
#write.xlsx(results,paste0("LENA_Texte_",vorlagen_short[i],".xlsx"),row.names=FALSE)

}

time_end <- Sys.time()
print(time_end)
print(time_end-time_start)

View(results)

