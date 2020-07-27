###Funktion für Texterstellung basierend auf Storyboard

build_texts <- function(dta) {
    
 for (i in 1:nrow(dta)) {
   
  #Einzelne Storyteile vom Storyboard extrahieren
  story_parts <- strsplit(dta$Storyboard[i],";")[[1]]

  
  #Texte einfügen für jeden Storyteil
  
  for (part in 1:length(story_parts)) {
  
  texts <- Textbausteine[Textbausteine$Text_ID == story_parts[part],]

  #Zufällige Variante auswählen bei mehreren Möglichkeiten
  
  #Seed mit Gemeinde-Nr -> Wird immer dieselbe Variante bei Gemeinde gewählt  
  set.seed(dta$Gemeinde_Nr[i])
  
  variante <- sample(1:nrow(texts),1)

  text <- texts[variante,]
  
  #Texte einfügen bei erstem Storyteil, bei weiteren Storyteilen Test verlängern
  
  if (part == 1) {
  
  dta$Text_d[i] <- text[,"Text_d"]
  dta$Text_f[i] <- text[,"Text_f"]

    } else {
  
      dta$Text_d[i] <- paste(dta$Text_d[i],text[,"Text_d"])
      dta$Text_f[i] <- paste(dta$Text_f[i],text[,"Text_f"])
   
}

print(dta$Text_d[i])
print(dta$Text_f[i])
  
  }

 
}

return(dta)

}


###Funktion für Ersetzen von Variablen im Text
replace_variables <- function(dta) {

for (i in 1:nrow(dta)) {
  
  dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#Gemeinde_d",dta$Gemeinde_d[i])
  dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#Kanton_d",dta$Kanton_d[i])
  dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#JaStimmenInProzent",gsub("[.]",",",round(dta$Ja_Stimmen_In_Prozent[i],1)))
  dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#NeinStimmenInProzent",gsub("[.]",",",round(dta$Nein_Stimmen_In_Prozent[i],1)))
  dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#JaStimmenAbsolut",as.character(dta$Ja_Stimmen_Absolut[i]))
  dta$Text_d[i] <- str_replace_all(dta$Text_d[i],"#NeinStimmenAbsolut",as.character(dta$Nein_Stimmen_Absolut[i]))
  
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#Gemeinde_f",dta$Gemeinde_f[i]) 
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#Kanton_f",dta$Kanton_f[i])
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#JaStimmenInProzent",gsub("[.]",",",round(dta$Ja_Stimmen_In_Prozent[i],1)))
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#NeinStimmenInProzent",gsub("[.]",",",round(dta$Nein_Stimmen_In_Prozent[i],1)))
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#JaStimmenAbsolut",as.character(dta$Ja_Stimmen_Absolut[i]))
  dta$Text_f[i] <- str_replace_all(dta$Text_f[i],"#NeinStimmenAbsolut",as.character(dta$Nein_Stimmen_Absolut[i]))
  
  
}
 
return(dta)   
}

