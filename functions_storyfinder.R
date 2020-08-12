#Einträge anfügen oder ersetzen im Storyboard
storyboard_modifier <- function(df, sel, insert, mode = "append") {
  if(mode == "append") {
    iii <- paste(df[sel, "Storyboard"] %>% unlist(), rep(insert), sep = ";") 
    df[sel, "Storyboard"] <- iii 
    cat("appended insert ")
    cat(insert)
    cat(" to: ")
    cat(sum(sel))
    cat(" line(s)\n")
    return(df)
  }
  if(mode == "replace") {
    df[sel, "Storyboard"] <- rep(insert)
    cat("replaced ")
    cat(sum(sel))
    cat(" line(s) with insert ")
    cat(insert)
    cat("\n")
    return(df)
  }
}

normal_intro <- function(dta) {
  out <- dta %>%
    mutate(Storyboard = case_when(
      Einstimmig_Ja == TRUE ~ c("Intro_Unanimous_Ja"),
      Einstimmig_Nein == TRUE ~ c("Intro_Unanimous_Nein"),
      Ja_Stimmen_Absolut > Nein_Stimmen_Absolut ~ c("Intro_Ja"),
      Ja_Stimmen_Absolut < Nein_Stimmen_Absolut ~ c("Intro_Nein"),
      Unentschieden == TRUE ~ c("Intro_Sonderfall")
    ))

  return(out)
}

lena_classics <- function(dta) {
  
  selection <- rank(dta$Ja_Stimmen_In_Prozent)==1
  dta <- storyboard_modifier(dta, selection, "Intro_Highest_No_CH", mode = "replace")
  
  selection <- rank(dta$Ja_Stimmen_In_Prozent)==2
  dta <- storyboard_modifier(dta, selection, "Intro_2Highest_No_CH", mode = "replace")
  
  selection <- rank(dta$Ja_Stimmen_In_Prozent)==3
  dta <- storyboard_modifier(dta, selection, "Intro_3Highest_No_CH", mode = "replace")
  
  selection <- rank(dta$Nein_Stimmen_In_Prozent)==1
  dta <- storyboard_modifier(dta, selection, "Intro_Highest_Yes_CH", mode = "replace")
  
  selection <- rank(dta$Nein_Stimmen_In_Prozent)==2
  dta <- storyboard_modifier(dta, selection, "Intro_2Highest_Yes_CH", mode = "replace")
  
  selection <- rank(dta$Nein_Stimmen_In_Prozent)==3
  dta <- storyboard_modifier(dta, selection, "Intro_3Highest_Yes_CH", mode = "replace")
  
  return(dta)
  
}


hist_storyfinder <- function(dta) {
 
  selection <- is.na(dta$Hist_Ja_Stimmen_In_Prozent) == FALSE &
    dta$Ja_Stimmen_In_Prozent > 50 &
    dta$Hist_Ja_Stimmen_In_Prozent > 50 &
    dta$Ja_Stimmen_In_Prozent > dta$Hist_Ja_Stimmen_In_Prozent
  dta <- storyboard_modifier(dta, selection, "HistoricPhrase_Ja_Ja_JaAnteilGestiegen", mode = "append")
  
  selection <- is.na(dta$Hist_Ja_Stimmen_In_Prozent) == FALSE &
    dta$Ja_Stimmen_In_Prozent > 50 &
    dta$Hist_Ja_Stimmen_In_Prozent > 50 &
    dta$Ja_Stimmen_In_Prozent < dta$Hist_Ja_Stimmen_In_Prozent
  dta <- storyboard_modifier(dta, selection, "HistoricPhrase_Ja_Ja_JaAnteilGesunken", mode = "append")
  
  selection <- is.na(dta$Hist_Ja_Stimmen_In_Prozent) == FALSE &
    dta$Ja_Stimmen_In_Prozent > 50 &
    dta$Hist_Ja_Stimmen_In_Prozent > 50 &
    dta$Ja_Stimmen_In_Prozent == dta$Hist_Ja_Stimmen_In_Prozent
  dta <- storyboard_modifier(dta, selection, "HistoricPhrase_Ja_Ja_JaAnteilUnverändert", mode = "append")
  
  selection <- is.na(dta$Hist_Ja_Stimmen_In_Prozent) == FALSE &
    dta$Ja_Stimmen_In_Prozent < 50 &
    dta$Hist_Ja_Stimmen_In_Prozent < 50 &
    dta$Ja_Stimmen_In_Prozent > dta$Hist_Ja_Stimmen_In_Prozent
  dta <- storyboard_modifier(dta, selection, "HistoricPhrase_Nein_Nein_NeinAnteilGestiegen", mode = "append")
  
  selection <- is.na(dta$Hist_Ja_Stimmen_In_Prozent) == FALSE &
    dta$Ja_Stimmen_In_Prozent < 50 &
    dta$Hist_Ja_Stimmen_In_Prozent < 50 &
    dta$Ja_Stimmen_In_Prozent < dta$Hist_Ja_Stimmen_In_Prozent
  dta <- storyboard_modifier(dta, selection, "HistoricPhrase_Nein_Nein_NeinAnteilGesunken", mode = "append")
  
  selection <- is.na(dta$Hist_Ja_Stimmen_In_Prozent) == FALSE &
    dta$Ja_Stimmen_In_Prozent < 50 &
    dta$Hist_Ja_Stimmen_In_Prozent < 50 &
    dta$Ja_Stimmen_In_Prozent == dta$Hist_Ja_Stimmen_In_Prozent
  dta <- storyboard_modifier(dta, selection, "HistoricPhrase_Nein_Nein_NeinAnteilUnverändert", mode = "append")
  
  selection <- is.na(dta$Hist_Ja_Stimmen_In_Prozent) == FALSE &
    dta$Ja_Stimmen_In_Prozent > 50 &
    dta$Hist_Ja_Stimmen_In_Prozent < 50
  dta <- storyboard_modifier(dta, selection, "HistoricPhrase_Nein_Ja", mode = "append")
  
  selection <- is.na(dta$Hist_Ja_Stimmen_In_Prozent) == FALSE &
    dta$Ja_Stimmen_In_Prozent < 50 &
    dta$Hist_Ja_Stimmen_In_Prozent > 50
  dta <- storyboard_modifier(dta, selection, "HistoricPhrase_Ja_Nein", mode = "append")
  
  
  return(dta)
  
  }




