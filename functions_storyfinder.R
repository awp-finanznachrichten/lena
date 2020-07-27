normal_intro <- function(dta) {
  out <- dta %>%
    mutate(Storyboard = case_when(
      Einstimmig_Ja == TRUE ~ c("Intro_Unanimous_Ja"),
      Einstimmig_Nein == TRUE ~ c("Intro_Unanimous_Nein"),
      Ja_Stimmen_Absolut > Nein_Stimmen_Absolut ~ c("Intro_Ja"),
      Ja_Stimmen_Absolut < Nein_Stimmen_Absolut  ~ c("Intro_Nein"),
      Unentschieden == TRUE ~ c("Intro_Sonderfall")
    ))

  return(out)
}

further_on <- function(dta) {
  
  selection <- dta$Stimmbeteiligung_In_Prozent > 50
  dta <- storyboard_modifier(dta, selection, "Stimmbeteiligung_high", mode = "append")
  
  
  return(dta)
}
