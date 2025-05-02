# Author: Eirik
# Renaming script to give the ethnicities their correct value. The values
# "Race" is also renamed to "Ethnicity". 

renameEthnicity <- function(df) {
  
  df <- df %>%
    mutate(
      Race = recode(
        Race,
        `1` = "Mexican_American",
        `2` = "Other_Hispanic",
        `3` = "Caucasian",
        `4` = "Black",
        `6` = "Asian",
        `7` = "Other_Including_Multi_Racial"
      )
    )
  
  df <- df %>%
    rename(
      Ethnicity = Race
    )
  
  return(df)
}