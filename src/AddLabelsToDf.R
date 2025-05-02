# Author: Eiirik
# This function is used to add labels for analysis in Rapid Miner.
# Adds new rows and relocates them so they stand beside their respective
# "parent"

addLabelsToDf <- function(df) {
  
  df$BMI_Over_Normal <- ifelse(df$BMI > 25, 1, 0)
  df$Testosterone_Over_Normal <- ifelse(df$Testosterone_Level.ng.dL. > 70, 1, 0)
  df$Follicle_Less_Than_Average <- ifelse(df$Antral_Follicle_Count < 20, 1, 0)
  
  df <- df %>%
    relocate(BMI_Over_Normal, .after = BMI) %>%
    relocate(Testosterone_Over_Normal, .after = Testosterone_Level.ng.dL.) %>%
    relocate(Follicle_Less_Than_Average, .after = Antral_Follicle_Count)
  
}