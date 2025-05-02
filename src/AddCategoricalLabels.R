# Author: Eirik
# This script is for adding attributes used for visualization. Not to be used
# in analysis, but only for visualization purposes.

addCategoricalLabels <- function(df) {
  df$Age_Category <- ifelse(df$Age < 25, "Young", "Adult")
  df$Age_Category[df$Age > 40] <- "Mature"
  df$BMI_Category <- ifelse(df$BMI < 18.5, "Underweight", "Normal")
  df$BMI_Category[df$BMI > 24.9] <- "Overweight"
  df$BMI_Category[df$BMI > 29.9] <- "Obese"
  df$BMI_Category[df$BMI > 39.9] <- "Extremely Obese"
  df$Testosterone_Category <- ifelse(df$Testosterone_Level.ng.dL. > 70,
                                     "High", "Normal")
  df$Follicle_Category <- ifelse(df$Antral_Follicle_Count < 8, "Low", "Normal")
  df$Follicle_Category[df$Antral_Follicle_Count > 15] <- "High"
  
  df <- df %>%
    relocate(Age_Category, .after = Age) %>%
    relocate(BMI_Category, .after = BMI) %>%
    relocate(Testosterone_Category, .after = Testosterone_Level.ng.dL.) %>%
    relocate(Follicle_Category, .after = Antral_Follicle_Count) 
} 

addCategoricalLabelsPrediction <- function(df) {
  source("src/RenameEthnicity.R")
  
  df$Age_Category <- ifelse(df$Age < 25, "Young", "Adult")
  df$Age_Category[df$Age > 40] <- "Mature"
  df$BMI_Category <- ifelse(df$BMI < 18.5, "Underweight", "Normal")
  df$BMI_Category[df$BMI > 24.9] <- "Overweight"
  df$BMI_Category[df$BMI > 29.9] <- "Obese"
  df$BMI_Category[df$BMI > 39.9] <- "Extremely Obese"
  df$Testosterone_Category <- ifelse(df$Testosterone_Level.ng.dL. > 70, "High", "Normal")
  
  df <- renameEthnicity(df)
  

  return(df)
} 