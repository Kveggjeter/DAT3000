# Author: Eirik
# Function for grouping each variable. Can be used for all variables

groupByVariable <- function(df, table_attribute) {
  
  df <- df %>%
    group_by(!!sym(table_attribute)) %>%
      summarise(
        Count = n(),
        Age_mean = mean(Age, na.rm = TRUE),
        BMI_Mean = mean(BMI, na.rm = TRUE),
        Menstrual_Irregularity = mean(Menstrual_Irregularity, na.rm = TRUE),
        Testosterone_Level_Mean.ng.dL = mean(Testosterone_Level.ng.dL.,
                                           na.rm = TRUE),
        PCOS_Diagnosis_Mean = mean(PCOS_Diagnosis, na.rm = TRUE)
      )
  
}

groupByVariablePrediction <- function(df, table_attribute) {
  
  df <- df %>%
    group_by(!!sym(table_attribute)) %>%
      summarise(
        Count = n(),
        BMI_Mean = mean(BMI, na.rm = TRUE),
        BMI_Standard_Deviation = mean(BMI, na.rm = TRUE),
        Testosterone_Level_Mean.ng.dL = mean(Testosterone_Level.ng.dL.,
                                           na.rm = TRUE),
        Age_mean = mean(Age, na.rm = TRUE),
        Number_Of_Children_Under_18_Mean = mean(Number_Of_Children_Under_18,
                                              na.rm = TRUE),
        Menstrual_Irregularity = mean(Menstrual_Irregularity, na.rm = TRUE),
        Prediction.PCOS_Diagnosis_Mean = mean(prediction.PCOS_Diagnosis.,
                                            na.rm = TRUE)
    )
  
}