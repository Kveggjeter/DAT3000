getwd()
setwd("DAT3000/Eksamen")
# Cleaning the original dataset
# JEG LOVER Å KOMMENTERE OG DELE DETTE OPP SENERE, MEN JEG GIDDER IKKE NÅÅÅÅÅÅÅÅÅÅÅÅÅÅÅÅÅÅÅÅÅÅÅÅ
# FAKEDATASETTTTTT
{
  library(dplyr)

    df <- as.data.frame(read.csv("pcos_dataset.csv"))
    
    df$BMI_Over_Normal <- ifelse(df$BMI > 25, 1, 0)
    df$Testosterone_Over_Normal <- ifelse(df$Testosterone_Level.ng.dL. > 70, 1, 0)
    df$Follicle_Less_Than_Average <- ifelse(df$Antral_Follicle_Count < 20, 1, 0)
    
    df <- df %>%
      relocate(BMI_Over_Normal, .after = BMI) %>%
      relocate(Testosterone_Over_Normal, .after = Testosterone_Level.ng.dL.) %>%
      relocate(Follicle_Less_Than_Average, .after = Antral_Follicle_Count)
    
    df <- df %>%
      group_by(BMI_Over_Normal) %>%
      summarise(
        BMI_Over_Normal_count = n(),
        Age_mean = mean(Age, na.rm = TRUE),
        Menstrual_Irregularity = mean(Menstrual_Irregularity, na.rm = TRUE),
        Testosterone_Level_Mean.ng.dL = mean(Testosterone_Level.ng.dL., na.rm = TRUE),
        PCOS_Diagnosis_Mean = mean(PCOS_Diagnosis, na.rm = TRUE)
      )
    
    View(df)
}  
    write.csv(df, paste0("pcos_cleaned.csv"), row.names = FALSE)

# Labels    
{
  df <- as.data.frame(read.csv("pcos_dataset.csv"))
  df$Age_Category <- ifelse(df$Age < 25, "Young", "Adult")
  df$Age_Category[df$Age > 40] <- "Mature"
  df$BMI_Category <- ifelse(df$BMI < 18.5, "Underweight", "Normal")
  df$BMI_Category[df$BMI > 24.9] <- "Overweight"
  df$BMI_Category[df$BMI > 29.9] <- "Obese"
  df$BMI_Category[df$BMI > 39.9] <- "Extremely Obese"
  df$Testosterone_Category <- ifelse(df$Testosterone_Level.ng.dL. > 70, "High", "Normal")
  df$Follicle_Category <- ifelse(df$Antral_Follicle_Count < 8, "Low", "Normal")
  df$Follicle_Category[df$Antral_Follicle_Count > 15] <- "High"
  
  
  df <- df %>%
    relocate(Age_Category, .after = Age) %>%
    relocate(BMI_Category, .after = BMI) %>%
    relocate(Testosterone_Category, .after = Testosterone_Level.ng.dL.) %>%
    relocate(Follicle_Category, .after = Antral_Follicle_Count) 
  
  View(df)
    }
    
    write.csv(df, paste0("pcos_cleaned_with_labels.csv"), row.names = FALSE)
    

# cleaning and and creating our own subset for predictions using nhanes 2015/2016 (latest iteration that had testosterone in their exams)   
{
  library(haven)

    bmi <- read_xpt("raw/BMX_I.xpt")
    demography <- read_xpt("raw/DEMO_I.xpt")
    hormones <- read_xpt("raw/TST_I.xpt")
    menstrual <- read_xpt("raw/RHQ_I.xpt")
    
    newData <- demography %>%
      inner_join(hormones, by = "SEQN") %>%
      inner_join(bmi, by = "SEQN") %>%
      inner_join(menstrual, by = "SEQN")
    
    newData <- replace(newData, is.na(newData), 0)
    newData <- subset(newData, RIAGENDR == 2)
    newData <- subset(newData, RIDAGEYR <= 46)
    newData <- subset(newData, RIDAGEYR >= 18)
    
    newData$BMI_Over_Normal <- ifelse(newData$BMXBMI > 25, 1, 0)
    newData$Testosterone_Over_Normal <- ifelse(newData$LBXTST > 70, 1, 0)
    
    newData <- newData %>%
      relocate(BMI_Over_Normal, .after = BMXBMI) %>%
      relocate(Testosterone_Over_Normal, .after = LBXTST)
}
    
    write.csv(newData, paste0("nhanes_without_selection.csv"), row.names = FALSE)
    
{
    newData$Number_Of_Children_Under_18 <- rowSums(newData[, c("DMDHHSZA", "DMDHHSZB")])
    newData <- subset(newData, select=c(RIDAGEYR, RIDRETH3, Number_Of_Children_Under_18, LBXTST, Testosterone_Over_Normal, BMXBMI, BMI_Over_Normal, RHQ031))
    newData <- newData %>%
      rename(
        Age = RIDAGEYR,
        Ethnicity = RIDRETH3,
        Testosterone_Level.ng.dL. = LBXTST,
        BMI = BMXBMI,
        Menstrual_Irregularity = RHQ031
      )
    
    newData$Menstrual_Irregularity <- ifelse(newData$Menstrual_Irregularity == 1, 0, 1)
    }
    write.csv(newData, paste0("nhanes_with_selection.csv"), row.names = FALSE)
    
    
    
    
  # Count of races,
  {
    raceCount <- as.data.frame(read.csv("prediction.csv"))
    raceCount$prediction.PCOS_Diagnosis. <- ifelse(raceCount$prediction.PCOS_Diagnosis. == "Yes", 1, 0)
    write.csv(raceCount, paste0("prediction_with_number.csv"), row.names = FALSE)
    
    raceCount <- raceCount %>%
      group_by(Race) %>%
      summarise(
        Ethnicity_Count = n(),
        BMI_Mean = mean(BMI, na.rm = TRUE),
        BMI_Standard_Deviation = mean(BMI, na.rm = TRUE),
        Testosterone_Level_Mean.ng.dL = mean(Testosterone_Level.ng.dL., na.rm = TRUE),
        Age_mean = mean(Age, na.rm = TRUE),
        Number_Of_Children_Under_18_Mean = mean(Number_Of_Children_Under_18, na.rm = TRUE),
        Menstrual_Irregularity = mean(Menstrual_Irregularity, na.rm = TRUE),
        Prediction.PCOS_Diagnosis_Mean = mean(prediction.PCOS_Diagnosis., na.rm = TRUE)
      )

    raceCount <- raceCount %>%
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
    
    raceCount <- raceCount %>%
      rename(
        Ethnicity = Race
      )
  }

    write.csv(raceCount, paste0("Conclusion.csv"), row.names = FALSE)

  
  {
    labelDf <- as.data.frame(read.csv("new_conclusion.csv"))
    labelDf$Age_Category <- ifelse(labelDf$Age < 25, "Young", "Adult")
    labelDf$Age_Category[labelDf$Age > 40] <- "Mature"
    labelDf$BMI_Category <- ifelse(labelDf$BMI < 18.5, "Underweight", "Normal")
    labelDf$BMI_Category[labelDf$BMI > 24.9] <- "Overweight"
    labelDf$BMI_Category[labelDf$BMI > 29.9] <- "Obese"
    labelDf$BMI_Category[labelDf$BMI > 39.9] <- "Extremely Obese"
    labelDf$Testosterone_Category <- ifelse(labelDf$Testosterone_Level.ng.dL. > 70, "High", "Normal")
    
    labelDf <- labelDf %>%
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
    
    labelDf <- labelDf %>%
      rename(
        Ethnicity = Race
      )
    
    
    
    labelDf$prediction.PCOS_Diagnosis. <- ifelse(labelDf$prediction.PCOS_Diagnosis. == "Yes", 1, 0)
    labelDf <- labelDf %>%
      group_by(Ethnicity) %>%
      summarise(
        Ethnicity_Count = n(),
        Age_mean = mean(Age, na.rm = TRUE),
        Menstrual_Irregularity = mean(Menstrual_Irregularity, na.rm = TRUE),
        Testosterone_Level_Mean.ng.dL = mean(Testosterone_Level.ng.dL., na.rm = TRUE),
        BMI_Mean = mean(BMI, na.rm = TRUE),
        Prediction.PCOS_Diagnosis_Mean = mean(prediction.PCOS_Diagnosis., na.rm = TRUE)
      )
    
    View(labelDf)
  
    }
    write.csv(labelDf, paste0("prediction_with_bmi_grouping.csv"), row.names = FALSE)
    write.csv(labelDf, paste0("prediction_with_age_grouping.csv"), row.names = FALSE)
    write.csv(labelDf, paste0("prediction_with_testosterone_grouping.csv"), row.names = FALSE)
    
    
    
    write.csv(labelDf, paste0("prediction_without_grouping_labels.csv"), row.names = FALSE)
  
  
  
  
  
  
  
  
  mainDf <- as.data.frame(read.csv("pcos_cleaned.csv"))
  secondDf <- as.data.frame(read.csv("nhanes_with_selection.csv"))
  
  mainDf <- mainDf %>%
    select(-Menstrual_Irregularity,
           -Antral_Follicle_Count,
           -Follicle_Less_Than_Average)
  secondDf <- secondDf %>%
    select(-Ethnicity, -Number_Of_Children_Under_18)
  
  secondDf <- secondDf %>%
    relocate(BMI, .after = Age) %>%
    relocate(BMI_Over_Normal, .after = BMI)
  
  mainDf$PCOS_Diagnosis <- ifelse(mainDf$PCOS_Diagnosis == 1, "Yes", "No")
  
  
  View(mainDf)
  View(secondDf)
    }
    
    write.csv(mainDf, paste0("mainDf.csv"), row.names = FALSE)
    write.csv(secondDf, paste0("nhanes_ex.csv"), row.names = FALSE)
  
