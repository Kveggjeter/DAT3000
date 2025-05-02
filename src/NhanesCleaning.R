# Author: Eirik
# This script is for cleaning and prepping the nhanes data. We have selected
# some values that we wanted to showcase, but the nhanes has a wide variety
# of columns that can be added for further context. 

nhanesCleaning <- function() {
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
  
  newData$Number_Of_Children_Under_18 <- rowSums(newData[,
                                                         c("DMDHHSZA",
                                                           "DMDHHSZB")])
  newData <- subset(newData, select=c(RIDAGEYR,
                                      RIDRETH3,
                                      Number_Of_Children_Under_18,
                                      LBXTST,
                                      Testosterone_Over_Normal,
                                      BMXBMI,
                                      BMI_Over_Normal,
                                      RHQ031))
  newData <- newData %>%
    rename(
      Age = RIDAGEYR,
      Ethnicity = RIDRETH3,
      Testosterone_Level.ng.dL. = LBXTST,
      BMI = BMXBMI,
      Menstrual_Irregularity = RHQ031
    )
  
  newData$Menstrual_Irregularity <- ifelse(
    newData$Menstrual_Irregularity == 1, 0, 1)
  
  write.csv(newData, paste0("processed/nhanes_with_selection.csv"),
            row.names = FALSE)
}