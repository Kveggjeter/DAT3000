# Author: Eirik
# This is the main runable script. The use case i predominantly to be used
# to clean datasets on the fly rather than an application, therefore this script
# does not function as a CLI/terminal tool or with UX like RShiny.
# It would probably be easier to use with some sort of jupyter notebook, but
# the usage is ment to be the same. Run the block you need for the subset you
# want.

# Remember to always check your working directory with getwd(). Jump over
# "setwd()" if you are in the right directory

getwd()
setwd("DAT3000/Eksamen")
{
library(dplyr)
library(haven)
source("src/AddLabelsToDf.R")
source("src/GroupByVariable.R")
source("src/AddCategoricalLabels.R")
source("src/NhanesCleaning.R")
source("src/RenameEthnicity.R")
  
 makeGroupedSubset <- function(table_attribute, nameOfNewCsv) {
   df <- as.data.frame(read.csv("pcos_dataset.csv"))
   df <- prepareMainDf(df)
   df <- groupByVariable(df, table_attribute)
   write.csv(df, paste0("processed/", nameOfNewCsv, ".csv"), row.names = FALSE)
 }
 
 predictionHandling <- function() {
   df <- read.csv("new_conclusion.csv")
   df <- addCategoricalLabelsPrediction(df)
   df$prediction.PCOS_Diagnosis. <- 
     ifelse(df$prediction.PCOS_Diagnosis. == "Yes", 1, 0)
   return(df)
 }
 
 makeGroupedPrediction <- function(table_attribute, nameOfNewCsv) {
   df <- predictionHandling()
   df <- groupByVariablePrediction(df, "Ethnicity")
   write.csv(df, paste0("processed/", nameOfNewCsv, ".csv"), row.names = FALSE)
 }
  
}

########## Preparing main dataset without grouping ##########

{
  df <- as.data.frame(read.csv("pcos_dataset.csv"))
  df <-  addLabelsToDf(df)
  write.csv(df, paste0("processed/pcos_cleaned.csv"), row.names = FALSE)
}

########## Grouping by BMI ##########

{
  makeGroupedSubset("BMI_Over_Normal", "pcos_grouped_by_BMI")
}


########## Grouping by Testosterone ########## 

{
  df <- as.data.frame(read.csv("pcos_dataset.csv"))
  makeGroupedSubset(df, "Testosterone_Over_Normal",
                    "pcos_grouped_by_Testosterone")
}

##########  Grouping by Menstrual Irregularity ########## 

{
  makeGroupedSubset("Menstrual_Irregularity",
                    "pcos_grouped_by_Menstrual_Irregularity")
}

##########  Grouping by Follicle count ########## 

{
  makeGroupedSubset("Follicle_Less_Than_Average",
                    "pcos_grouped_by_Follicle_Count")
}


########## Adding categories for Tableau ##########

{
  df <- as.data.frame(read.csv("pcos_dataset.csv"))
  df <- addCategoricalLabels(df)
  write.csv(df, paste0("processed/pcos_with_categories.csv"), row.names = FALSE)
}

########## Cleaning nhanes dataset ########## 

{
  nhanesCleaning()
}

########## Preparing the prediction-set for visualization ########## 

{
  df <- predictionHandling()
  write.csv(df, paste0("processed/prediction_not_grouped.csv"),
            row.names = FALSE)
}

########## Group prediction by Ethnicity ########## 

{
  makeGroupedPrediction("Ethnicity", "prediction_grouped_by_Ethnicity")
}

########## Group prediction by Age ########## 

{
  makeGroupedPrediction("Age", "prediction_grouped_by_Age")
}

########## Group prediction by Menstrual Irregularity ########## 

{
  makeGroupedPrediction("Menstrual_Irregularity",
                        "prediction_grouped_by_Menstrual_Irregularity")
}

########## Group prediction by BMI ########## 

{
  makeGroupedPrediction("BMI", "prediction_grouped_by_BMI")
}

########## Group prediction by Testosterone ########## 

{
  makeGroupedPrediction("estosterone_Level_Mean.ng.dL",
                        "prediction_grouped_by_Testosterone")
}

