####################### Script Comments #######################
#**************************************************************
#*
#*
#*  Housing Prices - Kaggle Competition
#*
#*  Objective: Utilize the Housing Prices dataset for Kaggle
#*    competition. 
#*
#*  Initial Build - 4/7/2018 - Chris Castillo
#*
#*  Change Log
#*    - 
#*
#*  Notes:
#*  - The Training data doesn't have a singular complete 
#*  record
#*  - Replace NA in categorical factors with "_Unknown_"
#*
#**************************************************************
#**************************************************************



#* Clear workspace and load libraries
rm(list=ls())
gc()


library(glmnet)
library(randomForest)
library(data.table)
library(caret)
library(ggplot2)
library(plyr)
library(dplyr)



#* Import Train dataset from local folder
Data <- fread(
  input = "C:/Users/Chris Castillo/Data Science/Projects/House Prices - Kaggle Competition/HousingPrices/Data/20180407_Train Data.csv"
  , stringsAsFactors = TRUE
  , strip.white = TRUE
  , data.table = FALSE
)



#* Reclassify certain data elements
Data$MSSubClass <- as.factor(Data$MSSubClass)
Data$MoSold <- as.factor(Data$MoSold)



#* Find the list of data frame names and their respective classes 
allClass <- function(x)
{unlist(lapply(unclass(x), class))}



#* Create a list of the columns that are class = integer 
allClass(Data)
Class.List <- which(allClass(Data) == "factor")
Integer.List <- which(allClass(Data) == "integer")
Factor.List <- match(names(Class.List), names(Data))



#* Replace "" records with "_Blank_" & "NA" as "_Unknown_"
for (i in 1:length(Factor.List)){
  
  Data[ , Factor.List[i]] <- as.character(Data[ , Factor.List[i]])
  
  
  Data[ , Factor.List[i]][ Data[ ,Factor.List[i]] == "" ] <- "_Blank_"
  
  Data[ , Factor.List[i]][is.na(Data[ ,Factor.List[i]])] <- "_Unknown_"
  
  
  Data[ , Factor.List[i]] <- as.factor(Data[ , Factor.List[i]])
  
}



#* Drop and unnecessary levels
Data <- droplevels(Data)



#* Identify any factors that still have "" levels
for (i in 1:length(Factor.List)) {
  
  z <- levels(Data[ , Factor.List[i]])
  
  if (length(z[z == ""]) != 0){
    
    print(
      paste(
        names(Class.List)[i]
        , "contains Blanks"
        , sep = " "
        )
    )
    
  }
  
  y <- length(Data[ , Factor.List[i]][is.na(Data[ ,Factor.List[i]])])
  
  if (y != 0){
    
    print(
      paste(
        names(Class.List)[i]
        , "contains NAs"
        , sep = " "
      )
    )
    
  }
  
  #* Clear out temporary objects
  rm(z, y)

} ## Close main loop



#* Check for Zero Variance and Near Zero Variance factors
Data.ZeroVarCheck <- nearZeroVar(Data, saveMetrics = TRUE)
Data.ZeroVarCheck[ Data.ZeroVarCheck[, "zeroVar"] + Data.ZeroVarCheck[ , "nzv"] > 0, ]



#* Create a data.frame to populate with Data factors and the level counts
Data_Levels_Table <- data.frame(
  matrix(
    ncol = 2
    , nrow = length(Class.List)
  )
)
colnames(Data_Levels_Table) <- c(
  "Factor"
  , "Level_Count"
)



#* Populate table with factors and level counts
for (i in 1:length(Class.List)){
  
  Data_Levels_Table[ i, 1 ] <- names(Class.List)[i]
  Data_Levels_Table[ i , 2 ] <- length(
    levels(
      eval(
        parse(
          text = paste(
            "Data$"
            , names(Class.List)[i]
            , sep = ""
          )
        )
      )
    )
  )
  
}



#*****************************************
#*****************************************
##### Separate Test & Train Datasets #####
#*****************************************
#*****************************************


#* Set random seed
set.seed(10)
#* Create a training dataset using 75% of Data
Train.Data <- Data[ sample(1:nrow(Data)
                           , size = round(
                             x = 0.75 * nrow(Data)
                             , digits = 0
                             )
                           , replace = FALSE
                           )
                    , ]



#* Use the median value of integer fields to impute any NA records
for (i in 1:length(Integer.List)){
  
  Train.Data[ 
    , match(names(Integer.List)[i]
            , names(Train.Data)
            )
    ][
      is.na(
        Train.Data[ 
          , match(names(Integer.List)[i]
                  , names(Train.Data)
                  )
          ]
        )
      ] <- median(
        x = Train.Data[ 
          , match(names(Integer.List)[i]
                  , names(Train.Data)
                  )
          ]
        , na.rm = TRUE
        )
  
} #* Close loop



#* Check for Zero Variance and Near Zero Variance factors
Train.Data.ZeroVarCheck <- nearZeroVar(Data, saveMetrics = TRUE)
Train.Data.ZeroVarCheck[ Train.Data.ZeroVarCheck[, "zeroVar"] + Train.Data.ZeroVarCheck[ , "nzv"] > 0, ]



#* Create an Exclusion vector of variables that I don't want to include in the model
Exclude <- c(
  #"Id"
  #, "Utilities"
)



#* Ensure no NA rows
if(exists("Exclude") & length(Exclude) != 0){
  
  Train.Data <- Train.Data[ complete.cases(Train.Data)
                            , -which(names(Data) %in% Exclude)
                          ]

} else {
  
    Train.Data <- Train.Data[ complete.cases(Train.Data), ]

}



#* Drop unnecessary levels
Train.Data <- droplevels(Train.Data)



#* Run StoreFactorLevels_Script
source("C:/Users/Chris Castillo/Data Science/Common Scripts/R_Common_Scripts/StoreFactorLevels_Script.R"
       , echo = TRUE
)



#********************************************************
#********************************************************
##### Create Simple Linear Regression on Train Data #####
#********************************************************
#********************************************************


#* Create a simple linear regression with all the available variables
Train.lm <- lm(Train.Data$SalePrice ~ .
               
               - Id
               
               , data = Train.Data
               , na.action = na.omit
               )



#**************************************
#**************************************
##### Create Test Dataset & Score #####
#**************************************
#**************************************


#* Create the Test dataset using the remainder of Data not in Train.Data
Test.Data <- Data[ !(Data$Id %in% Train.Data$Id)
  , -which(names(Data) %in% Exclude)
  ]


#* Ensure no NA rows
if(exists("Exclude") & length(Exclude) != 0){
  
  Test.Data <- Data[ !(Data$Id %in% Train.Data$Id)
                     , -which(names(Data) %in% Exclude)
                     ]
  
} else {
  
  Test.Data <- Data[ !(Data$Id %in% Train.Data$Id), ]
  
}



#* Create Score.Import from Test.Data to be compatible for ScoreImportImputation_Script
Score.Import <- Test.Data



source("C:/Users/Chris Castillo/Data Science/Common Scripts/R_Common_Scripts/ScoreImportImputation_Script.R"
       , echo = TRUE)





#*************************************
#*************************************
##### Import and Score Test File #####
#*************************************
#*************************************


#* Import Test dataset from local folder
Score.Import <- fread(
  input = "C:/Users/Chris Castillo/Data Science/Projects/House Prices - Kaggle Competition/Data/20180407_Test Data.csv"
  , stringsAsFactors = TRUE
  , strip.white = TRUE
  , data.table = FALSE
)



#* Reclassify certain data elements
Score.Import$MSSubClass <- as.factor(Score.Import$MSSubClass)
Score.Import$MoSold <- as.factor(Score.Import$MoSold)




#********************************************
#********************************************
##### Create predictions for Score.Data #####
#********************************************
#********************************************


#* Create prediction set
Score.Pred <- predict(object = Train.lm
                      , newdata = Score.Data
                      )



#* Export results of Score.Pred
Score.Export <- data.frame(
  Id = as.numeric(Score.Import$Id)
  , SalePrice = as.numeric(Score.Pred)
)



#* Export prediction set
write.csv(
  Score.Export
  , file = "C:/Users/Chris Castillo/Data Science/Projects/House Prices - Kaggle Competition/Data/20180407_Test LM Export.csv"
  , row.names = FALSE
)



