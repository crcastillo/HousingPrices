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
Train.Data <- Train.Data[ complete.cases(Train.Data)
                          , -which(names(Data) %in% Exclude)
                          ]



#* Drop unnecessary levels
Train.Data <- droplevels(Train.Data)



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



#* Run StoreFactorLevels_Script
source("C:/Users/Chris Castillo/Data Science/Common Scripts/R_Common_Scripts/StoreFactorLevels_Script.R"
       , echo = TRUE
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



#* Create a list of the columns that are class = factor and class = integer 
allClass(Score.Import)
Class.List.Score <- which(allClass(Score.Import) == "factor")
Factor.List.Score <- match(names(Class.List), names(Score.Import))
Integer.List.Score <- which(allClass(Score.Data) == "integer")



#* Replace "" records with "_Blank_" & "NA" as "_Unknown_"
for (i in 1:length(Factor.List)){
  
  Score.Import[ , Factor.List.Score[i]] <- as.character(Score.Import[ , Factor.List.Score[i]])
  
  Score.Import[ , Factor.List.Score[i]][ Score.Import[ ,Factor.List.Score[i]] == "" ] <- "_Blank_"
  
  Score.Import[ , Factor.List.Score[i]][is.na(Score.Import[ ,Factor.List.Score[i]])] <- "_Unknown_"
  
  
  Score.Import[ , Factor.List.Score[i]] <- as.factor(Score.Import[ , Factor.List.Score[i]])
  
}



#* Drop and unnecessary levels
Score.Import <- droplevels(Score.Import)



#* Identify any factors that still have "" levels
for (i in 1:length(Factor.List.Score)) {
  
  z <- levels(Score.Data[ , Factor.List.Score[i]])
  
  if (length(z[z == ""]) != 0){
    
    print(names(Class.List)[i])
    
  }
  
}



#**********************************
#**********************************
##### Level Vector Comparison #####
#**********************************
#**********************************


#* Ensure that Score.Data removes variables that are in the Exclude list
Score.Data = Score.Import[ , -which(names(Score.Import) %in% Exclude)]



#* Remove any superfluous factor levels
Score.Data <- droplevels(Score.Data)



#* Create a list of the columns that are class = factor
allClass(Score.Data)
Class.List.Score <- which(allClass(Score.Data) == "factor")



#* Create name vectors for each factor that stores the different levels in Score.Data
for (i in 1:length(Class.List.Score)){
  
  assign(paste("Levels.Score.", names(Class.List.Score)[i], sep = ""), levels(eval(parse(text = paste("Score.Data$", names(Class.List.Score)[i], sep = "")))))
  
} #* Close loop



#* Check to see if the factor names are present in Train.Data and Score.Data
identical(names(Class.List.Train), names(Class.List.Score))



#* Loop through level comparisons and paste differences, store factors that need to have missing levels coerced
Missing_Level_Factors <- NULL
for (i in 1:length(Class.List.Score)){
  
  Missing <- get(ls()[grep('^Levels.Score.*?'
                           , ls())][i])[ !get(ls()[grep('^Levels.Score.*?'
                                                        , ls())][i]) %in% get(ls()[grep('^Levels.Train.*?'
                                                                                        , ls())][i]
                                                        )
                                         ]
  
  if (length(Missing) != 0){
    
    for (j in 1:length(Missing)){
      
      print(paste(Missing[j]
                  , " is a level missing from "
                  , ls()[grep('^Levels.Train.*?'
                              , ls())][i]
                  , sep = ""
      ))
      Missing_Level_Factors <- c(Missing_Level_Factors
                                 , gsub("Levels.Train."
                                        , ""
                                        , ls()[grep('^Levels.Train.*?'
                                                    , ls())][i]
                                 ))
      
    } #* Close print/paste LOOP
  } #* Close Missing != 0 check IF
} #* Close final LOOP that iterates through the number of stored factor levels
Missing_Level_Factors <- unique(Missing_Level_Factors)



#* Print the factors that need to have missing levels imputed
print("The following categorical variables are missing a level compared to Train.Data")
print(Missing_Level_Factors)



#*****************************************
#*****************************************
##### Level cleansing for Score Data #####
#*****************************************
#*****************************************


#* Append on any factor levels in the Train.Data that is not in the Score.Data
for (i in 1:length(Class.List.Score)){
  
  Train.Data.Level <- eval(parse(text = paste("Train.Data$", names(Class.List.Score)[i], sep = "")))
  Score.Data.Level <- eval(parse(text = paste("Score.Data$", names(Class.List.Score)[i], sep = "")))
  
  levels(Score.Data[ ,match(names(Class.List.Score)[i], names(Score.Data))]) <- c(levels(Score.Data.Level), levels(Train.Data.Level)[!levels(Train.Data.Level) %in% levels(Score.Data.Level)])
  #print(paste("Train.Data$", names(Class.List.Score)[i]," vs. ", "Score.Data$", names(Class.List.Score)[i], sep = ""))
  
} #* Close loop



#* Find the object names of any other categorical factors that need to have missing levels coerced
Missing_Level_Factors <- Missing_Level_Factors[ !Missing_Level_Factors %in% unique(gsub("Levels.Train."
                                                                                        , ""
                                                                                        , c(Other_Match
                                                                                            #, Unknown_Match # Removed the need for coercing to _Unknown_
                                                                                        )
))
]


#* Coerce any Score.Data factor level that doesn't exist in Train.Data to "_Other_"
if(length(Other_Match) > 0){
  for (i in 1:length(Other_Match)){
    
    StoreVar <- match(substr(Other_Match[i], nchar("Levels.Train.") + 1, nchar(Other_Match)[i]), names(Score.Data))
    Score.Data[ , StoreVar][!(Score.Data[ , StoreVar]) %in% get(paste(Other_Match[i], sep = ""))] <- "_Other_"
    
  } #* Close loop
} #* Close loop



#* Coerce any Score.Data factor level that doesn't exist in Train.Data to "_Unknown_"
#if(length(Unknown_Match) > 0){
#for (i in 1:length(Unknown_Match)){

#StoreVar <- match(substr(Unknown_Match[i], nchar("Levels.Train.") + 1, nchar(Unknown_Match)[i]), names(Score.Data))
#Score.Data[ , StoreVar][!(Score.Data[ , StoreVar]) %in% get(paste(Unknown_Match[i], sep = ""))] <- "_Unknown_"

#} #* Close loop
#}



#* Coerce other missing factor levels with "Smart Level Imputation"
if(length(Missing_Level_Factors) > 0){
  for(i in 1:length(Missing_Level_Factors)){
    
    StoreVar <- Missing_Level_Factors[i]
    
    Missing_Levels <- get(paste("Levels.Score.", StoreVar, sep = ""))[!get(paste("Levels.Score.", StoreVar, sep = "")) %in% get(paste("Levels.Train.", StoreVar, sep = ""))]      
    
    Missing_Count <- length(eval(parse(text = paste("Score.Data$", StoreVar, sep = "")))[ eval(parse(text = paste("Score.Data$", StoreVar, sep = ""))) %in% Missing_Levels ])
    if(Missing_Count > 0){
      print(paste(StoreVar, " IS being imputed", sep = ""))
    } #else {
    #print(paste(StoreVar, " is NOT missing a level and will NOT be imputed", sep = ""))
    #}
    
    #* Set seed to ensure reproducibility for imputation
    set.seed(10)
    Score.Data[ , StoreVar][ Score.Data[ , StoreVar] %in% Missing_Levels] <- sample(eval(parse(text = paste("Train.Data$", StoreVar, sep = ""))), Missing_Count, replace = TRUE)
    
  }
} ## Close loop



#* Clear out any NULL factor levels
Score.Data <- droplevels(Score.Data)



#* Re-order Score.Data factor levels to align with Train.Data factor levels
for (i in 1:length(Class.List.Score)){
  
  Score.Data[ ,match(names(Class.List.Score)[i], names(Score.Data))] <- factor(Score.Data[ ,match(names(Class.List.Score)[i], names(Score.Data))], levels = get(paste("Levels.Train.", names(Class.List.Score)[i], sep = "")))
  
} #* Close loop



#******************************************
#******************************************
##### Impute Missing Numerical Values #####
#******************************************
#******************************************


#* Use the median value of integer fields within Train.Data to impute NA values within Score.Data
for (i in 1:length(Integer.List.Score)){
  
  Score.Data[ 
    , match(names(Integer.List.Score)[i]
            , names(Score.Data)
            )
    ][
      is.na(
        Score.Data[ 
          , match(names(Integer.List.Score)[i]
                  , names(Score.Data)
                  )
          ]
        )
      ] <- median(
        x = Train.Data[ 
          , match(names(Integer.List.Score)[i]
                  , names(Train.Data)
                  )
          ]
        , na.rm = TRUE
        )
  
} #* Close loop



#********************************************
#********************************************
##### Create predictions for Score.Data #####
#********************************************
#********************************************


#* Ensure Score.Data hasn't lost any records from the data cleansing
if(!identical(nrow(Score.Data), nrow(Score.Data[ complete.cases(Score.Data), ]))){
  
  break()
  
}



#* Remove any NA datum records
Score.Data <- Score.Data[ complete.cases(Score.Data), ]



#* Memory cleanup
gc()



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



