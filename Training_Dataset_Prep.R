


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
  input = "Data/20180407_Train Data.csv"
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

