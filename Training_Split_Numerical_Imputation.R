

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


