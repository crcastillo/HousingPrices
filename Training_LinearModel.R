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
source("StoreFactorLevels_Script.R"
       , echo = TRUE
)

