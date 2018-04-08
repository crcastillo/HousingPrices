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

