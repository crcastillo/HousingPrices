
source("Set_WD.R")

## Load train data and handling categorical NAs and blanks
source("Training_Dataset_Prep.R")

## Split train data into 75/25 train vs validation 
source("Training_Dataset_Split.R")

## Numerical Imputation
source("Training_Split_Numerical_Imputation.R")

## Run Linear Model Training
source("Training_LinearModel.R")

## Run Random Forest Model
source("Training_RandomForest.R")
