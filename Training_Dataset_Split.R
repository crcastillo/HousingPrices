
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
