#**************************************
#**************************************
##### Create Test Dataset & Score #####
#**************************************
#**************************************


#* Create the Test dataset using the remainder of Data not in Train.Data
if(exists("Exclude") & length(Exclude) != 0){
  
  Test.Data <- Data[ !(Data$Id %in% Train.Data$Id)
                     , -which(names(Data) %in% Exclude)
                     ]
  
} else {
  
  Test.Data <- Data[ !(Data$Id %in% Train.Data$Id), ]
  
}



#* Remove any extra levels
Test.Data <- droplevels(Test.Data)



#* Create Score.Import from Test.Data to be compatible for ScoreImportImputation_Script
Score.Import <- Test.Data



#* Run ScoreImportImputation_Script
source("ScoreImportImputation_Script.R", echo = TRUE)