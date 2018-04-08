###################### Commments on the script ######################
#********************************************************************
#****
#****   Store Train.Data Levels for Correction and Imputation
#****
#****   Objective: Design a script that can store the different
#****     categorical factor levels within the training dataset for
#****     various models (Train.Data). This can then be sourced by
#****     different training scripts and then utilized in the 
#****     scoring scripts for level correction and smart imputation.
#****
#****   4/3/2018 - Initial Build (Chris Castillo)
#****
#****
#****   Code Change Log:
#****   *USER NAME* - m/d/yyyy
#****     - 
#********************************************************************
#********************************************************************


#* Find the list of data frame names and their respective classes 
allClass <- function(x)
{unlist(lapply(unclass(x), class))}


#******************************************************************
#******************************************************************
##### Store levels of Train.Data factors for level correction #####
#******************************************************************
#******************************************************************


#* Create a list of the columns that are class = factor
allClass(Train.Data)
Class.List.Train <- which(allClass(Train.Data) == "factor")



#* Create name vectors for each factor that stores the different levels in Train.Data
for (i in 1:length(Class.List.Train)){
  
  assign(paste("Levels.Train.", names(Class.List.Train)[i], sep = ""), levels(eval(parse(text = paste("Train.Data$", names(Class.List.Train)[i], sep="")))))
  
}



#* Create a list of factor vectors
Train.Level.Objects <- ls()[grep('^Levels.Train.*?', ls())]



#* Create a vector that will store factors that contain _Unknown_
Unknown_Match <- NULL
#* If one of the factors contains _Unknown_ as a level, store the factor in Unknown_Match
for (i in 1:length(Class.List.Train))
{if(is.na(match('_Unknown_', get(ls()[grep('^Levels.Train.*?', ls())][i]))) == FALSE)
{Unknown_Match <- c(Unknown_Match, ls()[grep('^Levels.Train.*?', ls())][i])}
  
  print(paste(ls()[grep('^Levels.Train.*?', ls())][i],
              if(is.na(match('_Unknown_', get(ls()[grep('^Levels.Train.*?', ls())][i]))) == FALSE)
              {'Match'} else {'No Match'}
              , sep = " "
  )
  )
} #* Close loop



#* Store all factors that don't have _Unknown_
Missing_Unknown_Factors <- Train.Level.Objects[!Train.Level.Objects %in% Unknown_Match]



#* Create a vector that will store factors that contain _Other_
Other_Match <- NULL
#* If one of the factors contains _Other_ as a level, store the factor in Other_Match
for (i in 1:length(Class.List.Train))
{if(is.na(match('_Other_', get(ls()[grep('^Levels.Train.*?', ls())][i]))) == FALSE)
{Other_Match <- c(Other_Match, ls()[grep('^Levels.Train.*?', ls())][i])}
  
  print(paste(ls()[grep('^Levels.Train.*?', ls())][i],
              if(is.na(match('_Other_', get(ls()[grep('^Levels.Train.*?', ls())][i]))) == FALSE)
              {'Match'} else {'No Match'}
              , sep = " "
  )
  )
} #* Close loop



#* Store all factors that don't have _Other_
Missing_Other_Factors <- Train.Level.Objects[!Train.Level.Objects %in% Other_Match]