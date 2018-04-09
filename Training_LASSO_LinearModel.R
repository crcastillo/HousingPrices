library(doParallel)


## Create model matrix and scale/standardize the factors in preparation for logistic model run
Train.x <- model.matrix(SalePrice ~ . 
                         
                         - Id
                         
                         , data = Train.Data)[ , -1]



#* Create a vector of the column names for comparison later
colnames.Train <- colnames(Train.x)



#* Memory clean up
gc()



#* Create target vector
Train.y <- Train.Data$SalePrice



#* Store system time for model run
Start <- Sys.time()
Start



## Run LASSO regularization logistic regression, alpha = 1
Train.lasso.glm <- glmnet(x = Train.x
                          , y = Train.y
                          , alpha = 1
                          , family = "gaussian"
)



#* Memory clean up
gc()



#* Determine number of cores on machine and initialise parallelisation
parallelCluster <- parallel::makeCluster(parallel::detectCores())
registerDoParallel(parallelCluster, cores = numCores)



#* Load libraries onto each core in the cluster
clusterCall(parallelCluster,function(x) .libPaths(x),.libPaths())
#* Check the libraries loaded on each cluster
clusterEvalQ(parallelCluster, library(doParallel))



#* Establish the randomness seed
set.seed(10)
#* Use Cross Validation to determine best possible model
Train.lasso.glm.cv <- cv.glmnet(x = Train.x
                                , y = Train.y
                                , alpha = 1
                                , family = "gaussian"
                                , type.measure = "mse"
                                , parallel = TRUE
)



#* Display model run time
End <- Sys.time()
End
End - Start



#* Close any open clusters after running parallel process
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
  parallelCluster <- c()
}



#* Record the lambda value at 1 standard error from best CV AUC as well as the CV AUC
Train.lasso.bestlam.1se <- Train.lasso.glm.cv$lambda.1se
Train.lasso.bestlam.1se.mse <- Train.lasso.glm.cv$cvm[match(Train.lasso.glm.cv$lambda.1se, Train.lasso.glm.cv$lambda)]



#* Display the non-zero coefficients from the "best" model
Train.lasso.glm.coeff <- predict(Train.lasso.glm, type = "coefficients", s = Train.lasso.glm.cv$lambda.1se)[1:(ncol(Train.x)+1), ]
Train.lasso.glm.coeff[Train.lasso.glm.coeff != 0]



#* Check cross validated AUC
Train.lasso.bestlam.1se.mse



#* Plot out the model diagnostics 
dev.new()
par(mfrow=c(2, 1))
plot(Train.lasso.glm
     , xvar = "lambda"
     , xaxt = "n"
     )
plot(Train.lasso.glm.cv)



#* CV MSE = 1587532084



#* Export non-zero coefficients in CSV file
#fwrite(
#  x = data.frame(Train.lasso.glm.coeff[Train.lasso.glm.coeff != 0])
#  , file = ?
#  , sep = ","
#  , row.names = TRUE
#  , col.names = TRUE
#)