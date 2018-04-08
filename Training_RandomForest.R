## Random Forest Model


model.rf = randomForest(SalePrice ~ . - Id, 
                        data = Train.Data,
                        ntree = 100,mtry = 5,importance = TRUE)

