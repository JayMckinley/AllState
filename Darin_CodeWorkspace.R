library(caret)

load("DarinWS.RData")

###############################################################################
########################### Darin OLS - logloss  ##############################

models[[11]]  <- train(logLoss ~ .,
                      train.logloss[c(1:10000),], 
                      method = 'lm', 
                      metric = 'RMSE')

y_preds[[11]] <- predict(models[[11]], vald.logloss[,-c(1)])

mae(y_preds[[11]], vald.logloss[,1]) 

(MAElist[[11]] <- mae(exp(y_preds[[11]]), vald.loss)) # MAE: 1325



save.image("DarinWS.RData")
