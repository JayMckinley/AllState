library(caret)
library(FactoMineR) 

load("MattWS.RData")

###############################################################################
############## FAMD - Factor Analysis for Mixed Data - logloss ################

famd.fit <- FAMD(train.logloss[,-1],ncp = 20, graph = TRUE, sup.var = 131)

summary(famd.fit)
plot(famd.fit, habillage = 4, cex = 1.5)

###############################################################################
########################### Matt OLS - logloss  ###############################

models[[9]]  <- train(logLoss ~ cont2 + cat87 + cat80 + cat2 + cat9 + cat11 + cat79 + cat100,
                     train.logloss[c(1:10000),-c(132)], 
                     method = 'lm', 
                     metric = 'RMSE')

y_preds[[9]] <- predict(models[[9]], vald.logloss[,-c(1, 132)])

mae(y_preds[[9]], vald.logloss[,1]) 

(MAElist[[9]] <- mae(exp(y_preds[[9]]), vald.loss)) # MAE: 1447.41

save.image("MattWS.RData")
