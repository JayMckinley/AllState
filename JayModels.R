library(ggplot2)
library(caret)
library(corrplot)       # Correlation Plot
library(plotly)
library(reshape2)
library(GGally)
library(dismo)          # GBM.Step
library(hydroGOF)
library(RColorBrewer)
library(gplots)
library(plyr)
library(data.table)
library(gridExtra)
library(ROCR)
library(pROC)
library(FactoMineR) 
library(robustbase)
library(car)
library(EnvStats)
library(xgboost)

# Clear Workspace
rm(list = ls())
load("modelws.RData")

getLambdas <- function(df){
        Lambdas<-data.frame(Optimal=double(15))
        rownames(Lambdas)<-colnames(df)[118:132]
        j=1
        for(i in c(118:132)){
                bc <- EnvStats::boxcox(df[c(1:10000),i]+.0001,optimize = T,lambda = c(-3,3))
                Lambdas[j,1]<-bc$lambda
                j=j+1
        }
        return(Lambdas)
}     

transformData <- function(df, Lambdas, doLoss = T){
        train.trnfrm <- df
        j=1
        for(i in c(118:131)){
                train.trnfrm[,i]<-train.trnfrm[,i]^Lambdas[j,1]
                print(paste(colnames(train.trnfrm)[i],rownames(Lambdas)[j],Lambdas[j,1]))
                j<-j+1
        }
        if(doLoss){
                train.trnfrm[,'lossTrn']<-train.trnfrm[,132]^Lambdas[15,1]
        }
        
        train.trnfrm$cat101<-as.integer(train.trnfrm$cat101)
        train.trnfrm$cat114<-as.integer(train.trnfrm$cat114)
        train.trnfrm$cat107<-as.integer(train.trnfrm$cat107)
        train.trnfrm$cat104<-as.integer(train.trnfrm$cat104)
        train.trnfrm$cat106<-as.integer(train.trnfrm$cat106)
        train.trnfrm$cat105<-as.integer(train.trnfrm$cat105)
        train.trnfrm$cat115<-as.integer(train.trnfrm$cat115)
        train.trnfrm$cat112<-as.integer(train.trnfrm$cat112)
        train.trnfrm$cat110<-as.integer(train.trnfrm$cat110)
        train.trnfrm$cat113<-as.integer(train.trnfrm$cat113)
        train.trnfrm$cat109<-as.integer(train.trnfrm$cat109)
        train.trnfrm$cat116<-as.integer(train.trnfrm$cat116)
        # Extra
        train.trnfrm$cat111<-as.integer(train.trnfrm$cat111)
        
        return(train.trnfrm)
}

transformData2 <- function(df, Lambdas, doLoss = T){
        train.trnfrm <- df
        j=1
        for(i in c(118:131)){
                train.trnfrm[,i]<-train.trnfrm[,i]^Lambdas[j,1]
                print(paste(colnames(train.trnfrm)[i],rownames(Lambdas)[j],Lambdas[j,1]))
                j<-j+1
        }
        if(doLoss){
                train.trnfrm[,'lossTrn']<-train.trnfrm[,132]^Lambdas[15,1]
        }
        
        return(train.trnfrm)
}

checkTable <- function(){
for(i in c(89, 90, 92, 96, 99, 102, 103, 111)){
        cat('-------', i, '-----------\n')
        print(table(train.trnf[,i+1]))
        print(table(test.data[,i+1]))
        cat('-------\n\n')
}
}

# Load Data Frame
train.df <- read.csv("train.csv")
test.df <- read.csv("test.csv")

train.df <- train.df[-1,]

train.df$lSplit <- cut(train.df$loss,c(0, 1204, 2116, 3864, 10000, 121000 ))
train.df$lSplit2 <- cut(train.df$loss,c(0,10000, 121000 ))

levels(train.df$lSplit)
train.df$lSplit <- revalue(train.df$lSplit, c("(0,1.2e+03]" = 'Very Low',"(1.2e+03,2.12e+03]" = 'Low',"(2.12e+03,3.86e+03]" = 'Med',
                                              "(3.86e+03,1e+04]" = 'High', "(1e+04,1.21e+05]" = 'Very High'))
table(train.df$lSplit)

levels(train.df$lSplit2)
train.df$lSplit2 <- revalue(train.df$lSplit2, c("(0,1e+04]" = 'Regular',"(1e+04,1.21e+05]" = 'High'))
table(train.df$lSplit2)

train.df$logLoss <- log(train.df$loss) # Since none of the loss == 0 we can use log instead of log1p



###############################################################################
######################### GBM Benchmark - Split2  ############################

gbm.highvalue <- train(train.df.trnf[c(1:10000),-c(1, 132, 133, 134, 135, 136)], 
                    train.df.trnf[c(1:10000),135], 
                    method = 'gbm')

prob.highvalue <- predict(gbm.highvalue, train.df.trnf[,-c(1, 132, 133, 134, 135, 136)], type = 'prob')

train.df.trnf$pHigh <- prob.highvalue$High


###############################################################################
################################# MODELING ####################################
###############################################################################

names(getModelInfo())

train.sample <- createDataPartition(train.df.trnf$loss, p = .75, 
                                    list = FALSE, 
                                    times = 1)

var.names <- names(train.df.trnf)[-c(1, 132, 133, 134, 135, 136)]

train.loss <- train.df.trnf[train.sample,c('loss', var.names)]
vald.loss <- train.df.trnf[-train.sample, c('loss', var.names)]
train.logloss <- train.df.trnf[train.sample,c('logLoss', var.names)]
vald.logloss <- train.df.trnf[-train.sample, c('logLoss', var.names)]
train.lSplit <- train.df.trnf[train.sample,c('lSplit', var.names)]
vald.lSplit <- train.df.trnf[-train.sample, c('lSplit', var.names)]
train.lSplit2 <- train.df.trnf[train.sample,c('lSplit2', var.names)]
vald.lSplit2 <- train.df.trnf[-train.sample, c('lSplit2', var.names)]
train.lossTrn <- train.df.trnf[train.sample,c('lossTrn', var.names)]
vald.lossTrn <- train.df.trnf[-train.sample, c('lossTrn', var.names)]

dmy <- dummyVars('~ .', train.df.trnf2[,-c(1, 132, 133, 134, 135, 136)])
train.df.trnf.dmy <- data.frame(predict(dmy, train.df.trnf2[,-c(1, 132, 133, 134, 135, 136)]))

dmy.var.names <- names(train.df.trnf.dmy)

dmy.train <- cbind(logLoss = train.df.trnf2$logLoss, train.df.trnf.dmy)

dmy.train.logloss <- dmy.train[train.sample,]
dmy.vald.logloss <- dmy.train[-train.sample,]

trCtrl <- trainControl(method = 'cv', number = 5)

MAElist <- c()
y_pred <- list()

model <- list()


# Can get 1816 by always assigning 2100 to all losses
mae(rep(2100, length(vald.loss)), vald.loss)

knn(train.logloss)

###############################################################################
########################## GBM Benchmark - logloss  ###########################

model[[1]] <- train(train.logloss[c(1:5000),-c(1,132)], 
                     train.logloss[c(1:5000),1], 
                     method = 'gbm', 
                     metric = 'RMSE')

y_pred[[1]] <- predict(model[[1]], vald.logloss[,-c(1, 132)])

mae(y_pred[[1]], vald.logloss[,1]) # .485, .443

(MAElist[[1]] <- mae(exp(y_pred[[1]]), vald.loss[,1])) # MAE 1347, 1223



###############################################################################
########################## GBM Benchmark - logloss  ###########################

model[[2]] <- train(train.logloss[c(1:5000),-1], 
                     train.logloss[c(1:5000),1], 
                     method = 'gbm', 
                     metric = 'RMSE')

y_pred[[2]]  <- predict(model[[2]], vald.logloss[,-1])

mae(y_pred[[2]] , vald.logloss[,1]) # .485, .449

(MAElist[[2]] <- mae(exp(y_pred[[2]]), vald.loss[,1])) # 1339.26, 1243

###############################################################################
########################## GBM Benchmark - logloss  ###########################

model[[3]] <- train(train.logloss[c(1:10000),-c(1,132)], 
                     train.logloss[c(1:10000),1], 
                     method = 'gbm', 
                     metric = 'RMSE')

y_pred[[3]]  <- predict(model[[3]], vald.logloss[,-c(1,132)])

mae(y_pred[[3]] , vald.logloss[,1]) # .485

(MAElist[[3]] <- mae(exp(y_pred[[3]]), vald.loss[,1])) # 1304.49, 1230, 1214

###############################################################################
########################## GBM Benchmark - logloss  ###########################

model[[4]] <- train(train.logloss[c(1:20000),-c(1,132)], 
                     train.logloss[c(1:20000),1], 
                     method = 'gbm', 
                     metric = 'RMSE')

y_pred[[4]] <- predict(model[[4]], vald.logloss[,-c(1,132)])

mae(y_pred[[4]], vald.logloss[,1]) 

(MAElist[[4]] <-mae(exp(y_pred[[4]]), vald.loss[,1])) # 1262.34

###############################################################################
########################## GBM Benchmark - logloss  ###########################

model[[5]] <- train(train.logloss[c(1:50000),-c(1,132)], 
                     train.logloss[c(1:50000),1], 
                     method = 'gbm', 
                     metric = 'RMSE')

y_pred[[5]] <- predict(model[[5]], vald.logloss[,-c(1,132)])

mae(y_pred[[5]], vald.logloss[,1]) 

(MAElist[[5]] <- mae(exp(y_pred[[5]]), vald.loss[,1])) # 1237.728

###############################################################################
########################## GBM Benchmark - logloss  ###########################

model[[6]] <- train(train.logloss[,-1], 
                     train.logloss[,1], 
                     method = 'gbm', 
                     metric = 'RMSE')

y_pred[[6]] <- predict(model[[6]], vald.logloss[,-1])

mae(y_pred[[6]], vald.logloss[,1]) 

(MAElist[[6]] <- mae(exp(y_pred[[6]]), vald.loss[,1])) # 1222

###############################################################################
########################## GBM Benchmark - logloss  ###########################

model[[7]] <- train(train.logloss[c(1:20000),c(118:131)], 
                     train.logloss[c(1:20000),1], 
                     method = 'gbm', 
                     metric = 'RMSE')

y_pred[[7]] <- predict(model[[7]], vald.logloss[,c(118:131)])

mae(y_pred[[7]], vald.logloss[,1]) 

(MAElist[[7]] <- mae(exp(y_pred[[7]]), vald.loss[,1])) # 1786, 1775

###############################################################################
######################### GBM Benchmark - logloss  ############################

model[[8]] <- train(train.logloss[c(1:10000),c(118:132)], 
                     train.logloss[c(1:10000),1], 
                     method = 'gbm', 
                     metric = 'RMSE')

y_pred[[8]] <- predict(model[[8]], vald.logloss[,c(118:132)])

mae(y_pred[[8]], vald.logloss[,1]) 

(MAElist[[8]] <- mae(exp(y_pred[[8]]), vald.loss[,1])) # 1513, 1384

###############################################################################
############## FAMD - Factor Analysis for Mixed Data - logloss ################

famd.fit <- FAMD(train.logloss[,-1],ncp = 20, graph = TRUE, sup.var = 131)

summary(famd.fit)
plot(famd.fit, habillage = 4, cex = 1.5)

###############################################################################
########################### Matt OLS - logloss  ###############################

ols.var.names <- c('cont2', 'cat87', 'cat80', 'cat2', 'cat9', 'cat11', 'cat79', 'cat100')

model[[9]]  <- train(logLoss ~ cont2 + cat87 + cat80 + cat2 + cat9 + cat11 + cat79 + cat100,
                      train.logloss[c(1:10000),-c(132)], 
                      method = 'lm', 
                      metric = 'RMSE')

y_pred[[9]] <- predict(model[[9]], vald.logloss[,-c(1, 132)])

mae(y_pred[[9]], vald.logloss[,1]) 

(MAElist[[9]] <- mae(exp(y_pred[[9]]), vald.loss[,1])) # 1451, 1436

###############################################################################
########################### Darin OLS - lossTrn  ##############################

model[[10]]  <- train(lossTrn ~ .,
                      train.lossTrn[c(1:10000),-c(132)], 
                      method = 'lm', 
                      metric = 'RMSE')

y_pred[[10]] <- predict(model[[10]], vald.lossTrn[,-c(1, 132)])

mae(y_pred[[10]], vald.lossTrn[,1]) 

(MAElist[[10]] <- mae(y_pred[[10]]^(1/lamb[15,1]), vald.loss[,1])) # 1360, 1304.5

###############################################################################
########################### Darin OLS - logloss  ##############################

model[[11]]  <- train(logLoss ~ .,
                      train.logloss[c(1:10000),-c(132)], 
                      method = 'lm', 
                      metric = 'RMSE')

y_pred[[11]] <- predict(model[[11]], vald.logloss[,-c(1, 132)])

mae(y_pred[[11]], vald.logloss[,1]) 

(MAElist[[11]] <- mae(exp(y_pred[[11]]), vald.loss[,1])) # 1296



###############################################################################
############################ XGBoost - logloss  ###############################

models[[12]] <- xgboost(data=as.matrix(dmy.train.logloss[,-1]), 
                       label=as.matrix(dmy.train.logloss[, 1]), 
                       booster="gbtree", 
                       objective="reg:linear", 
                       eta=0.3, 
                       max_depth=5,
                       min_child_weight=1,
                       gamma = 0,
                       subsample = 0.75,
                       colsample_bytree = 1,
                       colsample_bylevel = 1,
                       lambda = 1,
                       alpha = 0,
                       nrounds = 70,
                       scale_pos_weight = 1)

y_pred[[12]] <- predict(model[[12]], as.matrix(dmy.vald.logloss[,-1]))

mae(y_pred[[12]], dmy.vald.logloss[,1]) 

(MAElist[[12]] <- mae(exp(y_pred[[12]]), vald.loss[,1])) #

xgb.importance(names(train.logloss.d), model = models[[12]])

###############################################################################
############################ XGBoost - logloss  ###############################

tmp <- colnames(train.logloss[2:117])
train.logloss.num <- train.logloss
vald.logloss.num <- vald.logloss
for (i in tmp)
{
        train.logloss.num[[i]] <- as.numeric(as.factor(train.logloss.num[[i]]))
        vald.logloss.num[[i]] <- as.numeric(as.factor(vald.logloss.num[[i]]))
}

models[[13]] <- xgboost(data=as.matrix(train.logloss.num[,-1]), 
                       label=as.matrix(train.logloss.num[, 1]), 
                       booster="gbtree", 
                       objective="reg:linear", 
                       eta=0.3, 
                       max_depth=5,
                       min_child_weight=1,
                       gamma = 0,
                       subsample = 0.75,
                       colsample_bytree = 1,
                       colsample_bylevel = 1,
                       lambda = 1,
                       alpha = 0,
                       nrounds = 70,
                       scale_pos_weight = 1)

y_preds[[13]] <- predict(models[[13]], as.matrix(vald.logloss.num[,-1]))

mae(y_preds[[13]], vald.logloss.num[,1]) 

(MAElist[[13]] <- mae(exp(y_preds[[13]]), vald.loss[,1])) # MAE: 1189

tmp <- xgb.importance(names(vald.logloss.num), model = models[[13]])

###############################################################################
########################## Elastic Net - logloss  ###########################



###############################################################################
############################ Neural Net - logloss  ############################

load("modelWS.RData")

library(neuralnet)

n <- names(train.logloss.d)
f <- as.formula(paste("logLoss ~", paste(n[!n %in% "logLoss"], collapse = " + ")))
models[[14]] <- neuralnet(f, 
                          data = train.logloss.d[1:1000,], 
                          hidden = c(120, 100), 
                          linear.output = T, 
                          err.fct = 'sse',
                          act.fct = 'logistic')


###############################################################################
############################ Neural Net - logloss  ############################

# Load H2O
library(h2o)
kd_h2o <- h2o.init(nthreads = -1, max_mem_size = "16g")

train_frame.hex <- as.h2o(train.logloss)
valid_frame.hex <- as.h2o(vald.logloss)
valid_predict.hex <- as.h2o(vald.logloss[,-1])
test.hex<-as.h2o(test)

dnn <- h2o.deeplearning(x = 2:(ncol(train_frame.hex)), 
                              y = 1, 
                              training_frame = train_frame.hex, 
                              validation_frame = valid_frame.hex,
                              epochs=20, 
                              stopping_rounds=5,
                              overwrite_with_best_model=T,
                              activation="Rectifier",
                              distribution="huber",
                              hidden=c(100,100))


pred <- as.matrix(predict(dnn, valid_predict.hex))

(MAElist[[13]] <- mae(exp(pred[,1]) - 500, vald.loss)) # MAE: 1189



###############################################################################
#################### Neural Net with Noise - logloss  #########################


###############################################################################
########################### ADABoost - logloss  ###############################



###############################################################################
############################### Predicting ####################################
###############################################################################

test.trnf <- transformData(test.df, lamb, doLoss = F)

test_pred  <- predict(model[[12]], test.trnf[,-c(1)])

submission <- data.frame(id = test.df$id, loss = exp(test_pred))

write.csv(submission, paste('submission', Sys.Date(),  '.csv', sep = ''), row.names = F) 




n <- 1
tmp <- c()
tmpnum <- c()
for(i in 1800:2500){
        tmp[[n]] <- mae(rep(log(i), length(train.logloss$logLoss)), train.logloss$logLoss)
        tmpnum[[n]] <- i
        n = n + 1
}







