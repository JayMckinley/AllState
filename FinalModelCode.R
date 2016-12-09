#setwd("C:/Users/jay_m/Desktop/Dropbox/School/DSA5103-IDA/Project/AllState")

loadLibs <- function(){
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
     # Load H2O
     library(h2o)
     
     getLambdas <<- function(df){
          Lambdas<-data.frame(Optimal=double(14))
          rownames(Lambdas)<-colnames(df)[118:131]
          j=1
          for(i in c(118:131)){
               bc <- EnvStats::boxcox(df[c(1:10000),i]+.0001,optimize = T,lambda = c(-3,3))
               Lambdas[j,1]<-bc$lambda
               j=j+1
          }
          return(Lambdas)
     }     
     transformData <<- function(df, Lambdas, doLoss = T){
          train.trnfrm <- df
          j=1
          for(i in c(118:131)){
               train.trnfrm[,i]<-train.trnfrm[,i]^Lambdas[j,1]
               cat(paste(colnames(train.trnfrm)[i], '\t',rownames(Lambdas)[j], '\t',Lambdas[j,1], '\n'))
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
          
          train.trnfrm$cat89 <- as.character(train.trnfrm$cat89)
          train.trnfrm[!train.trnfrm$cat89 %in% c('A', 'B', 'C'), 'cat89'] <- 'DEFGHI'
          train.trnfrm$cat89 <- as.factor(train.trnfrm$cat89)
          
          train.trnfrm$cat90 <- as.character(train.trnfrm$cat90)
          train.trnfrm[!train.trnfrm$cat90 %in% c('A', 'B', 'C', 'D'), 'cat90'] <- 'EFG'
          train.trnfrm$cat90 <- as.factor(train.trnfrm$cat90)
          
          train.trnfrm$cat92 <- as.character(train.trnfrm$cat92)
          train.trnfrm[!train.trnfrm$cat92 %in% c('A', 'B', 'H'), 'cat92'] <- 'CDEFGI'
          train.trnfrm$cat92 <- as.factor(train.trnfrm$cat92)
          
          train.trnfrm$cat96 <- as.character(train.trnfrm$cat96)
          train.trnfrm[!train.trnfrm$cat96 %in% c('D', 'B', 'E', 'G', 'F'), 'cat96'] <- 'ACHI'
          train.trnfrm$cat96 <- as.factor(train.trnfrm$cat96)
          
          train.trnfrm$cat99 <- as.character(train.trnfrm$cat99)
          train.trnfrm[train.trnfrm$cat99 %in% c('G', 'O', 'U'), 'cat99'] <- 'GOU'
          train.trnfrm$cat99 <- as.factor(train.trnfrm$cat99)
          
          train.trnfrm$cat102 <- as.character(train.trnfrm$cat102)
          train.trnfrm[!train.trnfrm$cat102 %in% c('A', 'B', 'C', 'D', 'E'), 'cat102'] <- 'FGHJ'
          train.trnfrm$cat102 <- as.factor(train.trnfrm$cat102)
          
          train.trnfrm$cat103 <- as.character(train.trnfrm$cat103)
          train.trnfrm[train.trnfrm$cat103 %in% c('K', 'L', 'M', 'N'), 'cat103'] <- 'KLMN'
          train.trnfrm$cat103 <- as.factor(train.trnfrm$cat103)
          
          train.trnfrm$cat111 <- as.character(train.trnfrm$cat111)
          train.trnfrm[train.trnfrm$cat111 %in% c('B', 'D', 'F', 'L', 'B', 'U', 'W', 'Y'), 'cat111'] <- 'BDFLBUWY'
          train.trnfrm$cat111 <- as.factor(train.trnfrm$cat111)
          
          
          return(train.trnfrm)
     }
     transformData2 <<- function(df, Lambdas, doLoss = T){
          train.trnfrm <- df
          j=1
          for(i in c(118:131)){
               train.trnfrm[,i]<-train.trnfrm[,i]^Lambdas[j,1]
               cat(paste(colnames(train.trnfrm)[i], '\t',rownames(Lambdas)[j], '\t',Lambdas[j,1], '\n'))
               j<-j+1
          }
          if(doLoss){
               train.trnfrm[,'lossTrn']<-train.trnfrm[,132]^Lambdas[15,1]
          }
          
          train.trnfrm$cat89 <- as.character(train.trnfrm$cat89)
          train.trnfrm[!train.trnfrm$cat89 %in% c('A', 'B', 'C'), 'cat89'] <- 'DEFGHI'
          train.trnfrm$cat89 <- as.factor(train.trnfrm$cat89)
          
          train.trnfrm$cat90 <- as.character(train.trnfrm$cat90)
          train.trnfrm[!train.trnfrm$cat90 %in% c('A', 'B', 'C', 'D'), 'cat90'] <- 'EFG'
          train.trnfrm$cat90 <- as.factor(train.trnfrm$cat90)
          
          train.trnfrm$cat92 <- as.character(train.trnfrm$cat92)
          train.trnfrm[!train.trnfrm$cat92 %in% c('A', 'B', 'H'), 'cat92'] <- 'CDEFGI'
          train.trnfrm$cat92 <- as.factor(train.trnfrm$cat92)
          
          train.trnfrm$cat96 <- as.character(train.trnfrm$cat96)
          train.trnfrm[!train.trnfrm$cat96 %in% c('D', 'B', 'E', 'G', 'F'), 'cat96'] <- 'ACHI'
          train.trnfrm$cat96 <- as.factor(train.trnfrm$cat96)
          
          train.trnfrm$cat99 <- as.character(train.trnfrm$cat99)
          train.trnfrm[train.trnfrm$cat99 %in% c('G', 'O', 'U'), 'cat99'] <- 'GOU'
          train.trnfrm$cat99 <- as.factor(train.trnfrm$cat99)
          
          train.trnfrm$cat102 <- as.character(train.trnfrm$cat102)
          train.trnfrm[!train.trnfrm$cat102 %in% c('A', 'B', 'C', 'D', 'E'), 'cat102'] <- 'FGHJ'
          train.trnfrm$cat102 <- as.factor(train.trnfrm$cat102)
          
          train.trnfrm$cat103 <- as.character(train.trnfrm$cat103)
          train.trnfrm[train.trnfrm$cat103 %in% c('K', 'L', 'M', 'N'), 'cat103'] <- 'KLMN'
          train.trnfrm$cat103 <- as.factor(train.trnfrm$cat103)
          
          train.trnfrm$cat111 <- as.character(train.trnfrm$cat111)
          train.trnfrm[train.trnfrm$cat111 %in% c('B', 'D', 'F', 'L', 'B', 'U', 'W', 'Y'), 'cat111'] <- 'BDFLBUWY'
          train.trnfrm$cat111 <- as.factor(train.trnfrm$cat111)
          
          
          
          return(train.trnfrm)
     }
     
}
loadLibs()

loadData <- function(s = 0, pct = .75){
     s <<- s
     # Load Data Frame
     cat('Loading Train/Test Data \n')
     train.df <- read.csv("train.csv")
     test.df <- read.csv("test.csv")
     
     test.data <- test.df
     
     test.id <<- test.df[,1]
     
     test.df$loss <- NA
     
     trNrow <- nrow(train.df)
     
     temp <- rbind(train.df, test.df)
     lamb <- getLambdas(temp)
     temp <- transformData2(temp, lamb, doLoss = F)
     dmy <- dummyVars('~ .', temp[,-c(1,132)])
     
     test.df$loss <- NULL
     rm(temp)
     
     # Remove ID column we dont ever need it.
     train.df <- train.df[-1,] 
     # Since none of the loss == 0 we can use log instead of log1p
     train.df$logLoss <- log(train.df$loss + s)
     
     
     train.trnf <- transformData(train.df, lamb, doLoss = F)
     test.data <<- transformData(test.data, lamb, doLoss = F)
     train.trnf2 <- transformData2(train.df, lamb)
     test.trnf2 <- transformData2(test.df, lamb, doLoss = F)
     
     t.samp <- createDataPartition(train.trnf$loss, p = pct, 
                                   list = FALSE, 
                                   times = 1)
     
     var.names <- names(train.trnf)[-c(1, 132, 133, 134)]
     cat('Create Train/Vald set \n')
     train.logloss <<- train.trnf[t.samp,c('logLoss', var.names)]
     vald.logloss <<- train.trnf[-t.samp, c('logLoss', var.names)]
     vald.loss <<- train.trnf[-t.samp, c('loss')]
     
     
     
     cat('Creating Dummy Variables in train - May take some time. \n')
     train.dmy <- data.frame(predict(dmy, train.trnf2[,-c(1, 132, 133, 134)]))
     cat('Creating Dummy Variables in test - May take some time. \n')
     test.dmy <<- data.frame(predict(dmy, test.trnf2[, -1]))
     
     dmy.var.names <- names(train.dmy)
     
     dmy.train <- cbind(logLoss = train.trnf2$logLoss, train.dmy)
     
     cat('Creat dummy Train/Vald set \n')
     train.logloss.d <<- dmy.train[t.samp,]
     vald.logloss.d <<- dmy.train[-t.samp,]
     
     
     MAElist <<- c()
     y_preds <<- list()
     t_preds <<- list()
     test_preds <<- list()
     
     models <<- list()
     
     
     
}

loadData(s = 5, pct = .97)
save.image("modelWS.RData")

###############################################################################
################################# MODELING ####################################
###############################################################################

StatusLog <- list()

###############################################################################
############################ XGBoost - logloss  ###############################

DMtx.Train <- xgb.DMatrix(data = as.matrix(train.logloss.d[,-1]), label = as.matrix(train.logloss.d[,1]))


nr <- c()
for(i in c(4, 5, 6, 8, 10)){
     for(j in c(.01, .02, .03, .05, .075, .09)){
          tmp <- xgb.cv(params = list(
               objective="reg:linear",
               eval_metric = 'rmse',
               min_child_weight=4.2922,
               gamma = .629,
               subsample = 0.993,
               colsample_bytree = .3085,
               eta = j,
               max.depth = i),
               data = DMtx.Train,
               nfold = 5,
               nrounds = 2500,
               early.stop.round = 8,
               maximize = F,
               verbose = 1,
               print.every.n = 100)
          nr <- c(nr, which.min(tmp$test.rmse.mean))
     }   
}

n = 1
for(i in c(4, 5, 6, 8, 10)){
     for(j in c(.01, .02, .03, .05, .075, .09)){
          models[[n]] <- xgboost(DMtx.Train, 
                                 booster="gbtree", 
                                 objective="reg:linear", 
                                 eta=j, 
                                 max_depth = i,
                                 min_child_weight=4.2922,
                                 gamma = .629,
                                 subsample = 0.993,
                                 colsample_bytree = .3085,
                                 colsample_bylevel = 1,
                                 lambda = 1,
                                 alpha = 0,
                                 nrounds = nr[[n]],
                                 eval_metric = 'rmse',
                                 scale_pos_weight = 1,
                                 verbose = 1,
                                 print.every.n = 100)
          
          t_preds[[n]] <- predict(models[[n]], as.matrix(train.logloss.d[,-1]))
          y_preds[[n]] <- predict(models[[n]], as.matrix(vald.logloss.d[,-1]))
          test_preds[[n]]  <- predict(models[[n]], as.matrix(test.dmy))
          
          mae(y_preds[[n]], vald.logloss.d[,1]) 
          
          (MAElist[[n]] <- mae(exp(y_preds[[n]]) - s, vald.loss)) # MAE: 1136
          cat(i, j, MAElist[[n]], '\n')
          StatusLog[[n]] <- paste('XGB', i, j, MAElist[[n]])
          n = n + 1
     }
}



###############################################################################
############################ Neural Net 2 - logloss  ##########################

kd_h2o <- h2o.init(nthreads = -1, max_mem_size = "16g")

train_frame.hex <- as.h2o(train.logloss)
valid_frame.hex <- as.h2o(vald.logloss)
train_predict.hex <- as.h2o(train.logloss[,-1])
valid_predict.hex <- as.h2o(vald.logloss[,-1])
test_predict.hex<-as.h2o(test.data)

n = 31
for(i in c(80, 100, 120, 150, 200)){
     for(j in c(80, 100, 120, 150, 200)){
          for(k in c(0, 80, 100, 120, 150)){
               
               if(k == 0){
                    hid <- c(i, j)
               } else {
                    hid <- c(i, j, k) 
               }
               
               models[[n]] <- h2o.deeplearning(x = 2:(ncol(train_frame.hex)), 
                                               y = 1, 
                                               training_frame = train_frame.hex, 
                                               validation_frame = valid_frame.hex,
                                               epochs=20, 
                                               stopping_rounds=5,
                                               overwrite_with_best_model=T,
                                               activation="Rectifier",
                                               distribution="huber",
                                               hidden = hid)
               
               
               t_preds[[n]] <- as.matrix(predict(models[[n]], train_predict.hex))[,1]
               y_preds[[n]] <- as.matrix(predict(models[[n]], valid_predict.hex))[,1]
               test_preds[[n]] <- as.matrix(predict(models[[n]], test_predict.hex))[,1]
               
               
               mae(y_preds[[n]], vald.logloss[,1]) 
               
               (MAElist[[n]] <- mae(exp(y_preds[[n]]) - s, vald.loss))
               cat(i, j, k, MAElist[[n]], '\n')
               StatusLog[[n]] <- paste('NN', i, j, k, MAElist[[n]])
               n = n + 1
          }
     }
}


###############################################################################
############################### Predicting ####################################
###############################################################################

cutoff <- 1125
StatusLog[which(MAElist < cutoff)]
MAElist[which(MAElist < cutoff)]
temp <- c()
for(i in which(MAElist < cutoff)){
     temp <- cbind(temp, test_preds[[i]])
     
}

rsum <- exp(rowSums(temp)/ncol(temp)) - s

###############################################################################
############################### Predicting ####################################
###############################################################################


submission <- data.frame(id = test.id, loss = rsum)

write.csv(submission, paste('submissionAVG', Sys.Date(),  '.csv', sep = ''), row.names = F) 

