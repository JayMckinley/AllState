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


# Clear Workspace
rm(list = ls())

# Load Data Frame
train.df <- read.csv("train.csv")
test.df <- read.csv("test.csv")

train.df <- train.df[-1,]

train.df$lSplit <- cut(train.df$loss,c(0, 1204, 2116, 3864, 10000, 121000 ))

levels(train.df$lSplit)

train.df$lSplit <- revalue(train.df$lSplit, c("(0,1.2e+03]" = 'Very Low',"(1.2e+03,2.12e+03]" = 'Low',"(2.12e+03,3.86e+03]" = 'Med',
                                              "(3.86e+03,1e+04]" = 'High', "(1e+04,1.21e+05]" = 'Very High'))

# Very Low      0       - 1204  
# Low           1204    - 2116
# High          2116    - 3864
# High          3864    - 10000
# Very High     10000   - 121000 

table(train.df$lSplit)

train.df$logLoss <- log(train.df$loss)

# Look at the Response variable a bit
length(train.df[train.df$loss >= 25000, 132]) # Only 159 observations are >= 25000
length(train.df[train.df$loss >= 10000, 132])  # Only 5880 observations are >= 10000
length(train.df[train.df$loss >= 7500, 132])  # Only 12931 observations are >= 7500
length(train.df[train.df$loss >= 5000, 132])  # Only 30906 observations are >= 5000

qplot(train.df[train.df$loss < 10000,132], bins = 50)

# If you transform to with log
qplot(round(log(train.df[train.df$loss < 10000,132]),2), bins = 50)
qplot(round((train.df[train.df$loss < 10000,132])^(1/3),2), bins = 50)

## Perform the test

qqnorm(round(log(train.df[train.df$loss < 10000,132]),2)) 
qqline(round(log(train.df[train.df$loss < 10000,132]),2), col = 2)
qqnorm(round((train.df[train.df$loss < 10000,132])^(1/3),2))
qqline(round((train.df[train.df$loss < 10000,132])^(1/3),2), col = 2)

summary(train.df$loss)

qplot(train.df$cont11, bins = 50)

ggplot(train.df, aes(loss, cont11, color = lSplit)) + geom_point(alpha = .3)

ggplot(train.df, aes(loss, cont6, color = lSplit)) + geom_point(alpha = .3)

ggplot(train.df[train.df$lSplit == 'Very Low',], aes(cont7, cont13, color = lSplit)) +
        geom_point()

ggplot(train.df[train.df$lSplit == 'Low',], aes(cont7, cont13, color = lSplit)) +
        geom_point()

ggplot(train.df[train.df$lSplit == 'Med',], aes(cont7, cont13, color = lSplit)) +
        geom_point()

ggplot(train.df[train.df$lSplit == 'High',], aes(cont7, cont13, color = lSplit)) +
        geom_point()

ggplot(train.df[train.df$lSplit == 'Very High',], aes(cont7, cont13, color = lSplit)) +
        geom_point()

colClass <- function(df){
        class.list <- c()
        n <- 0
        for(i in df){
                class.list <- c(class.list, class(i))
        }
        return(class.list)
}

levelCntList <- function(df){
        max.levels <- 0
        tmp <- c()
        for(i in 1:ncol(df)){
                if(class(df[,i]) == 'factor'){
                      tmp <- c(tmp, length(levels(df[,i])))
                } else {
                        tmp <- c(tmp, 1)
                }
                
        }
        return(tmp)
}

tableData <- function(df){
        col.class <- colClass(df)
        col.names <- names(df)
        cl.L <- levelCntList(df)
        max.L <- max(cl.L)
        total.df <- data.frame(matrix(0, nrow = 1, ncol = max.L + 2))
        names(total.df) <-c('ColName', 'NumFactors', paste('Level', 1:(max.L), sep=''))
        for(i in 1:ncol(df)){
                if(col.class[i] == 'factor'){
                        temp <- data.frame(matrix(0, nrow = 1, ncol = max.L + 2))
                        names(temp) <-c('ColName', 'NumFactors', paste('Level', 1:(max.L), sep=''))
                        temp$ColName <- col.names[i]
                        temp$NumFactors <- cl.L[i]
                        temp[,3:(max.L+2)] <- c(as.numeric(table(df[,i])), rep(0, max.L - cl.L[i]))
                        total.df <- rbind(total.df, temp)
                }
        }
        return(total.df[-1,])
}

df.table <- tableData(train.df[,-1])

quartz()
num.of.factors <- 2
ggplot(data =melt(df.table[df.table$NumFactors <= num.of.factors,c(1,3:(3+num.of.factors))]), aes(variable, ColName, fill = value))+
        geom_tile(color = "white")+
        theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                         size = 3, hjust = 1),
              axis.text.y = element_text(size = 3))+
        coord_fixed()


# 112 Heatmaps to get an idea
for(i in 2:115){
        print(table(train.df[,i]))
}

# 112 Heatmaps to get an idea
for(i in 2:116){
        heatmap(table(train.df[,i], train.df[,133]))
}
i <- 118
# Histograms of Continuous Features
for(i in 118:131){
        p1 <- qplot(train.df[,i], xlab = paste('Cont', i-117), bins = 50)
        print(p1)
}

for(i in 117:132){
p1 <- ggplot(train.df, aes_string(x = 'loss', y=names(train.df)[i], color = 'lSplit')) + geom_point(alpha = .3)
print(p1)
}
corrplot::corrplot(cor(train.df[,c(118:132)]),method="circle", type="lower",  sig.level = 0.01, insig = "blank") 


train.num <- train.df[,-c(1,133)]
for(i in 1:116){
        train.num[,i] <- as.numeric(train.num[,i])
}

corr.df <- melt(cor(train.num))

quartz()
ggplot(data =melt(cor(train.num)), aes(Var2, Var1, fill = value))+
        geom_tile(color = "white")+
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1,1), space = "Lab", 
                             name="Pearson\nCorrelation") +
        theme_minimal()+ 
        theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                         size = 3, hjust = 1),
              axis.text.y = element_text(size = 3))+
        coord_fixed()

corr.df <- melt(cor(train.num))

loss.corr.df <- corr.df[corr.df$Var2 == 'loss' & corr.df$Var1 != 'loss',]

loss.corr.df.order <- loss.corr.df[order(loss.corr.df$value),]

qplot(data = loss.corr.df.order, x = Var1, y = value)

ggplot(data =loss.corr.df.order, aes(Var1, value))+
        geom_point(aes(color = value)) +
        theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                         size = 3, hjust = 1),
              axis.text.y = element_text(size = 3))+
        scale_color_gradient2(low = "red", high = "blue", mid = "light blue", 
                             midpoint = .06, limit = c(-.5,.5)) +
        labs(colour = "Correlation")

high.corr <- loss.corr.df.order[loss.corr.df.order$value < -.025 | loss.corr.df.order$value > .025,]

nrow(train.df[train.df$cat80 == 'D',])
summary(train.df[train.df$cat80 == 'D','loss'])
qplot(train.df[train.df$cat80 == 'D','loss'], bins = 50)
qplot(train.df[train.df$cat80 == 'B' & train.df$loss < 10000,'loss'], bins = 50)


nrow(train.df[train.df$cat80 == 'B',])
summary(train.df[train.df$cat80 == 'B', 'loss'])
qplot(train.df[train.df$cat80 == 'B','loss'], bins = 50)
qplot(train.df[train.df$cat80 == 'B' & train.df$loss < 10000,'loss'], bins = 50)


nrow(train.df[train.df$cat79 == 'B',])
summary(train.df[train.df$cat79 == 'B', 'loss'])
qplot(train.df[train.df$cat79 == 'B','loss'], bins = 50)
qplot(train.df[train.df$cat79 == 'B' & train.df$loss < 10000,'loss'], bins = 50)

length(train.df[train.df$cat80 == 'D' & train.df$cat79 == 'B', 'loss'])
qplot(train.df[train.df$cat80 == 'D' & train.df$cat79 == 'B' & train.df$loss < 10000, 'loss'], bins = 50)




p <- ggplot(train.df[train.df$loss < 15000,], aes(cat1, loss, fill = cat1)) +
        geom_boxplot()+
        ggtitle("CAT 1 Box Plot")

ggplotly(p)

catSummary <- lapply(train.df[,c(2:117)],function(x) as.data.frame(cbind(summary(x))))
catSummary2 <- lapply(train.df[,c(2:117)],function(x) as.data.frame(summary(x)))

################################# MODELING ####################################
names(getModelInfo())

##########################GBM Benchmark - loss  ###############################

train.sample <- createDataPartition(train.df$loss, p = .75, 
                                    list = FALSE, 
                                    times = 1)

var.names <- names(train.df)[-c(1, 133, 134, 132)]

train.md <- train.df[train.sample,c('loss', var.names)]
vald.md <- train.df[-train.sample, c('loss', var.names)]



trCtrl <- trainControl(method = 'cv', number = 5)

gbm.bench <- train(train.md[,-1], 
                   train.md[,1], 
                   method = 'gbm', 
                   metric = 'mae')

y_pred_bench <- predict(gbm.bench, vald.md[,-1])

mae(y_pred_bench, vald.md[,1])

y_pred_bench_test <- predict(gbm.bench, test.df[,var.names])

bench.final <- data.frame(id = test.df[,1], loss = y_pred_bench_test)

write.csv(bench.final, 'GBMbenchmark.csv', row.names = F) # MAE: 1247 Worse than RF Benchmark

###############################################################################

######################### GBM Benchmark - logloss  ############################

train.ll <- train.df[train.sample,c('logLoss', var.names)]
vald.ll <- train.df[-train.sample, c('logLoss', var.names)]

trCtrl <- trainControl(method = 'cv', number = 5)

gbm.bench.ll <- train(train.md[,-1], 
                      train.md[,1], 
                      method = 'gbm', 
                      metric = 'mae')

y_pred_bench.ll<- predict(gbm.bench.ll, vald.md[,-1])

mae(y_pred_bench.ll, vald.md[,1])

y_pred_bench_test <- predict(gbm.bench.ll, test.df[,var.names])

bench.final <- data.frame(id = test.df[,1], loss = y_pred_bench_test)

write.csv(bench.final, 'GBMbenchmark.csv', row.names = F) # MAE: 

###############################################################################

######################### CTree Benchmark - logloss  ############################


train.crt <- train.df[train.sample,c('logLoss', var.names)]
vald.crt <- train.df[-train.sample, c('logLoss', var.names)]

trCtrl <- trainControl(method = 'cv', number = 5)

crt.bench <- train(train.md[,-1], 
                       train.md[,1], 
                       #trControl = trCtrl, 
                       method = 'ctree', 
                       metric = 'mae')

y_pred_crt <- predict(crt.bench, vald.md[,-1])

mae(y_pred_crt, vald.md[,1]) # MAE: 

###############################################################################



factor_to_numeric <- function(df){
        n = 1
        for(i in df){
                if (class(i) == 'factor'){
                        df[,n] <- as.numeric(i)
                }
                n = n + 1
        }
        return(df)
}

temp <- factor_to_numeric(train.df[,-c(1,132,133)])

pca <- prcomp(temp, center = T, scale. = T)


tree.model <- train(loss~., train.df[c(1:1000),], 'rpart')
print(tree.model) 
quartz()
plot(tree.model$finalModel)
text(tree.model$finalModel)


rfGrid <-  expand.grid(mtry = 2:9)
forest.model <- train(loss~.,  train.df[c(1:1000),], 'rf', tuneGrid=rfGrid,
                      prox=TRUE,allowParallel=TRUE)


temp <- data.frame(forest.model$finalModel$importance)

######################### GBM Benchmark - lSplit  ############################

var.names <- names(train.df)[-c(1, 133, 134, 132)]


train.cat <- train.df[train.sample,c('lSplit', var.names)]
vald.cat <- train.df[-train.sample, c('lSplit', var.names)]

trCtrl <- trainControl(method = 'cv', number = 5)

gbm.cat <- train(train.cat[,-1], 
                 train.cat[,1], 
                 method = 'gbm', 
                 metric = 'mae')

y_pred_cat<- predict(gbm.cat, vald.cat[,-1])

mae(y_pred_cat, vald.cat[,1])

