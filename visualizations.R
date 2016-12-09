
## Cor Plot of Continuous Featrues
train.num <-train.df[,c(118:131,132)]
for(i in 1:116){
        train.num[,i] <- as.numeric(train.num[,i])
}

ggplot(data =melt(cor(train.num)), aes(Var2, Var1, fill = value))+
        geom_tile(color = "white")+
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1,1), space = "Lab", 
                             name="Pearson\nCorrelation") +
        theme_minimal()+ 
        theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                         size = 3, hjust = 1),
              axis.text.y = element_text(size = 3))+
        coord_fixed() +
        xlab('Continuous Features')+
        ylab('Continuous Features') +
        labs(title = "Correlation Plot")

